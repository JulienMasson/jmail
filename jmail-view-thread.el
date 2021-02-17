;;; jmail-view-thread.el --- XXXX

;; Copyright (C) 2021 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jmail
;; Created: 2021-02-16

;;; License

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'jmail-view)

;;; Mode

(defvar jmail-view-thread-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'jmail-view-thread-next)
    (define-key map "p" 'jmail-view-thread-previous)
    (define-key map "N" 'jmail-view-thread-search-next)
    (define-key map "P" 'jmail-view-thread-search-previous)
    (define-key map (kbd "TAB") 'jmail-view-thread-fold-unfold-mail)
    (define-key map [C-tab] 'jmail-view-thread-fold-unfold-all-mails)
    map)
  "Keymap for `jmail-view-thread-mode'")

(define-derived-mode jmail-view-thread-mode jmail-view-mode "jmail view thread")

;;; Customization

(defcustom jmail-view-thread-default-view nil
  "If non nil, we display all mails from a thread in *jmail-view* buffer"
  :type 'boolean
  :group 'jmail)

;;; Faces

(defface jmail-view-thread-separator-face
  '((t :foreground "DimGray" :strike-through t :extend t))
  "Face used for separator in jmail view thread"
  :group 'jmail)

;;; Internal Variables

(defvar-local jmail-view-thread--fold-overlays nil)

;;; Internal Functions

(defun jmail-view-thread--insert-separator ()
  (let ((beg (point)))
    (insert "\n")
    (overlay-put (make-overlay beg (point)) 'face 'jmail-view-thread-separator-face)))

(defun jmail-view-thread--insert-mail (target-path paths data)
  (with-jmail-view-buffer
   (let (start header end)
     (save-excursion
       (goto-char (point-max))
       (setq start (point))
       (unless (= (point-min) (point-max))
	 (jmail-view-thread--insert-separator))
       (setq header (point))
       (jmail-view--insert-contents data)
       (setq end (- (point-max) 1))
       (add-text-properties start (point-max) (list :jmail-view-data data
						    :jmail-view-start start
						    :jmail-view-header header
						    :jmail-view-end end))
       (jmail-view--fontify-mail header)
       (set-buffer-modified-p nil)
       (jmail-view-thread--insert-mails target-path paths))
     (if (string= target-path (plist-get data :path))
	 (progn
	   (goto-char header)
	   (jmail-view-thread--mark-as-read))
       (jmail-view-thread--add-fold-overlay data start end)))))

(defun jmail-view-thread--insert-mails (target-path paths)
  (when-let ((current-path (pop paths))
	     (handler (apply-partially #'jmail-view-thread--insert-mail
				       target-path paths)))
    (jmail-view--get-mail-data current-path handler)))

(defun jmail-view-thread--add-fold-overlay (data start end)
  (let* ((from (jmail-search--from data))
	 (date (jmail-search--date data))
	 (header (format (jmail-search--fmt) date from ""))
	 (subject (jmail-search--subject data))
	 (flags (plist-get data :flags))
	 (overlay (make-overlay start end)))
    (when (and jmail-search-bold-unread-message (member 'unread flags))
      (setq subject (propertize subject 'face 'bold)))
    (add-to-list 'jmail-view-thread--fold-overlays overlay)
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'before-string (concat header " " subject))))

(defun jmail-view-thread--find-fold-overlay (start end)
  (cl-find-if (lambda (ov)
		(and (<= (overlay-start ov) start)
		     (>= (overlay-end ov) end)))
	      jmail-view-thread--fold-overlays))

(defun jmail-view-thread--remove-fold-overlay (overlay)
  (setq jmail-view-thread--fold-overlays (remove overlay jmail-view-thread--fold-overlays))
  (delete-overlay overlay))

(defun jmail-view-thread--fold-current-mail ()
  (when-let* ((props (text-properties-at (point)))
	      (data (plist-get props :jmail-view-data))
	      (start (plist-get props :jmail-view-start))
	      (end (plist-get props :jmail-view-end)))
    (jmail-view-thread--add-fold-overlay data start end)))

(defun jmail-view-thread--setup-buffer (buffer)
  (with-current-buffer (get-buffer-create jmail-view--buffer-name)
    (jmail-view-thread-mode))
  (select-window (jmail-split-window-below buffer))
  (switch-to-buffer jmail-view--buffer-name))

(defun jmail-view-thread--clean-up ()
  (with-jmail-view-buffer
   (delete-all-overlays)
   (setq jmail-view-thread--fold-overlays nil)
   (erase-buffer)))

(defun jmail-view-thread--mark-as-read ()
  (let* ((props (text-properties-at (point)))
	 (data (plist-get props :jmail-view-data))
	 (path (plist-get data :path))
	 (flags (plist-get data :flags))
	 (start (plist-get props :jmail-view-start))
	 (end (plist-get props :jmail-view-end))
	 new-path new-flags)
    (when (cl-intersection flags (list 'new 'unread))
      (with-jmail-search-buffer
       (save-excursion
	 (when-let ((pos (jmail-search-find-path path)))
	   (goto-char pos)
	   (jmail-search--mark-as-read)
	   (when-let ((object (text-properties-at (point))))
	     (setq new-path (plist-get object :path))
	     (setq new-flags (plist-get object :flags))))))
      (when (and new-path new-flags)
	(plist-put data :path new-path)
	(plist-put data :flags new-flags)
	(put-text-property start end :jmail-view-data data)))))

(defun jmail-view-thread--mark-all-as-read ()
  (save-excursion
    (goto-char (- (point-max) 1))
    (while (not (bobp))
      (jmail-view-thread--mark-as-read)
      (jmail-view-thread-previous))
    (jmail-view-thread--mark-as-read)))

;;; External Functions

(defun jmail-view-thread-next ()
  (interactive)
  (when-let* ((props (text-properties-at (point)))
	      (end (plist-get props :jmail-view-end)))
    (goto-char end)
    (unless (= (+ (point) 1) (point-max))
      (forward-line)
      (when-let* ((props (text-properties-at (point)))
		  (header (plist-get props :jmail-view-header)))
	(goto-char header)))))

(defun jmail-view-thread-previous ()
  (interactive)
  (when-let* ((props (text-properties-at (point)))
	      (start (plist-get props :jmail-view-start)))
    (goto-char start)
    (forward-line -1)
    (when-let* ((props (text-properties-at (point)))
		(header (plist-get props :jmail-view-header)))
      (goto-char header))))

(defun jmail-view-thread-search-next ()
  (interactive)
  (jmail-view-thread--clean-up)
  (with-jmail-search-buffer
   (jmail-search-next-thread)
   (jmail-search-enter)))

(defun jmail-view-thread-search-previous ()
  (interactive)
  (jmail-view-thread--clean-up)
  (with-jmail-search-buffer
   (jmail-search-previous-thread)
   (jmail-search-enter)))

(defun jmail-view-thread-fold-unfold-mail ()
  (interactive)
  (if-let* ((overlay (jmail-view-thread--find-fold-overlay (line-beginning-position)
							   (line-end-position)))
	    (props (text-properties-at (point)))
	    (header (plist-get props :jmail-view-header)))
      (progn
	(jmail-view-thread--remove-fold-overlay overlay)
	(goto-char header)
	(jmail-view-thread--mark-as-read))
    (jmail-view-thread--fold-current-mail)))

(defun jmail-view-thread-fold-unfold-all-mails ()
  (interactive)
  (if jmail-view-thread--fold-overlays
      (progn
	(mapc #'jmail-view-thread--remove-fold-overlay jmail-view-thread--fold-overlays)
	(jmail-view-thread--mark-all-as-read))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
	(jmail-view-thread--fold-current-mail)
	(line-move 1)))))

(defun jmail-view-thread (current-path paths buffer)
  (if (get-buffer jmail-view--buffer-name)
      (pop-to-buffer jmail-view--buffer-name)
    (jmail-view-thread--setup-buffer buffer))
  (jmail-view-thread--clean-up)
  (jmail-view-thread--insert-mails current-path paths))

(provide 'jmail-view-thread)
