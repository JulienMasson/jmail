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
    (define-key map "t" 'jmail-view-thread-toggle-html)
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

;;; Internal Functions

(defun jmail-view-thread--insert-separator ()
  (let ((beg (point))
	overlay)
    (insert "\n")
    (setq overlay (make-overlay beg (point)))
    (overlay-put overlay 'jmail-view-separator t)
    (overlay-put overlay 'face 'jmail-view-thread-separator-face)))

(defun jmail-view-thread--find-separator (point)
  (cl-find-if (lambda (ov)
		(when-let ((props (overlay-properties ov)))
		  (plist-get props 'jmail-view-separator)))
	      (overlays-in point (+ point 1))))

(defun jmail-view-thread--disable-separator (point)
  (when-let ((ov (jmail-view-thread--find-separator point)))
    (overlay-put ov 'face nil)))

(defun jmail-view-thread--enable-separator (point)
  (when-let ((ov (jmail-view-thread--find-separator point)))
    (overlay-put ov 'face 'jmail-view-thread-separator-face)))

(defun jmail-view-thread--update-props (from offset)
  (save-excursion
    (goto-char from)
    (while (not (eobp))
      (let ((props (text-properties-at (line-end-position))))
	(unless (plist-get props :unload)
	  (when-let ((data (plist-get props :jmail-view-data))
		     (start (plist-get props :jmail-view-start))
		     (new-start (+ start offset))
		     (header (plist-get props :jmail-view-header))
		     (new-header (+ header offset))
		     (end (plist-get props :jmail-view-end))
		     (new-end (+ end offset)))
	    (put-text-property new-start new-end :jmail-view-data data)
	    (put-text-property new-start new-end :jmail-view-start new-start)
	    (put-text-property new-start new-end :jmail-view-header new-header)
	    (put-text-property new-start new-end :jmail-view-end new-end)
	    (goto-char new-end)))
	(forward-line)))))

(defun jmail-view-thread--load-mail (start thread after-load-cb data)
  (with-jmail-view-buffer
   (let (header end)
     (save-excursion
       (goto-char start)
       (unless (= start (point-min))
	 (jmail-view-thread--insert-separator))
       (setq header (point))
       (setq data (append data (list :thread thread)))
       (jmail-view--insert-contents data)
       (unless jmail-view--html-view
	 (jmail-view--clean-body))
       (setq end (point))
       (add-text-properties start end (list :jmail-view-data data
					    :jmail-view-start start
					    :jmail-view-header header
					    :jmail-view-end end))
       (jmail-view--fontify-mail header end)
       (set-buffer-modified-p nil)
       (unless (= (point) (point-max))
	 (jmail-view-thread--update-props (+ end 1) (- end start)))
       (when after-load-cb (funcall after-load-cb)))
     (goto-char header)
     (jmail-view-thread--mark-as-read))))


(defun jmail-view-thread--after-load-target-cb (objects)
  (when objects
    (insert "\n")
    (mapc #'jmail-view-thread--insert-unload objects)))

(defun jmail-view-thread--load-target (target objects)
  (let* ((thread (plist-get target :thread))
	 (after-load-cb (apply-partially #'jmail-view-thread--after-load-target-cb
					 objects))
	 (handler (apply-partially #'jmail-view-thread--load-mail
				   (point-max) thread after-load-cb))
	 (path (plist-get target :path)))
    (jmail-view--get-mail-data path handler)))

(defun jmail-view-thread--insert-unload (object)
  (with-jmail-view-buffer
   (let ((data (append object (list :unload t))))
     (insert "▶ ")
     (jmail-view-thread--add-fold-overlay data (point-max) (point-max))
     (insert "\n"))))

(defun jmail-view-thread--insert-mails (target objects)
  (catch 'target-found
    (let ((object (pop objects)))
      (while object
	(if (eq target object)
	    (progn
	      (jmail-view-thread--load-target target objects)
	      (throw 'target-found nil))
	  (jmail-view-thread--insert-unload object))
	(setq object (pop objects))))))

(defun jmail-view-thread--add-fold-overlay (data start end)
  (let* ((from (jmail-search--from data))
	 (date (jmail-search--date data))
	 (thread (jmail-search--thread data))
	 (header (format (jmail-search--fmt) date from (if thread thread "")))
	 (subject (jmail-search--subject data))
	 (flags (plist-get data :flags))
	 (overlay (make-overlay start end)))
    (when (and jmail-search-bold-unread-message (member 'unread flags))
      (setq subject (propertize subject 'face 'bold)))
    (overlay-put overlay 'jmail-view-data data)
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'after-string (concat header " " subject))))

(defun jmail-view-thread--find-fold-overlay (start end)
  (cl-find-if (lambda (ov)
		(when-let ((props (overlay-properties ov)))
		  (and (<= (overlay-start ov) start)
		       (>= (overlay-end ov) end)
		       (plist-get props 'jmail-view-data))))
	      (overlays-in (point-min) (point-max))))

(defun jmail-view-thread--fold-overlays-p ()
  (cl-find-if (lambda (ov)
		(when-let ((props (overlay-properties ov)))
		  (plist-get props 'jmail-view-data)))
	      (overlays-in (point-min) (point-max))))

(defun jmail-view-thread--fold-current-mail ()
  (when-let* ((props (text-properties-at (line-end-position)))
	      (data (plist-get props :jmail-view-data))
	      (start (plist-get props :jmail-view-start))
	      (end (plist-get props :jmail-view-end)))
    (goto-char start)
    (jmail-view-thread--disable-separator start)
    (insert "▶ ")
    (jmail-view-thread--update-props start 2)
    (jmail-view-thread--add-fold-overlay data (+ start 2) (+ end 2))
    (goto-char (line-beginning-position))))

(defun jmail-view-thread--unfold-all-mails (start)
  (with-jmail-view-buffer
   (save-excursion
     (goto-char start)
     (catch 'exit-loop
       (while (not (eobp))
	 (when-let* ((overlay (jmail-view-thread--find-fold-overlay (line-end-position)
								    (line-end-position)))
		     (start (line-beginning-position))
		     (ov-end (overlay-end overlay))
		     (props (overlay-properties overlay))
		     (data (plist-get props 'jmail-view-data)))
	   (delete-overlay overlay)
	   (delete-region start (+ start 2))
	   (jmail-view-thread--update-props start -2)
	   (if (plist-get data :unload)
	     (let* ((path (plist-get data :path))
		    (thread (plist-get data :thread))
		    (after-load-cb (apply-partially #'jmail-view-thread--unfold-all-mails
						    start))
		    (handler (apply-partially #'jmail-view-thread--load-mail
					      start thread after-load-cb)))
	       (jmail-view--get-mail-data path handler)
	       (throw 'exit-loop nil))
	     (jmail-view-thread--enable-separator start))
	   (goto-char (- ov-end 2)))
	 (forward-line))))
   (goto-char (point-min))))

(defun jmail-view-thread--setup-buffer (buffer)
  (with-current-buffer (get-buffer-create jmail-view--buffer-name)
    (jmail-view-thread-mode))
  (select-window (jmail-split-window-below buffer))
  (switch-to-buffer jmail-view--buffer-name))

(defun jmail-view-thread--clean-up ()
  (with-jmail-view-buffer
   (delete-all-overlays)
   (erase-buffer)
   (setq jmail-view--html-view jmail-view-html-default-view)))

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
	   (jmail-search--update-fold-overlay)
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
  (if (jmail-view-thread--find-fold-overlay (line-end-position)
					    (line-end-position))
      (next-line)
    (when-let* ((props (text-properties-at (point)))
		(end (plist-get props :jmail-view-end)))
      (goto-char end)
      (unless (= (+ (point) 1) (point-max))
	(forward-line)
	(when-let* ((props (text-properties-at (point)))
		    (header (plist-get props :jmail-view-header)))
	  (goto-char header))))))

(defun jmail-view-thread-previous ()
  (interactive)
  (previous-line)
  (unless (jmail-view-thread--find-fold-overlay (line-end-position)
						(line-end-position))
    (previous-line)
    (when-let* ((props (text-properties-at (point)))
		(start (plist-get props :jmail-view-start)))
      (goto-char start)
      (forward-line -2)
      (when-let* ((props (text-properties-at (point)))
		  (header (plist-get props :jmail-view-header)))
	(goto-char header)))))

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
  (with-jmail-view-buffer
   (if-let* ((overlay (jmail-view-thread--find-fold-overlay (line-end-position)
							    (line-end-position)))
	     (start (line-beginning-position))
	     (props (overlay-properties overlay))
	     (data (plist-get props 'jmail-view-data)))
       (progn
	 (delete-overlay overlay)
	 (delete-region start (+ start 2))
	 (jmail-view-thread--update-props start -2)
	 (if (plist-get data :unload)
	     (let* ((path (plist-get data :path))
		    (thread (plist-get data :thread))
		    (handler (apply-partially #'jmail-view-thread--load-mail
					      start thread nil)))
	       (jmail-view--get-mail-data path handler))
	   (when-let* ((props (text-properties-at start))
		       (header (plist-get props :jmail-view-header)))
	     (jmail-view-thread--enable-separator start)
	     (goto-char header))))
     (jmail-view-thread--fold-current-mail))))

(defun jmail-view-thread-fold-unfold-all-mails ()
  (interactive)
  (with-jmail-view-buffer
   (if (jmail-view-thread--fold-overlays-p)
       (jmail-view-thread--unfold-all-mails (point-min))
     (goto-char (point-min))
     (save-excursion
       (while (< (point) (point-max))
	 (jmail-view-thread--fold-current-mail)
	 (jmail-view-thread-next))))))

(defun jmail-view-thread-toggle-html ()
  (interactive)
  (message "Not supported in jmail view thread"))

(defun jmail-view-thread (target objects buffer)
  (if (get-buffer jmail-view--buffer-name)
      (pop-to-buffer jmail-view--buffer-name)
    (jmail-view-thread--setup-buffer buffer))
  (jmail-view-thread--clean-up)
  (jmail-view-thread--insert-mails target objects))

(provide 'jmail-view-thread)
