;;; jmail-org-msg.el --- XXXX

;; Copyright (C) 2020 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jmail
;; Created: 2019-07-12

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

(require 'jmail-compose)
(require 'ox-html)
(require 'org-msg)

;;; Faces

(defface jmail-org-msg-overlay-face
  '((t :inherit 'font-lock-keyword-face))
  "Default face used to display infos when composing mail in `org-msg-mode'"
  :group 'jmail)

;;; Internal Variables

(defvar jmail-org-msg--infos-hidden nil)

;;; Internal Functions

(defun jmail-org-msg--set-keymap ()
  (define-key org-msg-edit-mode-map (kbd "C-c a") 'jmail-org-msg-attach-dired-files)
  (define-key org-msg-edit-mode-map (kbd "C-c C-h") 'jmail-org-msg-toggle-infos)
  (define-key org-msg-edit-mode-map [remap org-export-dispatch] 'jmail-org-msg-preview))

(defun jmail-org-msg--range (str-start str-end)
  (let (start end)
    (save-excursion
      (message-goto-body)
      (when (re-search-forward (regexp-quote str-start) nil t)
	(setq start (line-beginning-position))
	(when (re-search-forward (regexp-quote str-end) nil t)
	  (setq end (point))
	  (list start end))))))

(defun jmail-org-msg--options-range ()
  (jmail-org-msg--range "#+OPTIONS" ":END:"))

(defun jmail-org-msg--signature-range ()
  (jmail-org-msg--range "#+begin_signature" "#+end_signature"))

(defun jmail-org-msg--hide-options ()
  (when-let* ((options-range (jmail-org-msg--options-range))
	      (overlay-options (apply #'make-overlay options-range)))
    (overlay-put overlay-options 'invisible t)
    (overlay-put overlay-options 'before-string
		 (propertize "[Org-Msg infos]" 'face 'jmail-org-msg-overlay-face))))

(defun jmail-org-msg--hide-signature ()
  (when-let ((signature-range (jmail-org-msg--signature-range))
	     (signature-options (apply #'make-overlay signature-range)))
    (overlay-put signature-options 'invisible t)
    (overlay-put signature-options 'before-string
		 (propertize "[signature]" 'face 'jmail-org-msg-overlay-face))))

(defun jmail-org-msg--hide-infos ()
  (org-show-all)
  (jmail-org-msg--hide-options)
  (jmail-org-msg--hide-signature)
  (setq jmail-org-msg--infos-hidden t))

(defun jmail-org-msg--show-infos ()
  (delete-all-overlays)
  (setq jmail-org-msg--infos-hidden nil))

(defun jmail-org-msg--reply-to (reply-data)
  (let* ((from (jmail-view--address-str reply-data :from))
	 (subject (plist-get reply-data :subject))
	 (to (jmail-view--address-str reply-data :to))
	 (cc (jmail-view--address-str reply-data :cc))
	 (date (jmail-view--date-str reply-data))
         (header (list (concat "From: " from)
                       (concat "To: " to)
                       (when cc (concat "Cc: " cc))
                       (concat "Subject: " subject)
                       (concat "Date: " date)))
         (header (string-join (delq nil header) "\n"))
         (parts (with-temp-buffer
                  (insert-file-contents (plist-get reply-data :path))
                  (mm-dissect-buffer t))))
    (with-temp-buffer
      (let ((gnus-article-buffer (current-buffer))
	    (gnus-article-mime-handles parts))
	(prog1 (org-msg-save-article-for-reply-gnus parts header)
	  (mm-destroy-parts parts))))))

(defun jmail-org-msg--setup (&optional reply-data)
  (let ((reply-to (jmail-org-msg--reply-to reply-data)))
    ;; clean-up body
    (save-excursion
      (message-goto-body)
      (delete-region (point) (point-max)))
    ;; setup body
    (message-goto-body)
    (insert (org-msg-header reply-to '(html)))
    (insert "\n\n")
    (insert org-msg-signature)
    (org-msg-edit-mode)
    (jmail-org-msg--hide-infos)
    (set-buffer-modified-p nil)
    (jmail-org-msg--set-keymap)))

;;; External Functions

(defun jmail-org-msg-preview ()
  (interactive)
  (when-let* ((file (make-temp-file "org-msg-" nil ".html"))
              (contents (buffer-substring (org-msg-start) (org-msg-end)))
	      (html (org-msg-export-as-html contents)))
    (with-temp-file file (insert html))
    (browse-url file)))

(defun jmail-org-msg-attach-dired-files ()
  "Attach marked dired files"
  (interactive)
  (when-let ((files (dired-get-all-marked)))
    (mapc #'org-msg-attach-attach files)))

(defun jmail-org-msg-toggle-infos ()
  "Toggle infos (options, signature ...) displayed in the body"
  (interactive)
  (if jmail-org-msg--infos-hidden
      (jmail-org-msg--show-infos)
    (jmail-org-msg--hide-infos)))

(defun org-msg-mode-jmail ())

(defun jmail-org-msg-enable ()
  "Enable `org-msg-mode' for jmail"
  (interactive)
  (add-to-list 'org-msg-supported-mua (cons 'jmail-user-agent "jmail"))
  (add-hook 'jmail-compose-hook 'jmail-org-msg--setup)
  (add-hook 'jmail-view-reply-hook 'jmail-org-msg--setup)
  (org-msg-mode 1))

(defun jmail-org-msg-disable ()
  "Disable `org-msg-mode' for jmail"
  (interactive)
  (org-msg-mode -1)
  (remove-hook 'jmail-compose-hook 'jmail-org-msg--setup)
  (remove-hook 'jmail-view-reply-hook 'jmail-org-msg--setup)
  (setq org-msg-supported-mua (assoc-delete-all 'jmail-user-agent org-msg-supported-mua)))

(provide 'jmail-org-msg)
