;;; jmail-utils.el --- XXXX

;; Copyright (C) 2019 Julien Masson.

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

;;; Customization

(defcustom jmail-split-window-size 20
  "Upper window size line"
  :type 'integer
  :group 'jmail)

;;; Faces

(defface jmail-bold-region-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face used when bold region"
  :group 'jmail)

;;; External Functions

(defun jmail-abort (msg)
  (error (substring-no-properties msg)))

(defmacro jmail-cb (&rest body)
  `(lambda () ,@body))

(defun jmail-funcall (func &rest args)
  (condition-case-unless-debug err
      (apply func args)
    (error (message "Error %s: %S" (symbol-name func) err))))

(defun jmail-process-filter (process str)
  (when-let ((buffer (process-buffer process)))
    (when (buffer-live-p (process-buffer process))
      (with-current-buffer buffer
	(goto-char (point-max))
	(insert str)))))

(defun jmail-terminate-process-buffer (buffer)
  (when-let* ((process (get-buffer-process buffer))
	      (status (process-status process)))
    (when (eq status 'run)
      (interrupt-process process))))

(defun jmail-switch-to-buffer (buffer)
  (if (get-buffer-window-list buffer)
      (pop-to-buffer buffer)
    (switch-to-buffer buffer)))

(defun jmail-split-window-below (buffer)
  (with-current-buffer buffer
    (split-window-below jmail-split-window-size)))

(defun jmail-bold-region (beg end &optional bold-face)
  (set-text-properties beg end (list 'face (if bold-face bold-face 'bold))))

(defun jmail-unbold-region (beg end)
  (remove-text-properties beg end (list 'face 'bold)))

(defun jmail-read-prompt (prompt completions &optional initial-contents)
  (let* ((func (lambda (string)
		 (if (string-match "\\(^\\|.* (?\\)\\([^ ]*\\)$" string)
		     (mapcar (lambda (compl)
			       (concat (match-string-no-properties 1 string) compl))
			     (all-completions (match-string-no-properties 2 string)
					      completions))
		   (t (list string)))))
	 (minibuffer-completion-table (completion-table-dynamic func))
	 (keymap (copy-keymap minibuffer-local-map)))
    (define-key keymap (kbd "TAB") 'minibuffer-complete)
    (read-from-minibuffer prompt initial-contents keymap)))

(defun jmail-maildirs (top)
  (when (file-exists-p top)
    (when-let ((dirs (directory-files-recursively top "cur$" t)))
      (mapcar (lambda (dir)
  		(replace-regexp-in-string (format "%s\\(.*\\)/cur" top)
  					  "\\1" dir)) dirs))))

(defun jmail--maildir-subdirs-assoc (from dirs)
  (let (data)
    (dolist (dir dirs)
      (let* ((subdir (replace-regexp-in-string (format "%s/\\(.*\\)/cur" from)
  					       "\\1" dir))
	     (maildir (concat "maildir:/" subdir)))
	(pcase-let ((`(,root ,child) (split-string subdir "/")))
	  (if-let ((childs (assoc-default root data)))
	      (setcdr (assoc root data) (append childs (list (cons child maildir))))
	    (add-to-list 'data (cons root (list (cons child maildir))) t)))))
    data))

(defun jmail-extract-sexp-object (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward "^(" nil t)
      (backward-char)
      (when-let* ((end (ignore-errors (scan-sexps (point) 1)))
		  (str (buffer-substring (point) end)))
	(delete-region (point-min) end)
	(car (read-from-string str))))))

(defun jmail-get-account-infos ()
  (let (email user)
    (save-excursion
      (when (re-search-forward "^from[[:space:]]*" nil t)
	(setq email (buffer-substring (point) (line-end-position)))))
    (save-excursion
      (when (re-search-forward "^user[[:space:]]*" nil t)
	(setq user (buffer-substring (point) (line-end-position)))))
    (when user
      (list :name user :email email))))

(defun jmail-get-accounts (file)
  (let (accounts)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^account[[:space:]]*" nil t)
	(when-let ((account (buffer-substring (point) (line-end-position)))
		   (infos (jmail-get-account-infos)))
	  (add-to-list 'accounts (cons account infos) t))))
    accounts))

(defun jmail-get-top-maildir ()
  (when jmail-top-maildir
    (file-name-as-directory
     (expand-file-name jmail-top-maildir))))

(defun jmail-make-address-str (elem)
  (when-let ((email (plist-get elem :email)))
    (if-let ((name (plist-get elem :name)))
	(format "%s <%s>" name email)
      (format "<%s>" email))))

(provide 'jmail-utils)
