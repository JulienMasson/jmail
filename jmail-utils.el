;;; jmail-utils.el --- XXXX

;; Copyright (C) 2019 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jm-config
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

(require 'tramp)

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

(defun jmail-untramp-path (path)
  (if (tramp-tramp-file-p path)
      (tramp-file-name-localname (tramp-dissect-file-name path))
    path))

(defun jmail-common-host (path1 path2)
  (cl-flet ((get-host (path)
	     (when (tramp-tramp-file-p path)
	       (tramp-file-name-host (tramp-dissect-file-name path)))))
    (eq (get-host path1) (get-host path2))))

(defun jmail-abort (msg)
  (error (substring-no-properties msg)))

(defun jmail-funcall (func &rest args)
  (condition-case-unless-debug err
      (apply func args)
    (error (message "Error %s: %S" (symbol-name func) err))))

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

(defun jmail-bold-region (beg end)
  (save-excursion
    (when-let* ((extract (delete-and-extract-region beg end))
		(str (substring-no-properties extract)))
      (goto-char beg)
      (insert (propertize str 'face 'jmail-bold-region-face)))))

(defun jmail-unbold-region (beg end)
  (save-excursion
    (when-let* ((extract (delete-and-extract-region beg end))
		(str (substring-no-properties extract)))
      (goto-char beg)
      (insert str))))

(defun jmail-tramp-executable-find (program-name)
  (with-parsed-tramp-file-name default-directory nil
    (let ((buffer (tramp-get-connection-buffer v))
	  (cmd (concat "which " program-name)))
      (with-current-buffer buffer
	(tramp-send-command v cmd)
	(goto-char (point-min))
	(when (looking-at "^\\(.+\\)")
	  (match-string 1))))))

(defun jmail-read-prompt (prompt completions)
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
    (read-from-minibuffer prompt nil keymap)))

(defun jmail-find-program (program-name)
  (if (tramp-tramp-file-p default-directory)
      (jmail-tramp-executable-find program-name)
    (executable-find program-name)))

(defun jmail-find-alphanumeric-character (from forward)
  (save-excursion
    (goto-char from)
    (if forward
	(progn
	  (end-of-line)
	  (when (re-search-forward "[[:alnum:]]" nil t)
	    (- (point) 1)))
      (beginning-of-line)
      (when (re-search-backward "[[:alnum:]]" nil t)
	(beginning-of-line)
	(re-search-forward "[[:alnum:]]" nil t)
	(- (point) 1)))))

(defun jmail-maildir-add-query (header name query queries &optional append)
  (if (assoc header queries)
      (let ((data (assoc-default header queries)))
	(setcdr (assoc header queries)
		(add-to-list 'data `(,name . ,query) append)))
    (add-to-list 'queries (cons header `((,name . ,query))) append))
  queries)

(defun jmail-autofill-maildir-queries (top)
  (when (file-exists-p top)
    (let* ((path (expand-file-name top))
	   (dirs (directory-files-recursively path "cur$" t))
  	   (subdirs (mapcar (lambda (dir)
  			      (replace-regexp-in-string
  			       (format "%s/\\(.*\\)/cur" path)
  			       "\\1" dir)) dirs))
	   queries)
      (mapc (lambda (elem)
	      (let ((query (format "maildir:/%s" elem)))
		(cl-multiple-value-bind (header name)
		    (split-string elem "/")
		  (setq queries (if name
				    (jmail-maildir-add-query header name query queries t)
				  (jmail-maildir-add-query nil header query queries))))))
	    subdirs)
      queries)))

(defun jmail-maildirs (top)
  (when (file-exists-p top)
    (when-let ((dirs (directory-files-recursively top "cur$" t)))
      (mapcar (lambda (dir)
  		(replace-regexp-in-string (format "%s\\(.*\\)/cur" top)
  					  "\\1" dir)) dirs))))

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
  (let (from user)
    (save-excursion
      (when (re-search-forward "^from[[:space:]]*" nil t)
	(setq from (buffer-substring (point) (line-end-position)))))
    (save-excursion
      (when (re-search-forward "^user[[:space:]]*" nil t)
	(setq user (buffer-substring (point) (line-end-position)))))
    (when user
      (cons user from))))

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
  (let ((name (car elem))
	(address (format "<%s>" (cdr elem))))
    (if name
	(format "%s %s" name address)
      address)))

(defun jmail-eoh-mail-point ()
  (save-excursion (rfc822-goto-eoh)))

(provide 'jmail-utils)
