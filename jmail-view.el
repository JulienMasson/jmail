;;; jmail-view.el --- XXXX

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

(require 'goto-addr)
(require 'message)
(require 'jmail-attachment)
(require 'jmail-compose)
(require 'jmail-font-lock)
(require 'jmail-html)

;;; Mode

(defvar jmail-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "F" 'jmail-view-forward)
    (define-key map "R" 'jmail-view-reply)
    (define-key map "S" 'jmail-view-save-attachments)
    (define-key map "n" 'jmail-search-next)
    (define-key map "o" 'jmail-view-open-html)
    (define-key map "p" 'jmail-search-previous)
    (define-key map "q" 'jmail-view-quit)
    (define-key map "t" 'jmail-view-toggle-html)
    map)
  "Keymap for `jmail-view-mode'")

(define-derived-mode jmail-view-mode text-mode
  "jmail view"
  (toggle-read-only t))

;;; Customization

(defcustom jmail-view-html-default-view nil
  "If non nil, we display the html part of the email by default"
  :type 'boolean
  :group 'jmail)

;;; Internal Variables

(defconst jmail-view--buffer-name "*jmail-view*")

(defvar-local jmail-view--handler nil)

(defvar-local jmail-view--html-view nil)

;;; Internal Functions

(defmacro with-jmail-view-buffer (&rest body)
  `(when (get-buffer jmail-view--buffer-name)
     (with-current-buffer jmail-view--buffer-name
       (let ((inhibit-read-only t))
	 ,@body))))

(defun jmail-view--data-at-point ()
  (when-let ((props (text-properties-at (point))))
    (plist-get props :jmail-view-data)))

(defun jmail-view--setup-buffer (buffer)
  (with-current-buffer (get-buffer-create jmail-view--buffer-name)
    (jmail-view-mode))
  (select-window (jmail-split-window-below buffer))
  (switch-to-buffer jmail-view--buffer-name))

(defun jmail-view--clean-body ()
  (let ((clean-actions '(("$"            "")
			 (">[[:blank:]]+>" ">>")
			 (">[[:blank:]]+"  "> "))))
    (save-excursion
      (dolist (action clean-actions)
	(message-goto-body)
	(while (re-search-forward (car action) nil t)
	  (replace-match (cadr action)))))))

(defun jmail-view--fill-line ()
  (let* ((indentation (get-text-property (point) 'shr-indentation))
	 (level (/ indentation 32))
	 (face (intern-soft (format "jmail-font-lock-cited-%d-face" level)))
	 (header (concat "┃" (make-string level (string-to-char " ")))))
    (when (> level 0)
      (insert (propertize " " 'display header))
      (put-text-property (line-beginning-position)
			 (line-end-position)
			 'face face))))

(defun jmail-view--insert-html (html)
  (flet ((shr-fill-line () (jmail-view--fill-line)))
    (let ((beg (point)))
      (insert html)
      (shr-render-region beg (point)))))

(defun jmail-view--address-str (data field)
  (when-let ((address (plist-get data field)))
    (mapconcat #'jmail-make-address-str address ", ")))

(defun jmail-view--date-str (data)
  (when-let ((date (plist-get data :date)))
    (format-time-string "%a, %e %b %Y %T %z" date)))

(defun jmail-view--add-attachments (msg-path attachments)
  (jmail-attachment-save-all msg-path (temporary-file-directory) nil)
  (let ((files (mapcar (lambda (file)
			 (concat (temporary-file-directory) (car file)))
		       attachments)))
    (if org-msg-mode
	(mapc #'org-msg-attach-attach files)
      (mapc #'mml-attach-file files))))

(defun jmail-view--get-attachments (data)
  (when-let* ((parts (plist-get data :parts))
	      (attachments (seq-filter (lambda (elem)
					 (plist-get elem :attachment))
				       parts)))
    (mapcar (lambda (elem)
	      (cons (plist-get elem :name) (plist-get elem :index)))
	    attachments)))

(defun jmail-view--attachments-prompt ()
  (when-let* ((data (jmail-view--data-at-point))
	      (attachments (jmail-view--get-attachments data)))
    (append (list "all") (mapcar #'car attachments))))

(defun jmail-view--attachments-str (data)
  (when-let ((attachments (jmail-view--get-attachments data)))
    (mapconcat #'car attachments ", ")))

(defun jmail-view--insert-contents (data)
  (let ((from (jmail-view--address-str data :from))
	(to (jmail-view--address-str data :to))
	(cc (jmail-view--address-str data :cc))
	(mailing-list (plist-get data :mailing-list))
	(subject (plist-get data :subject))
	(date (jmail-view--date-str data))
	(attachments (jmail-view--attachments-str data))
	(plain-text (plist-get data :body-txt))
	(html (plist-get data :body-html)))
    (cl-macrolet ((insert-header (field)
		   `(when ,field
		      (message-insert-header ',field ,field)
		      (insert "\n"))))
      (insert-header from)
      (insert-header to)
      (insert-header cc)
      (insert-header mailing-list)
      (insert-header subject)
      (insert-header date)
      (insert-header attachments))
    (cond ((and html plain-text)
	   (if jmail-view--html-view
	       (jmail-view--insert-html html)
	     (insert "\n" plain-text "\n")))
	  ((and html (not plain-text))
	   (unless jmail-view--html-view
	     (setq jmail-view--html-view t))
	   (jmail-view--insert-html html))
	  ((and (not html) plain-text)
	   (when jmail-view--html-view
	     (setq jmail-view--html-view nil))
	   (insert "\n" plain-text "\n")))))

(defun jmail-view--fontify-mail (start)
  (setq-local font-lock-defaults '(jmail-font-lock t))
  (save-excursion
    (goto-char start)
    (let ((limit (if jmail-view--html-view (jmail-view-eoh-mail-point) (point-max))))
      (while (and (not (eobp)) (< (point) limit))
	(font-lock-fontify-region (line-beginning-position) (line-end-position))
	(forward-line)))
    (goto-address-fontify start (point-max))))

(defun jmail-view--insert-mail (data)
  (with-jmail-view-buffer
   (erase-buffer)
   (jmail-view--insert-contents data)
   (unless jmail-view--html-view
     (jmail-view--clean-body))
   (add-text-properties (point-min) (point-max) (list :jmail-view-data data))
   (jmail-view--fontify-mail (point-min))
   (set-buffer-modified-p nil)
   (goto-char (point-min))))

(defun jmail-view--process-sentinel (process status)
  (when (eq (process-exit-status process) 0)
    (when-let* ((buffer (process-buffer process))
		(object (jmail-extract-sexp-object buffer))
		(handler (with-current-buffer buffer jmail-view--handler)))
      (kill-buffer buffer)
      (funcall handler object))))

(defun jmail-view--get-mail-data (path handler)
  (when-let* ((default-directory jmail-top-maildir)
	      (program (jmail-find-program jmail-index-program))
	      (args (list "view" "--nocolor" "--format=sexp" path))
	      (buffer (get-buffer-create "*jmail-view-process*"))
	      (process (apply 'start-file-process "jmail-view" buffer
			      program args)))
    (with-current-buffer buffer
      (setq jmail-view--handler handler)
      (erase-buffer))
    (set-process-filter process 'jmail-process-filter)
    (set-process-sentinel process 'jmail-view--process-sentinel)))

(defun jmail-view--signature-begin ()
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward message-signature-separator nil t)
	(- (point) 1)
      (point-max))))

(defun jmail-view--citation (beg end)
  (save-excursion
    (goto-char beg)
    (move-beginning-of-line 1)
    (while (< (point) end)
      (let* ((prefix (buffer-substring (point) (+ (point) 1)))
	     (str (if (string= prefix ">") ">" "> ")))
	(insert str)
	(setq end (+ end (length str)))
	(forward-line)))))

(defun jmail-view--insert-reply-text (from plain-text)
  (save-excursion
    (message-goto-body)
    (insert "\n")
    (message-insert-formatted-citation-line from (message-make-date))
    (let ((beg (point)))
      (insert plain-text)
      (jmail-view--clean-body)
      (jmail-view--citation beg (jmail-view--signature-begin)))))

(defun jmail-view--insert-forward-text (from date to cc subject plain-text)
  (save-excursion
    (message-goto-body)
    (insert "\n\n---------- Forwarded message ---------\n")
    (insert "From: " from "\n")
    (insert "Date: " date "\n")
    (insert "Subject: " subject "\n")
    (insert "To: " to "\n")
    (when cc
      (insert "Cc: " cc "\n"))
    (insert plain-text)))

(defun jmail-view--autodetect-account (data)
  (if-let* ((accounts (jmail-get-accounts jmail-smtp-config-file))
	    (accounts-address (mapcar #'cddr accounts))
	    (props (text-properties-at (point)))
	    (to (plist-get data :to))
	    (to-address (mapcar #'cdr to))
	    (address (car (cl-intersection accounts-address to-address
					   :test #'string=))))
      (seq-find (lambda (elem)
		  (string= address (cddr elem)))
		accounts)
    (when-let ((accounts (jmail-get-accounts jmail-smtp-config-file))
	       (account (completing-read "Select account: "
					 (mapcar #'car accounts))))
      (assoc account accounts))))

(defun jmail-view--reply-get-to (data from-email)
  (when-let* ((to (append (plist-get data :from)
			  (plist-get data :to)))
	      (to-list (mapcar #'jmail-make-address-str to)))
    (string-join (seq-remove (lambda (elem)
			       (string-match from-email elem))
			     to-list) ", ")))

;;; External Functions

(defun jmail-view-eoh-mail-point ()
  (let* ((props (text-properties-at (point)))
	 (header (plist-get props :jmail-view-header))
	 (start (if header header (point-min))))
    (save-excursion
      (goto-char start)
      (re-search-forward "^\\([:\n]\\|[^: \t\n]+[ \t\n]\\)" nil t)
      (point))))

(defun jmail-view-reply ()
  (interactive)
  (with-jmail-view-buffer
   (let* ((data (jmail-view--data-at-point))
	  (account (jmail-view--autodetect-account data))
	  (sender (car (plist-get data :from)))
	  (from (jmail-make-address-str (cdr account)))
	  (from-email (cddr account))
	  (to (jmail-view--reply-get-to data from-email))
	  (cc (jmail-view--address-str data :cc))
	  (subject (message-simplify-subject (plist-get data :subject)))
	  (plain-text (plist-get data :body-txt))
	  (in-reply-to (plist-get data :message-id)))
     (message-pop-to-buffer (message-buffer-name "reply" to))
     (message-setup `((From . ,from)
		      (To . ,to)
		      (Cc . ,cc)
		      (Subject . ,(concat "Re: " subject))
		      (In-reply-to . ,(format "<%s>" in-reply-to))))
     (message-sort-headers)
     (message-hide-headers)
     (when plain-text
       (jmail-view--insert-reply-text (jmail-make-address-str sender)
				      plain-text))
     (jmail-compose-mode)
     (jmail-company-setup)
     (jmail-compose-setup-send-mail)
     (jmail-compose-set-extra-arguments (car account) from-email)
     (message-goto-body))))

(defun jmail-view-forward ()
  (interactive)
  (with-jmail-view-buffer
   (let* ((data (jmail-view--data-at-point))
	  (account (jmail-view--autodetect-account data))
	  (from (jmail-make-address-str (cdr account)))
	  (from-email (cddr account))
	  (from-fwd (jmail-view--address-str data :from))
	  (date-fwd (jmail-view--date-str data))
	  (subject-fwd (plist-get data :subject))
	  (to-fwd (jmail-view--address-str data :to))
	  (cc-fwd (jmail-view--address-str data :cc))
	  (subject (message-simplify-subject subject-fwd))
	  (plain-text (plist-get data :body-txt))
	  (in-reply-to (plist-get data :in-reply-to))
	  (attachments (jmail-view--get-attachments data))
	  (msg-path (plist-get data :path)))
     (message-pop-to-buffer (generate-new-buffer-name "*unsent forward*"))
     (message-setup `((From . ,from)
		      (To . "")
		      (Subject . ,(concat "Fwd: " subject))
		      (In-reply-to . ,in-reply-to)))
     (message-sort-headers)
     (message-hide-headers)
     (when plain-text
       (jmail-view--insert-forward-text
	from-fwd date-fwd to-fwd cc-fwd subject-fwd plain-text))
     (jmail-compose-mode)
     (when attachments
       (jmail-view--add-attachments msg-path attachments))
     (jmail-company-setup)
     (jmail-compose-setup-send-mail)
     (jmail-compose-set-extra-arguments (car account) from-email)
     (message-goto-body))))

(defun jmail-view-save-attachments (attachments outdir)
  (interactive (list (completing-read "Save: "
				      (jmail-view--attachments-prompt))
		     (read-directory-name "Path: ")))
  (when-let* ((data (jmail-view--data-at-point))
	      (msg-path (plist-get data :path))
	      (attachments-list (jmail-view--get-attachments data)))
    (if (string= attachments "all")
	(jmail-attachment-save-all msg-path outdir)
      (jmail-attachment-save msg-path (assoc-default attachments attachments-list)
			     outdir))))

(defun jmail-view-toggle-html ()
  (interactive)
  (with-jmail-view-buffer
   (setq jmail-view--html-view (not jmail-view--html-view))
   (jmail-view--insert-mail (jmail-view--data-at-point))))

(defun jmail-view-open-html ()
  (interactive)
  (let* ((data (jmail-view--data-at-point))
	 (html (plist-get data :body-html))
	 (dir (make-temp-file "jmail-view-" t))
	 (default-directory dir)
	 (file (concat dir "/file.html"))
	 (msg-path (plist-get data :path))
	 (attachments (jmail-view--get-attachments data)))
    (when html
      (with-temp-file file
	(insert html)
	(when attachments
	  (jmail-attachment-save-all msg-path dir nil)
	  (goto-char (point-min))
	  (while (re-search-forward "cid:\\([[:graph:]]+\\)@[[:graph:]]+\"" nil t)
	    (replace-match "\\1\""))))
      (jmail-html-open file))))

(defun jmail-view-quit ()
  (interactive)
  (when (get-buffer jmail-view--buffer-name)
    (with-jmail-view-buffer
     (kill-buffer-and-window))))

(defun jmail-view (path buffer)
  (if (get-buffer jmail-view--buffer-name)
      (pop-to-buffer jmail-view--buffer-name)
    (jmail-view--setup-buffer buffer))
  (with-jmail-view-buffer
   (setq jmail-view--html-view jmail-view-html-default-view))
  (jmail-view--get-mail-data path #'jmail-view--insert-mail))

(provide 'jmail-view)
