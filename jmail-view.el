;;; jmail-view.el --- XXXX

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

(require 'goto-addr)
(require 'magit-diff)
(require 'message)
(require 'jmail-compose)
(require 'jmail-font-lock)

;;; Mode

(defvar jmail-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "F" 'jmail-view-forward)
    (define-key map "R" 'jmail-view-reply)
    (define-key map "S" 'jmail-view-save-attachments)
    (define-key map "n" 'jmail-search-next)
    (define-key map "o" 'jmail-view-open-html)
    (define-key map "O" 'jmail-view-open-raw)
    (define-key map "p" 'jmail-search-previous)
    (define-key map "q" 'jmail-view-quit)
    (define-key map "t" 'jmail-view-toggle-html)
    (define-key map (kbd "M-n") 'forward-paragraph)
    (define-key map (kbd "M-p") 'backward-paragraph)
    map)
  "Keymap for `jmail-view-mode'")

(define-derived-mode jmail-view-mode text-mode
  "jmail view"
  (setq truncate-lines nil)
  (setq buffer-read-only t))

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

(defun jmail-view--data-at-point ()
  (when-let ((props (text-properties-at (point))))
    (plist-get props :jmail-view-data)))

(defmacro with-jmail-view-buffer (&rest body)
  `(when (get-buffer jmail-view--buffer-name)
     (with-current-buffer jmail-view--buffer-name
       (let ((inhibit-read-only t))
	 ,@body))))

(defun jmail-view--setup-buffer (buffer)
  (with-current-buffer (get-buffer-create jmail-view--buffer-name)
    (jmail-view-mode))
  (select-window (jmail-split-window-below buffer))
  (switch-to-buffer jmail-view--buffer-name))

(defun jmail-view--clean-text (plain-text)
  (with-temp-buffer
    (insert plain-text)
    (goto-char (point-min))
    (let ((clean-actions '(("$"            "")
			   (">[[:blank:]]+>" ">>")
			   (">[[:blank:]]+"  "> "))))
      (save-excursion
        (dolist (action clean-actions)
	  (while (re-search-forward (car action) nil t)
	    (replace-match (cadr action))))))
    (buffer-string)))

(defun jmail-view--beg-diffstat ()
  (save-excursion
    (when (re-search-forward magit-diff-statline-re nil t)
      (goto-char (line-beginning-position))
      (while (looking-at magit-diff-statline-re)
        (forward-line -1))
      (forward-line)
      (point))))

(defun jmail-view--wash-diffstat ()
  (when-let ((beg (jmail-view--beg-diffstat)))
    (let (heading)
      (when (re-search-forward "^ ?\\([0-9]+ +files? change[^\n]*\n\\)" nil t)
        (setq heading (match-string 1))
        (magit-delete-match)
        (goto-char beg)
        (insert (propertize heading 'font-lock-face 'magit-diff-file-heading))
        (while (looking-at magit-diff-statline-re)
          (magit-bind-match-strings (file sep cnt add del) nil
            (magit-delete-line)
            (when (string-match " +$" file)
              (setq sep (concat (match-string 0 file) sep))
              (setq file (substring file 0 (match-beginning 0))))
            (let ((le (length file)) ld)
              (setq file (magit-decode-git-path file))
              (setq ld (length file))
              (when (> le ld)
                (setq sep (concat (make-string (- le ld) ?\s) sep))))
            (insert (propertize file 'font-lock-face 'magit-filename) sep cnt " ")
            (when add
              (insert (propertize add 'font-lock-face 'magit-diffstat-added)))
            (when del
              (insert (propertize del 'font-lock-face 'magit-diffstat-removed)))
            (insert "\n")))))))

(defun jmail-view--diff ()
  (jmail-view--wash-diffstat)
  (when (re-search-forward magit-diff-headline-re nil t)
    (goto-char (line-beginning-position))
    (magit-wash-sequence (apply-partially #'magit-diff-wash-diff nil))))

(defun jmail-view--cited ()
  (let ((regexp (concat "^\\(" message-cite-prefix-regexp "\\).*")))
    (while (re-search-forward regexp nil t)
      (set-text-properties (line-beginning-position) (line-end-position)
                           (list 'face (jmail-font-lock--cited-face))))))

(defun jmail-view--insert-plain-text (plain-text)
  (let ((text (with-temp-buffer
                (insert (jmail-view--clean-text plain-text))
                (dolist (func (list #'jmail-view--diff
                                    #'jmail-view--cited))
                  (save-excursion
                    (message-goto-body)
                    (funcall func)))
                (buffer-string))))
    (insert "\n" text "\n")))

(defun jmail-view--insert-html (html)
  (insert (with-temp-buffer
            (mm-inline-text-html html)
            (buffer-string))))

(defun jmail-view--insert-headers (from to cc mailing-list subject date attachments)
  (insert (with-temp-buffer
            (setq-local font-lock-defaults '(message-font-lock-keywords t))
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
            (font-lock-ensure)
            (buffer-string))))

(defun jmail-view--address-str (data field)
  (let (users)
    (dolist (user (plist-get data field))
      (when-let ((email (plist-get user :email)))
        (if-let ((name (plist-get user :name)))
            (push (format "%s <%s>" name email) users)
          (push (format "<%s>" email) users))))
    (when users
      (string-join (delete-dups users) ", "))))

(defun jmail-view--attachment-name (handle)
  (cond ((assoc "attachment" handle)
         (when-let ((attach (assoc-default "attachment" handle)))
           (assoc-default 'filename attach)))
        ((assoc "application/pdf" handle)
         (when-let ((pdf (assoc-default "application/pdf" handle)))
           (assoc-default 'name pdf)))))

(defun jmail-view--add-attachments (handles html-view)
  (let ((outdir (temporary-file-directory)))
    (dolist (handle handles)
      (when-let* ((name (jmail-view--attachment-name handle))
                  (file (concat outdir name)))
        (mm-save-part-to-file handle file)
        (if (and html-view (bound-and-true-p org-msg-mode))
	    (org-msg-attach-attach file)
          (mml-attach-file file))))))

(defun jmail-view--attachments (data)
  (when-let* ((handles (plist-get data :attachments)))
    (mapcar (lambda (handle) (jmail-view--attachment-name handle))
            handles)))

(defun jmail-view--attachments-str (data)
  (when-let* ((attachments (jmail-view--attachments data)))
    (string-join attachments ", ")))

(defun jmail-view--get-plain-text (data)
  (when-let ((mm-text (plist-get data :mm-text)))
    (with-temp-buffer
      (mm-inline-text mm-text)
      (buffer-string))))

(defun jmail-view--insert-contents (data)
  (let ((from (jmail-view--address-str data :from))
	(to (jmail-view--address-str data :to))
	(cc (jmail-view--address-str data :cc))
	(mailing-list (plist-get data :list))
	(subject (plist-get data :subject))
	(date (jmail-view--date-str data))
	(attachments (jmail-view--attachments-str data))
	(plain-text (jmail-view--get-plain-text data))
	(html (plist-get data :mm-html)))
    (jmail-view--insert-headers from to cc mailing-list subject date attachments)
    (cond ((and html plain-text)
	   (if jmail-view--html-view
	       (jmail-view--insert-html html)
	     (jmail-view--insert-plain-text plain-text)))
	  ((and html (not plain-text))
	   (setq jmail-view--html-view t)
	   (jmail-view--insert-html html))
	  ((and (not html) plain-text)
	   (setq jmail-view--html-view nil)
           (jmail-view--insert-plain-text plain-text)))))

(defun jmail-view--insert-mail (data)
  (with-jmail-view-buffer
   (erase-buffer)
   (jmail-view--insert-contents data)
   (add-text-properties (point-min) (point-max) (list :jmail-view-data data
						      :jmail-view-start (point-min)
						      :jmail-view-header (point-min)
						      :jmail-view-end (point-max)))
   (set-buffer-modified-p nil)
   (message-goto-body)))

(defun jmail-view--add-attachment (object handle)
  (let ((handles (plist-get object :attachments)))
    (add-to-list 'handles handle)
    (plist-put object :attachments handles)))

(defun jmail-view--add-image (object handle)
  (let ((handles (plist-get object :images)))
    (add-to-list 'handles handle)
    (plist-put object :images handles)))

(defun jmail-view--mm (handles object)
  (if (equal (mm-handle-media-supertype handles) "multipart")
      (dolist (handle (cdr handles))
        (jmail-view--mm handle object))
    (when (bufferp (car handles))
      (cond ((assoc "text/plain" handles)
             (plist-put object :mm-text handles))
            ((assoc "text/html" handles)
             (plist-put object :mm-html handles))
            ((assoc "attachment" handles)
             (jmail-view--add-attachment object handles))
            ((assoc "application/pdf" handles)
             (jmail-view--add-attachment object handles))
            ((assoc "image/png" handles)
             (jmail-view--add-image object handles))))))

(defun jmail-view--fill-object (object)
  (with-temp-buffer
    (insert-file-contents (plist-get object :path))
    (jmail-view--mm (mm-dissect-buffer t) object)))

(defun jmail-view--process-sentinel (process status)
  (when (eq (process-exit-status process) 0)
    (when-let* ((buffer (process-buffer process))
		(object (jmail-extract-sexp-object buffer))
		(handler (with-current-buffer buffer jmail-view--handler)))
      (kill-buffer buffer)
      (jmail-view--fill-object object)
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
      (insert (jmail-view--clean-text plain-text))
      (jmail-view--citation beg (jmail-view--signature-begin)))))

(defun jmail-view--date-str (data)
  (when-let ((date (plist-get data :date)))
    (format-time-string "%a, %e %b %Y %T %z" date)))

(defun jmail-view--insert-forward-text (plain-text data)
  (let* ((account (jmail-view--autodetect-account data))
         (from (jmail-make-address-str (car (plist-get data :from))))
         (date (jmail-view--date-str data))
         (subject (message-simplify-subject (plist-get data :subject)))
         (to (jmail-view--address-to data (cdr account)))
	 (cc (jmail-view--address-cc data)))
    (save-excursion
      (message-goto-body)
      (insert "\n\n---------- Forwarded message ---------\n")
      (insert "From: " from "\n")
      (insert "Date: " date "\n")
      (insert "Subject: " subject "\n")
      (insert "To: " to "\n")
      (when cc (insert "Cc: " cc "\n"))
      (insert plain-text))))

(defun jmail-view--all-emails (data)
  (mapcar (lambda (e) (plist-get e :email)) data))

(defun jmail-view--autodetect-account (data)
  (if-let* ((accounts (jmail-get-accounts jmail-smtp-config-file))
	    (accounts-emails (jmail-view--all-emails (mapcar #'cdr accounts)))
	    (target (append (jmail-view--all-emails (plist-get data :from))
			    (jmail-view--all-emails (plist-get data :to))
			    (jmail-view--all-emails (plist-get data :cc))))
	    (email (car (cl-intersection accounts-emails target :test #'string=))))
      (cl-find-if (lambda (e) (string= email (plist-get (cdr e) :email))) accounts)
    (when-let ((account (completing-read "Select account: " (mapcar #'car accounts))))
      (assoc account accounts))))

(defun jmail-view--address-to (data from)
  (when-let* ((to-list (append (plist-get data :from) (plist-get data :to)))
              (to-list (cl-remove-if (lambda (to)
                                       (string= (plist-get to :email)
                                                (plist-get from :email)))
                                     to-list))
              (users (mapcar (lambda (e) (jmail-make-address-str e)) to-list)))
    (string-join (delete-dups users) ", ")))

(defun jmail-view--address-cc (data)
  (when-let* ((cc-list (plist-get data :cc))
              (users (mapcar (lambda (e) (jmail-make-address-str e)) cc-list)))
    (string-join (delete-dups users) ", ")))

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
          (account-email (plist-get (cdr account) :email))
          (sender (jmail-make-address-str (car (plist-get data :from))))
	  (from (jmail-make-address-str (cdr account)))
	  (to (jmail-view--address-to data (cdr account)))
	  (cc (jmail-view--address-cc data))
	  (subject (message-simplify-subject (plist-get data :subject)))
          (plain-text (jmail-view--get-plain-text data))
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
       (jmail-view--insert-reply-text sender plain-text))
     (jmail-compose-mode)
     (jmail-capf-setup)
     (jmail-compose-setup-send-mail)
     (jmail-compose-set-extra-arguments (car account) account-email)
     (message-goto-body))))

(defun jmail-view-forward ()
  (interactive)
  (with-jmail-view-buffer
   (let* ((data (jmail-view--data-at-point))
	  (account (jmail-view--autodetect-account data))
          (account-email (plist-get (cdr account) :email))
	  (from (jmail-make-address-str (cdr account)))
	  (subject (message-simplify-subject (plist-get data :subject)))
          (plain-text (jmail-view--get-plain-text data))
	  (in-reply-to (plist-get data :message-id))
          (attachments (plist-get data :attachments))
          (html-view jmail-view--html-view))
     (message-pop-to-buffer (message-buffer-name "forward"))
     (message-setup `((From . ,from)
		      (To . "")
		      (Subject . ,(concat "Fwd: " subject))
		      (In-reply-to . ,in-reply-to)))
     (message-sort-headers)
     (message-hide-headers)
     (when plain-text
       (jmail-view--insert-forward-text plain-text data))
     (jmail-compose-mode)
     (when attachments
       (jmail-view--add-attachments attachments html-view))
     (jmail-capf-setup)
     (jmail-compose-setup-send-mail)
     (jmail-compose-set-extra-arguments (car account) account-email)
     (message-goto-body))))


(defun jmail-view--find-attachment (handles name)
  (cl-find-if (lambda (handle)
                (string= (jmail-view--attachment-name handle) name))
              handles))

(defun jmail-view-save-attachments ()
  (interactive)
  (let* ((data (jmail-view--data-at-point))
         (handles (plist-get data :attachments))
         (attachments (jmail-view--attachments data))
         (target (completing-read "Save: " (append (list "all") attachments)))
         (outdir (read-directory-name "Path: " "~/Downloads/")))
    (dolist (name (if (string= target "all") attachments (list target)))
      (when-let ((handle (jmail-view--find-attachment handles name)))
        (mm-save-part-to-file handle (concat outdir name))))
    (dired-other-window outdir)))

(defun jmail-view-save-all-images (handles outdir)
  (dolist (handle handles)
    (when-let* ((image (assoc-default "image/png" handle))
                (name (assoc-default 'name image)))
      (mm-save-part-to-file handle (format "%s/%s" outdir name)))))

(defun jmail-view-toggle-html ()
  (interactive)
  (with-jmail-view-buffer
   (setq jmail-view--html-view (not jmail-view--html-view))
   (jmail-view--insert-mail (jmail-view--data-at-point))))

(defun jmail-view-open-html ()
  (interactive)
  (when-let* ((data (jmail-view--data-at-point))
	      (html (plist-get data :mm-html))
	      (outdir (make-temp-file "jmail-view-" t))
	      (default-directory outdir)
	      (file (concat outdir "/file.html")))
    (with-temp-file file
      (insert (mm-get-part html))
      (setq coding-system-for-write 'raw-text)
      (when-let ((images (plist-get data :images)))
	(jmail-view-save-all-images images outdir)
        (goto-char (point-min))
	(while (re-search-forward "cid:\\([[:graph:]]+\\)@[[:graph:]]+\"" nil t)
	  (replace-match "\\1\""))))
    (browse-url file)))

(defun jmail-view-open-raw ()
  (interactive)
  (when-let* ((data (jmail-view--data-at-point))
	      (file (plist-get data :path)))
    (find-file file)))

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
