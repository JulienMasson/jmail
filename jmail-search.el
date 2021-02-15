;;; jmail-search.el --- XXXX

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

(require 'jmail-actions)
(require 'jmail-view)

;;; Mode

(defvar jmail-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'jmail-actions-apply)

    (define-key map "d" 'jmail-search-delete-at-point-or-region)
    (define-key map "D" 'jmail-search-delete-thread)

    (define-key map "m" 'jmail-search-mark-at-point-or-region)
    (define-key map "M" 'jmail-search-mark-thread)

    (define-key map "r" 'jmail-search-move-at-point-or-region)
    (define-key map "R" 'jmail-search-move-thread)

    (define-key map "t" 'jmail-search-toggle-thread)
    (define-key map "T" 'jmail-search-toggle-related)

    (define-key map (kbd "TAB") 'jmail-search-fold-unfold-thread)
    (define-key map (kbd "C-TAB") 'jmail-search-fold-unfold-all-thread)

    (define-key map "g" 'jmail-search-refresh)
    (define-key map "s" 'jmail-search-rerun)

    (define-key map "L" 'jmail-search-display-all)

    (define-key map "n" 'jmail-search-next)
    (define-key map "p" 'jmail-search-previous)
    (define-key map (kbd "C-<down>") 'jmail-search-next-thread)
    (define-key map (kbd "C-<up>") 'jmail-search-previous-thread)
    (define-key map (kbd "M-<right>") 'jmail-search-next-query)
    (define-key map (kbd "M-<left>") 'jmail-search-previous-query)

    (define-key map [return] 'jmail-search-enter)
    (define-key map (kbd "<C-return>") 'jmail-search-show-this-thread)

    (define-key map "q" 'jmail-search-quit)

    map)
  "Keymap for `jmail-search-mode'")

(define-derived-mode jmail-search-mode fundamental-mode
  "jmail search"
  (jmail-search--insert-header-line)
  (setq-local hl-line-face 'jmail-search-hl-line)
  (setq truncate-lines t)
  (add-hook 'window-scroll-functions #'jmail-search--after-scroll nil t)
  (add-hook 'isearch-mode-hook #'jmail-search-display-all nil t)
  (toggle-read-only t))

;;; Faces

(defface jmail-search-hl-line
  '((t :inherit region :weight bold :underline t))
  "Face with which to highlight the current line in `jmail-search-mode'"
  :group 'jmail)

(defface jmail-search-overlay-fold-face
  '((t :inherit 'font-lock-keyword-face))
  "Default face used to display `jmail-search--overlay-string'"
  :group 'jmail)

(defface jmail-search-results-footer-face
  '((t :slant italic))
  "Default face used to display results footer"
  :group 'jmail)

;;; Customization

(defcustom jmail-search-threaded-view t
  "If non nil, search buffer will display threaded messages by default.
The user is still able to toggle the view with `jmail-search-toggle-thread'."
  :type 'boolean
  :group 'jmail)

(defcustom jmail-search-show-flags t
  "If non nil, search buffer will display flags in front of message title"
  :type 'boolean
  :group 'jmail)

(defcustom jmail-search-bold-unread-message nil
  "If non nil, unread message title is bold"
  :type 'boolean
  :group 'jmail)

(defcustom jmail-search-mark-flags '(("read"      . jmail-search--mark-as-read)
				     ("unread"    . jmail-search--mark-as-unread)
				     ("flagged"   . jmail-search--mark-as-flagged)
				     ("unflagged" . jmail-search--mark-as-unflagged))
  "Alist of flags used to mark messages"
  :type 'alist
  :group 'jmail)

(defcustom jmail-search-fields '("cc:" "bcc:" "from:" "to:" "subject:"
				 "body:" "maildir:" "msgid:" "prio:"
				 "flag:" "date:" "size:" "embed:" "file:"
				 "mime:" "tag:" "list:")
  "List of search fields supported"
  :type 'list
  :group 'jmail)

;;; Internal Variables

(defconst jmail-search--process-buffer-name "*jmail-search-process*")

(defvar jmail-search--process-next nil)

(defconst jmail-search--buffer-name "*jmail-search*")

(defvar-local jmail-search--current nil)

(defvar jmail-search--objects-thread nil)

(defvar-local jmail-search--saved-objects nil)

(defvar-local jmail-search--done nil)

(defvar-local jmail-search--flags-overlays nil)

(defvar-local jmail-search--fold-overlays nil)

(defconst jmail-search--overlay-string " [...]")

(defvar jmail-search--saved nil)

(defvar jmail-search--saved-index 0)

;;; Internal Functions

(defmacro with-jmail-search-buffer (&rest body)
  `(when (get-buffer jmail-search--buffer-name)
     (with-current-buffer jmail-search--buffer-name
       (let ((inhibit-read-only t))
	 ,@body))))

(defun jmail-search--insert-header-line ()
  (with-jmail-search-buffer
   (setq header-line-format (format " %-11s %-16s  %s" "Date" "From" "Subject"))
   (force-mode-line-update)))

(defun jmail-search--subject (object)
  (if-let ((subject (plist-get object :subject)))
      subject
    "(no subject)"))

(defun jmail-search--flags (object)
  (plist-get object :flags))

(defun jmail-search--thread (object)
  (when-let ((thread (plist-get object :thread)))
    (let* ((level (plist-get thread :level))
	   (last-child (plist-get thread :last-child))
	   (spaces (make-string (* level 4) (string-to-char " "))))
      (cond ((= level 1)
	     (if last-child "  ┗━▶" "  ┣━▶"))
	    ((> level 1)
	     (if last-child
		 (format "  ┃%s┗━▶" spaces)
	       (format "  ┃%s┣━▶" spaces)))))))

(defun jmail-search--date (object)
  (propertize (format-time-string "%F" (plist-get object :date))
	      'face 'font-lock-comment-face))

(defun jmail-search--from (object)
  (if-let ((from (car (plist-get object :from))))
      (if-let ((name (car from)))
	  (propertize (truncate-string-to-width name 16)
		      'face 'font-lock-variable-name-face)
	(propertize (truncate-string-to-width (cdr from) 16)
		    'face 'font-lock-variable-name-face))
    (propertize "unknown" 'face 'font-lock-variable-name-face)))

(defun jmail-search--find-flags (pos)
  (cl-find-if (lambda (ov)
		(and (<= (overlay-start ov) pos)
		     (>= (overlay-end ov) pos)))
	      jmail-search--flags-overlays))

(defun jmail-search--svg-flags (flags)
  (let (svg)
    (mapc (lambda (flag)
	    (cond ((eq flag 'unread)
		   (add-to-list 'svg (svg-rounded-text "unread" "white" "red")))
		  ((eq flag 'flagged)
		   (add-to-list 'svg (svg-rounded-text "star" "white" "goldenrod")))
		  ((eq flag 'attach)
		   (add-to-list 'svg (svg-rounded-text "attach" "white" "DarkViolet")))))
	  flags)
    svg))

(defun jmail-search--insert-flags (start flags)
  (when (cl-intersection flags (list 'unread 'flagged 'attach))
    (let* ((svg-flags (jmail-search--svg-flags flags))
	   (svg-str (mapconcat (lambda (svg-flag)
				 (propertize "x" 'display svg-flag))
			       svg-flags " "))
	   (overlay (make-overlay start (+ start 1))))
      (overlay-put overlay 'invisible t)
      (overlay-put overlay 'before-string (format " %s" svg-str))
      (add-to-list 'jmail-search--flags-overlays overlay))))

(defun jmail-search--delete-all-overlays ()
  (with-jmail-search-buffer
   (delete-all-overlays)
   (setq jmail-search--fold-overlays nil)
   (setq jmail-search--flags-overlays nil)))

(defun jmail-search--process-results (object)
  (with-jmail-search-buffer
   (let ((from (jmail-search--from object))
	 (date (jmail-search--date object))
	 (thread (jmail-search--thread object))
	 (flags (jmail-search--flags object))
	 (subject (jmail-search--subject object))
	 subject-start)
     (save-excursion
       (goto-char (point-max))
       (insert (format "%-11s %-16s  %s" date from (if thread thread "")))
       (setq subject-start (point))
       ;; insert flags
       (when jmail-search-show-flags
	 (jmail-search--insert-flags (point) flags))
       ;; insert subject
       (if (and jmail-search-bold-unread-message (member 'unread flags))
	   (insert " " (propertize subject 'face 'bold))
	 (insert " " subject))
       (setq object (append (list :subject-start subject-start) object))
       (add-text-properties (line-beginning-position) (line-end-position) object)
       (insert "\n")))))

(defun jmail-search--update-flags ()
  (when-let* ((object (text-properties-at (point)))
	      (start (plist-get object :subject-start)))
    (when-let ((overlay (jmail-search--find-flags start)))
      (setq jmail-search--flags-overlays (remove overlay jmail-search--flags-overlays))
      (delete-overlay overlay))
    (when-let ((flags (jmail-search--flags object)))
      (jmail-search--insert-flags start flags))))

(defun jmail-search--set-flag (flag)
  (when-let* ((props (text-properties-at (point)))
	      (start (plist-get props :subject-start)))
    (jmail-search--insert-flags start (list flag))))

(defun jmail-search--set-property (prop value)
  (put-text-property (line-beginning-position)
		     (line-end-position)
		     prop value))

(defun jmail-search--rename-file (old-path flags)
  (let ((pattern "\\(.+\\)\\(?:cur\\|new\\|tmp\\)/\\(.+U=[0-9]+:2,\\).*")
	(new-path old-path))
    (when (string-match pattern old-path)
      (bind-match-strings (dir file) old-path
	(setq new-path (concat dir (if (member 'new flags) "new/" "cur/") file))
	(when (member 'flagged flags)
	  (setq new-path (concat new-path "F")))
	(unless (member 'unread flags)
	  (setq new-path (concat new-path "S")))
	(unless (string= old-path new-path)
	  (rename-file old-path new-path))))
    new-path))

(defun jmail-search--unbold-subject ()
  (when-let* ((object (text-properties-at (point)))
	      (subject-start (plist-get object :subject-start)))
    (jmail-unbold-region subject-start (line-end-position))))

(defun jmail-search--bold-subject ()
  (when-let* ((object (text-properties-at (point)))
	      (subject-start (plist-get object :subject-start)))
    (jmail-bold-region subject-start (line-end-position))))

(defun jmail-search--mark-as-read ()
  (with-jmail-search-buffer
   (when-let* ((point (line-beginning-position))
	       (object (text-properties-at point))
	       (path (plist-get object :path))
	       (flags (plist-get object :flags))
	       (new-path path))
     (when (member 'new flags)
       (setq flags (remove 'new flags))
       (jmail-search--set-property :flags flags))
     (when (member 'unread flags)
       (setq flags (remove 'unread flags))
       (jmail-search--set-property :flags flags)
       (jmail-search--update-flags)
       (when jmail-search-bold-unread-message
	 (jmail-search--unbold-subject)))
     (setq new-path (jmail-search--rename-file path flags))
     (jmail-search--set-property :path new-path))))

(defun jmail-search--mark-as-unread ()
  (with-jmail-search-buffer
   (when-let* ((point (line-beginning-position))
	       (object (text-properties-at point))
	       (path (plist-get object :path))
	       (flags (plist-get object :flags))
	       (str (propertize "U " 'face 'error))
	       (new-path path))
     (unless (member 'unread flags)
       (push 'unread flags)
       (jmail-search--set-property :flags flags)
       (setq new-path (jmail-search--rename-file path flags))
       (jmail-search--set-property :path new-path)
       (jmail-search--update-flags)
       (when jmail-search-bold-unread-message
	 (jmail-search--bold-subject))))))

(defun jmail-search--mark-as-flagged ()
  (with-jmail-search-buffer
   (when-let* ((point (line-beginning-position))
	       (object (text-properties-at point))
	       (path (plist-get object :path))
	       (flags (plist-get object :flags))
	       (str (propertize "S " 'face 'font-lock-warning-face))
	       (new-path path))
     (unless (member 'flagged flags)
       (push 'flagged flags)
       (jmail-search--set-property :flags flags)
       (setq new-path (jmail-search--rename-file path flags))
       (jmail-search--set-property :path new-path)
       (jmail-search--update-flags)))))

(defun jmail-search--mark-as-unflagged ()
  (with-jmail-search-buffer
   (when-let* ((point (line-beginning-position))
	       (object (text-properties-at point))
	       (path (plist-get object :path))
	       (flags (plist-get object :flags))
	       (new-path path))
     (when (member 'flagged flags)
       (setq flags (remove 'flagged flags))
       (jmail-search--set-property :flags flags)
       (jmail-search--update-flags)
       (setq new-path (jmail-search--rename-file path flags))
       (jmail-search--set-property :path new-path)))))

(defun jmail-search--args (query thread related)
  (let ((option "find")
	(default-args (list "--reverse"
			    "--format=sexp"
			    query)))
    (when thread
      (push "--threads" default-args))
    (when related
      (push "--include-related" default-args))
    (push option default-args)))

(defun jmail-search--insert-footer ()
  (with-jmail-search-buffer
   (when (and jmail-search--done
	      (not jmail-search--saved-objects))
     (save-excursion
       (goto-char (point-max))
       (if (and (eq (point-min) (point-max))
		(not (eq (cadr (text-properties-at (point-min)))
			 'jmail-search-results-footer-face)))
	   (insert (propertize "No results found !!!!"
			       'face 'jmail-search-results-footer-face))
	 (unless (eq (cadr (text-properties-at (- (point-max) 1)))
		     'jmail-search-results-footer-face)
	   (insert (propertize "End of search results."
			       'face 'jmail-search-results-footer-face))))))))

(defun jmail-search--after-scroll (win _start)
  (jmail-search--display-saved-objects))

(defun jmail-search--save-objects-handler ()
  (with-jmail-search-buffer
   (let (object)
     (while (setq object (jmail-extract-sexp-object
			  jmail-search--process-buffer-name))
       (add-to-list 'jmail-search--saved-objects object t))))
  (when jmail-search--done
    (kill-buffer jmail-search--process-buffer-name)))

(defun jmail-search--save-objects ()
  (unless (and jmail-search--objects-thread
	       (thread-live-p jmail-search--objects-thread))
    (setq jmail-search--objects-thread
	  (make-thread #'jmail-search--save-objects-handler))))

(defun jmail-search--display-saved-objects ()
  (with-jmail-search-buffer
   (while (and jmail-search--saved-objects
	       (< (count-lines (window-start) (point-max))
		  (window-total-height)))
     (jmail-search--process-results (pop jmail-search--saved-objects))))
  (jmail-search--insert-footer))

(defun jmail-search--display-objects (buffer)
  (let (object)
    (while (setq object (jmail-extract-sexp-object buffer))
      (jmail-search--process-results object))))

(defun jmail-search--process-objects (buffer)
  (with-jmail-search-buffer
   (if (>= (count-lines (window-start (get-buffer-window (current-buffer)))
		       (point-max))
	   (window-total-height))
       (jmail-search--save-objects)
     (jmail-search--display-objects buffer)
     (when (and jmail-search--done
		(or (not jmail-search--objects-thread)
		    (not (thread-live-p jmail-search--objects-thread))))
       (kill-buffer jmail-search--process-buffer-name)))))

(defun jmail-search--process-done ()
  (with-jmail-search-buffer
   (setq jmail-search--done t)))

(defun jmail-search--process-sentinel (process status)
  (jmail-search--process-done)
  (when (and (eq (process-exit-status process) 0)
  	     (buffer-live-p (process-buffer process)))
    (jmail-search--process-objects (process-buffer process)))
  (jmail-search--insert-footer)
  (when jmail-search--process-next
    (jmail-search--setup-env)
    (jmail-search--process-run jmail-search--process-next)
    (setq jmail-search--process-next nil)))

(defun jmail-search--process-filter (process str)
  (unless (eq (process-status process) 'signal)
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert str)
      (jmail-search--process-objects (current-buffer)))))

(defun jmail-search--process-run (args)
  (when-let* ((default-directory jmail-top-maildir)
	      (program (jmail-find-program jmail-index-program))
	      (buffer (get-buffer-create jmail-search--process-buffer-name))
	      (process (apply 'start-file-process "jmail-search-process" buffer
			      program args)))
    (with-current-buffer buffer
      (erase-buffer))
    (set-process-filter process 'jmail-search--process-filter)
    (set-process-sentinel process 'jmail-search--process-sentinel)))

(defun jmail-search--stop-process ()
  (setq jmail-search--process-next nil)
  (jmail-terminate-process-buffer jmail-search--process-buffer-name))

(defun jmail-search--process (args)
  (if (jmail-terminate-process-buffer jmail-search--process-buffer-name)
      (setq jmail-search--process-next args)
    (jmail-search--process-run args)))

(defun jmail-search--setup-env ()
  (with-jmail-search-buffer
   (jmail-search--delete-all-overlays)
   (erase-buffer)
   (setq jmail-search--done nil)
   (setq jmail-search--saved-objects nil)))

(defun jmail-search--run (query thread related &optional save)
  (unless (get-buffer jmail-search--buffer-name)
    (with-current-buffer (get-buffer-create jmail-search--buffer-name)
      (jmail-search-mode)))
  (with-jmail-search-buffer
   (jmail-search--setup-env)
   (setq jmail-search--current `(:query ,query
				 :thread ,thread
				 :related ,related))
   (when save
     (push jmail-search--current jmail-search--saved))
   (switch-to-buffer (current-buffer))
   (jmail-search--process (jmail-search--args query thread related))))

(defun jmail-search--thread-level-at-point ()
  (with-jmail-search-buffer
   (when-let* ((object (text-properties-at (point)))
	       (thread (plist-get object :thread)))
       (plist-get thread :level))))

(defun jmail-search--goto-root-thread ()
  (when-let* ((object (text-properties-at (point)))
	      (thread (plist-get object :thread))
	      (level (plist-get thread :level)))
    (unless (zerop level)
      (previous-line)
      (jmail-search--goto-root-thread))))

(defmacro jmail-search--foreach-line-thread (&rest body)
  `(with-jmail-search-buffer
    (save-excursion
      (lexical-let (start end)
	(jmail-search--goto-root-thread)
	(setq start (line-beginning-position))
	,@body
	(next-line)
	(while (and (jmail-search--thread-level-at-point)
		    (not (zerop (jmail-search--thread-level-at-point))))
	  ,@body
	  (next-line))
	(setq end (line-beginning-position))
	(list start end)))))

(defmacro jmail-search--foreach-line-region (&rest body)
  `(with-jmail-search-buffer
    (save-excursion
      (when (region-active-p)
	(lexical-let ((beg (region-beginning))
		      (end (region-end))
		      start)
	  (deactivate-mark)
	  (goto-char beg)
	  (setq start (line-beginning-position))
	  (while (<= (point) end)
	    ,@body
	    (next-line))
	  (list start (line-beginning-position)))))))

(defun jmail-search--thread-range ()
  (if-let ((level (jmail-search--thread-level-at-point)))
      (let ((start (line-end-position))
	    end)
	(save-excursion
	  (forward-line)
	  (while (and (jmail-search--thread-level-at-point)
		      (< level (jmail-search--thread-level-at-point))
		      (not (eobp)))
	    (forward-line))
	  (setq end (- (point) 1)))
	(list start end))
    (list (point) (point))))

(defun jmail-search--find-fold-overlay (start end)
  (cl-find-if (lambda (ov)
		(and (<= (overlay-start ov) start)
		     (>= (overlay-end ov) end)))
	      jmail-search--fold-overlays))

(defun jmail-search--remove-fold-overlay (overlay)
  (setq jmail-search--fold-overlays (remove overlay jmail-search--fold-overlays))
  (delete-overlay overlay))

(defun jmail-search--add-fold-overlay (start end)
  (let ((overlay (make-overlay start end)))
    (add-to-list 'jmail-search--fold-overlays overlay)
    (overlay-put overlay 'invisible t)
    (overlay-put overlay 'before-string
		 (propertize jmail-search--overlay-string
			     'face 'jmail-search-overlay-fold-face))))

(defun jmail-search--remove-overlay ()
  (when-let* ((object (text-properties-at (point)))
	      (start (plist-get object :subject-start))
	      (overlay (jmail-search--find-flags start)))
    (setq jmail-search--flags-overlays (remove overlay jmail-search--flags-overlays))
    (delete-overlay overlay)))

(defun jmail-search--remove-overlay-range (start end)
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (jmail-search--remove-overlay)
      (next-line))))

(defun jmail-search--delete-region (start end)
  (with-jmail-search-buffer
   (delete-region start end)))

(defun jmail-search--delete-line ()
  (jmail-search--delete-region (line-beginning-position)
			       (+ (line-end-position) 1)))

(defun jmail-search--delete-message ()
  (with-jmail-search-buffer
   (when-let* ((object (text-properties-at (point)))
	       (path (plist-get object :path)))
     (delete-file path))))

(defun jmail-search--read-uidvalidity (dir)
  (when-let* ((default-directory dir)
	      (data (with-temp-buffer
		      (insert-file-contents ".uidvalidity")
		      (split-string (buffer-string) "\n" t))))
    (string-to-number (cadr data))))

(defun jmail-search--write-uidvalidity (dir val)
  (let ((default-directory dir)
	(message-log-max nil))
    (with-current-buffer (find-file-noselect ".uidvalidity")
      (goto-char (- (point-max) 1))
      (delete-region (line-beginning-position)
		     (line-end-position))
      (insert (number-to-string val))
      (save-buffer)
      (kill-buffer))))

(defun jmail-search--move-message (dest-dir)
  (with-jmail-search-buffer
   (when-let* ((object (text-properties-at (point)))
	       (path (plist-get object :path))
	       (src-dir (expand-file-name (concat path "/../../")))
	       (dest-uid (jmail-search--read-uidvalidity dest-dir))
	       (new-path (replace-regexp-in-string
			  ".*/\\(cur\\|new\\|tmp\\)\\(.*U=\\)[0-9]+\\(:.*\\)"
			  (format "%s/\\1\\2%d\\3" dest-dir (+ dest-uid 1))
			  path)))
     (jmail-search--set-property :path new-path)
     (jmail-search--write-uidvalidity dest-dir (+ dest-uid 1))
     (rename-file path new-path))))

;;; External Functions

(defun jmail-search-delete-at-point-or-region (confirm)
  (interactive (list (yes-or-no-p (if (region-active-p)
				      "Delete all messages from region: "
				    "Delete message: "))))
  (when confirm
    (if (region-active-p)
	(let ((range (jmail-search--foreach-line-region
		      (jmail-search--delete-message))))
	  (apply #'jmail-search--remove-overlay-range range)
	  (apply #'jmail-search--delete-region range))
      (jmail-search--delete-message)
      (jmail-search--remove-overlay)
      (jmail-search--delete-line))
    (jmail-fetch-refresh-all t)))

(defun jmail-search-delete-thread (confirm)
  (interactive (list (yes-or-no-p "Delete whole thread: ")))
  (when confirm
    (let ((range (jmail-search--foreach-line-thread
		  (jmail-search--delete-message))))
      (apply #'jmail-search--remove-overlay-range range)
      (apply #'jmail-search--delete-region range)
      (jmail-fetch-refresh-all t))))

(defun jmail-search-mark-at-point-or-region (flag)
  (interactive (list (completing-read (if (region-active-p)
					  "Mark on region as: "
					"Mark at point as: ")
				      (mapcar #'car jmail-search-mark-flags))))
  (if (region-active-p)
      (jmail-search--foreach-line-region
       (funcall (assoc-default flag jmail-search-mark-flags)))
    (funcall (assoc-default flag jmail-search-mark-flags)))
  (jmail-fetch-refresh-all t))

(defun jmail-search-mark-thread (flag)
  (interactive (list (completing-read "Mark whole thread as: "
				      (mapcar #'car jmail-search-mark-flags))))
  (jmail-search--foreach-line-thread
   (funcall (assoc-default flag jmail-search-mark-flags)))
  (jmail-fetch-refresh-all t))

(defun jmail-search-apply-patch-series (dir)
  (interactive "DApply patch series: ")
  (jmail-search--foreach-line-thread
      (when-let* ((object (text-properties-at (point)))
		  (thread (plist-get object :thread))
		  (level (plist-get thread :level))
		  (msg (plist-get object :path))
		  (subject (plist-get object :subject))
		  (default-directory dir)
		  (tmp-patch (concat default-directory ".jmail.patch")))
	(when (and (string-match "^\\[PATCH " subject)
		   (= level 1))
	  (if (not (tramp-tramp-file-p default-directory))
	      (shell-command (concat "git am " msg))
	    (copy-file msg tmp-patch t)
	    (shell-command (concat "git am " (jmail-untramp-path tmp-patch)))
	    (delete-file tmp-patch))))))

(defun jmail-search-apply-patch (dir)
  (interactive "DApply patch: ")
  (with-jmail-search-buffer
   (when-let* ((object (text-properties-at (point)))
	       (msg (plist-get object :path))
	       (subject (plist-get object :subject))
	       (default-directory dir)
	       (tmp-patch (concat default-directory ".jmail.patch")))
     (when (string-match "^\\[PATCH " subject)
       (if (not (tramp-tramp-file-p default-directory))
	   (shell-command (concat "git am " msg))
	 (copy-file msg tmp-patch t)
	 (shell-command (concat "git am " (jmail-untramp-path tmp-patch)))
	 (delete-file tmp-patch))))))

(defun jmail-search-move-at-point-or-region (maildir)
  (interactive (list (completing-read (if (region-active-p)
					  "Move region to: "
					"Move to: ")
				      (jmail-maildirs (jmail-get-top-maildir)))))
  (if (region-active-p)
      (jmail-search--foreach-line-region
       (jmail-search--move-message (concat (jmail-get-top-maildir) maildir)))
    (jmail-search--move-message (concat (jmail-get-top-maildir) maildir)))
  (jmail-fetch-refresh-all t))

(defun jmail-search-move-thread (maildir)
  (interactive (list (completing-read "Move whole thread to: "
				      (jmail-maildirs (jmail-get-top-maildir)))))
  (jmail-search--foreach-line-thread
   (jmail-search--move-message (concat (jmail-get-top-maildir) maildir)))
  (jmail-fetch-refresh-all t))

(defun jmail-search-toggle-thread ()
  (interactive)
  (when jmail-search--current
    (let ((query (plist-get jmail-search--current :query))
	  (thread (not (plist-get jmail-search--current :thread)))
	  (related (plist-get jmail-search--current :related)))
      (jmail-search--run query thread related))))

(defun jmail-search-toggle-related ()
  (interactive)
  (when jmail-search--current
    (let ((query (plist-get jmail-search--current :query))
	  (thread (plist-get jmail-search--current :thread))
	  (related (not (plist-get jmail-search--current :related))))
      (jmail-search--run query thread related))))

(defun jmail-search-fold-unfold-thread ()
  (interactive)
  (cl-multiple-value-bind (start end)
      (jmail-search--thread-range)
    (unless (= start end)
      (if-let ((overlay (jmail-search--find-fold-overlay start end)))
	  (jmail-search--remove-fold-overlay overlay)
	(jmail-search--add-fold-overlay start end)))))

(defun jmail-search-fold-unfold-all-thread ()
  (interactive)
  (if jmail-search--fold-overlays
      (mapc #'jmail-search--remove-fold-overlay
	    jmail-search--fold-overlays)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
	(jmail-search-toggle-folding-thread)
	(line-move 1)))))

(defun jmail-search-refresh ()
  (interactive)
  (when jmail-search--current
    (let ((query (plist-get jmail-search--current :query))
	  (thread (plist-get jmail-search--current :thread))
	  (related (plist-get jmail-search--current :related)))
      (jmail-search--run query thread related))))

(defun jmail-search-rerun ()
  (interactive)
  (when jmail-search--current
    (let* ((query (plist-get jmail-search--current :query))
	   (query (read-string "Search: " query))
	   (thread (plist-get jmail-search--current :thread))
	   (related (plist-get jmail-search--current :related)))
      (jmail-search--run query thread related))))

(defun jmail-search-display-all ()
  (interactive)
  (with-jmail-search-buffer
   (when jmail-search--saved-objects
     (let ((total (length jmail-search--saved-objects)))
       (while jmail-search--saved-objects
	 (jmail-search--process-results (pop jmail-search--saved-objects))
	 (message "Display all results: %d%%"
		  (/ (* (- total (length jmail-search--saved-objects)) 100)
		     total))))
     (jmail-search--insert-footer))))

(defun jmail-search-next ()
  (interactive)
  (unless (eq (current-buffer) (get-buffer jmail-search--buffer-name))
    (switch-to-buffer-other-window jmail-search--buffer-name))
  (with-jmail-search-buffer
   (next-line)
   (jmail-search-enter)))

(defun jmail-search-previous ()
  (interactive)
  (unless (eq (current-buffer) (get-buffer jmail-search--buffer-name))
    (switch-to-buffer-other-window jmail-search--buffer-name))
  (with-jmail-search-buffer
   (previous-line)
   (jmail-search-enter)))

(defun jmail-search-next-thread ()
  (interactive)
  (next-line)
  (when-let ((level (jmail-search--thread-level-at-point)))
    (while (and level (not (zerop level)))
      (next-line)
      (setq level (jmail-search--thread-level-at-point)))))

(defun jmail-search-previous-thread ()
  (interactive)
  (previous-line)
  (jmail-search--goto-root-thread))

(defun jmail-search-next-query ()
  (interactive)
  (let* ((size (length jmail-search--saved))
	 (index (if (zerop jmail-search--saved-index)
		    (- size 1)
		  (- jmail-search--saved-index 1)))
	 (search (nth index jmail-search--saved)))
    (setq jmail-search--current search)
    (setq jmail-search--saved-index index)
    (jmail-search-refresh)))

(defun jmail-search-previous-query ()
  (interactive)
  (let* ((size (length jmail-search--saved))
	 (index (mod (+ jmail-search--saved-index 1) size))
	 (search (nth index jmail-search--saved)))
    (setq jmail-search--current search)
    (setq jmail-search--saved-index index)
    (jmail-search-refresh)))

(defun jmail-search-enter ()
  (interactive)
  (with-jmail-search-buffer
   (jmail-search--mark-as-read)
   (when-let* ((object (text-properties-at (point)))
	       (path (plist-get object :path)))
     (jmail-view path (current-buffer)))))

(defun jmail-search-show-this-thread ()
  (interactive)
  (with-jmail-search-buffer
   (when-let* ((object (text-properties-at (point)))
	       (message-id (plist-get object :message-id))
	       (query (concat "msgid:" message-id)))
     (setq jmail-search--saved-index 0)
     (jmail-search--run query t t t))))

(defun jmail-search-quit ()
  (interactive)
  (jmail-search--stop-process)
  (jmail-view-quit)
  (jmail-search--delete-all-overlays)
  (with-jmail-search-buffer
   (kill-buffer))
  (jmail))

(defun jmail-search (query)
  (setq jmail-search--saved-index 0)
  (jmail-search--run query jmail-search-threaded-view nil t))

(provide 'jmail-search)
