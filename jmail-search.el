;;; jmail-search.el --- XXXX

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

(require 'jmail-actions)
(require 'jmail-view)
(require 'jmail-view-thread)

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

    (define-key map "t" 'jmail-search-toggle-options)

    (define-key map (kbd "TAB") 'jmail-search-fold-unfold-thread)
    (define-key map [C-tab] 'jmail-search-fold-unfold-all-thread)

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

    (define-key map "q" 'jmail-search-back-queries)

    map)
  "Keymap for `jmail-search-mode'")

(define-derived-mode jmail-search-mode fundamental-mode
  "jmail search"
  (jmail-search--insert-header-line)
  (setq-local hl-line-face 'jmail-search-hl-line)
  (setq truncate-lines t)
  (add-hook 'window-scroll-functions #'jmail-search--after-scroll nil t)
  (setq buffer-read-only t))

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

(defcustom jmail-search-date-width 11
  "Set max width for date field in search view"
  :type 'number
  :group 'jmail)

(defcustom jmail-search-date-format "%F"
  "Date format used in search view"
  :type 'string
  :group 'jmail)

(defcustom jmail-search-from-width 16
  "Set max width for from field in search view"
  :type 'number
  :group 'jmail)

(defcustom jmail-search-thread-symbols '("┗━▶" "┣━▶" "┃")
  "List of symbols used when we display thread"
  :type 'list
  :group 'jmail)

(defcustom jmail-search-unread (propertize "" 'font-lock-face '(:foreground "firebrick1"))
  "String used to indicate unread message"
  :type 'string
  :group 'jmail)

(defcustom jmail-search-flagged (propertize "" 'font-lock-face '(:foreground "goldenrod"))
  "String used to indicate flagged message"
  :type 'string
  :group 'jmail)

(defcustom jmail-search-attach (propertize "" 'font-lock-face '(:foreground "DarkViolet"))
  "String used to indicate flagged message"
  :type 'string
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

(defvar jmail-search--saved nil)

(defvar jmail-search--saved-index 0)

;;; Internal Functions

(defmacro with-jmail-search-buffer (&rest body)
  `(when (get-buffer jmail-search--buffer-name)
     (with-current-buffer jmail-search--buffer-name
       (let ((inhibit-read-only t))
	 ,@body))))

(defun jmail-search--fmt ()
  (format "%%-%ds %%-%ds %%s" jmail-search-date-width jmail-search-from-width))

(defun jmail-search--insert-header-line ()
  (with-jmail-search-buffer
   (setq header-line-format (format (jmail-search--fmt) " Date" " From" " Subject"))
   (force-mode-line-update)))

(defun jmail-search--subject (object)
  (if-let ((subject (plist-get object :subject)))
      subject
    "(no subject)"))

(defun jmail-search--flags (object)
  (plist-get object :flags))

(defun jmail-search--subject-start ()
  (when-let ((object (text-properties-at (point))))
    (plist-get object :subject-start)))

(defun jmail-search--path ()
  (when-let ((object (text-properties-at (point))))
    (plist-get object :path)))

(defun jmail-search--thread (object)
  (when-let ((thread (plist-get object :meta)))
    (let* ((level (plist-get thread :level))
	   (last-child (plist-get thread :last-child))
	   (spaces (make-string (* level 2) (string-to-char " ")))
	   (last-child-symbol (nth 0 jmail-search-thread-symbols))
	   (child-symbol (nth 1 jmail-search-thread-symbols))
	   (separator-symbol (nth 2 jmail-search-thread-symbols)))
      (cond ((= level 1)
	     (concat "  " (if last-child last-child-symbol child-symbol)))
	    ((> level 1)
	     (if last-child
		 (concat "  " separator-symbol spaces last-child-symbol)
	       (concat "  " separator-symbol spaces child-symbol)))))))

(defun jmail-search--date (object)
  (let* ((date (plist-get object :date))
	 (str (format-time-string jmail-search-date-format date)))
    (propertize (truncate-string-to-width str jmail-search-date-width)
		'face 'font-lock-comment-face)))

(defun jmail-search--from (object)
  (if-let* ((from (car (plist-get object :from)))
            (email (plist-get from :email)))
      (let* ((name (plist-get from :name))
             (str (if name name email)))
	(propertize (truncate-string-to-width str jmail-search-from-width)
		    'face 'font-lock-variable-name-face))
    (propertize "unknown" 'face 'font-lock-variable-name-face)))

(defun jmail-search--find-flags (pos)
  (cl-find-if (lambda (ov)
		(and (<= (overlay-start ov) pos)
		     (>= (overlay-end ov) pos)))
	      jmail-search--flags-overlays))

(defun jmail-search--flags-render (flags)
  (let (flags-list)
    (dolist (flag flags)
      (cond ((eq flag 'unread)
	     (push jmail-search-unread flags-list))
	     ((eq flag 'flagged)
              (push jmail-search-flagged flags-list))
	     ((eq flag 'attach)
              (push jmail-search-attach flags-list))))
    flags-list))

(defun jmail-search--insert-flags (start flags)
  (when (cl-intersection flags (list 'unread 'flagged 'attach))
    (let* ((flags-list (jmail-search--flags-render flags))
	   (flags-str (concat " " (string-join flags-list " ")))
	   (overlay (make-overlay start start)))
      (overlay-put overlay 'invisible t)
      (overlay-put overlay 'before-string flags-str)
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
       (insert (format (jmail-search--fmt) date from (if thread thread "")))
       (setq subject-start (point))
       ;; insert flags
       (when jmail-search-show-flags
	 (jmail-search--insert-flags (point) flags))
       ;; insert subject
       (when (and jmail-search-bold-unread-message (member 'unread flags))
	 (setq subject (propertize subject 'face 'bold)))
       (insert " " subject)
       (setq object (append (list :subject-start subject-start) object))
       (add-text-properties (line-beginning-position) (line-end-position) object)
       (when (plist-get jmail-search--current :auto-fold-thread)
	 (jmail-search--fold-current-thread object))
       (insert "\n")))))

(defun jmail-search--update-flags ()
  (when-let ((start (jmail-search--subject-start)))
    (when-let ((overlay (jmail-search--find-flags start)))
      (setq jmail-search--flags-overlays (remove overlay jmail-search--flags-overlays))
      (delete-overlay overlay))
    (when-let ((flags (jmail-search--flags object)))
      (jmail-search--insert-flags start flags))))

(defun jmail-search--set-flag (flag)
  (when-let ((start (jmail-search--subject-start)))
    (jmail-search--insert-flags start (list flag))))

(defun jmail-search--set-property (prop value)
  (put-text-property (line-beginning-position)
		     (line-end-position)
		     prop value))

(defun jmail-search--rename-file (old-path flags)
  (let ((pattern "\\(.+\\)\\(?:cur\\|new\\|tmp\\)/\\(.+U=[0-9]+:2,\\).*")
	(new-path old-path))
    (when (string-match pattern old-path)
      (magit-bind-match-strings (dir file) old-path
	(setq new-path (concat dir (if (member 'new flags) "new/" "cur/") file))
	(when (member 'flagged flags)
	  (setq new-path (concat new-path "F")))
	(unless (member 'unread flags)
	  (setq new-path (concat new-path "S")))
	(unless (string= old-path new-path)
	  (rename-file old-path new-path))))
    new-path))

(defun jmail-search--unbold-subject ()
  (when-let ((start (jmail-search--subject-start)))
    (jmail-unbold-region start (line-end-position))))

(defun jmail-search--bold-subject ()
  (when-let ((start (jmail-search--subject-start)))
    (jmail-bold-region start (line-end-position))))

(defun jmail-search--mark-as-read (&optional refresh)
  (with-jmail-search-buffer
   (let* ((point (line-beginning-position))
	  (object (text-properties-at point))
	  (path (plist-get object :path))
	  (flags (plist-get object :flags))
	  new-path)
     (when path
       (when (member 'new flags)
	 (setq flags (remove 'new flags))
	 (jmail-search--set-property :flags flags))
       (when (member 'unread flags)
	 (setq flags (remove 'unread flags))
	 (jmail-search--set-property :flags flags)
	 (when jmail-search-show-flags
	   (jmail-search--update-flags))
	 (when jmail-search-bold-unread-message
	   (jmail-search--unbold-subject))
         (when refresh
           (jmail-refresh-all)))
       (setq new-path (jmail-search--rename-file path flags))
       (jmail-search--set-property :path new-path)))))

(defun jmail-search--mark-as-unread ()
  (with-jmail-search-buffer
   (let* ((point (line-beginning-position))
	  (object (text-properties-at point))
	  (path (plist-get object :path))
	  (flags (plist-get object :flags))
	  (new-path path))
     (unless (member 'unread flags)
       (push 'unread flags)
       (jmail-search--set-property :flags flags)
       (setq new-path (jmail-search--rename-file path flags))
       (jmail-search--set-property :path new-path)
       (when jmail-search-show-flags
	 (jmail-search--update-flags))
       (when jmail-search-bold-unread-message
	 (jmail-search--bold-subject))))))

(defun jmail-search--mark-as-flagged ()
  (with-jmail-search-buffer
   (let* ((point (line-beginning-position))
	  (object (text-properties-at point))
	  (path (plist-get object :path))
	  (flags (plist-get object :flags))
	  (new-path path))
     (unless (member 'flagged flags)
       (push 'flagged flags)
       (jmail-search--set-property :flags flags)
       (setq new-path (jmail-search--rename-file path flags))
       (jmail-search--set-property :path new-path)
       (when jmail-search-show-flags
	 (jmail-search--update-flags))))))

(defun jmail-search--mark-as-unflagged ()
  (with-jmail-search-buffer
   (let* ((point (line-beginning-position))
	  (object (text-properties-at point))
	  (path (plist-get object :path))
	  (flags (plist-get object :flags))
	  (new-path path))
     (when (member 'flagged flags)
       (setq flags (remove 'flagged flags))
       (jmail-search--set-property :flags flags)
       (when jmail-search-show-flags
	 (jmail-search--update-flags))
       (setq new-path (jmail-search--rename-file path flags))
       (jmail-search--set-property :path new-path)))))

(defun jmail-search--args (query thread related)
  (let ((option "find")
	(default-args (list "--reverse"
			    "--skip-dups"
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

(defun jmail-search--total-height ()
  (if-let* ((start (window-start (get-buffer-window (current-buffer))))
	    (end (point-max))
	    (lines (mapcar (lambda (ov)
			     (let ((ov-start (overlay-start ov))
				   (ov-end (overlay-end ov)))
			       (when (and (<= start ov-start)
					  (>= end ov-end))
				 (count-lines ov-start ov-end))))
			   jmail-search--fold-overlays)))
      (+ (window-total-height) (apply #'+ (delq nil lines)))
    (window-total-height)))

(defun jmail-search--display-saved-objects ()
  (with-jmail-search-buffer
   (while (and jmail-search--saved-objects
	       (< (count-lines (window-start) (point-max))
		  (jmail-search--total-height)))
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
	   (jmail-search--total-height))
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
	      (program (executable-find jmail-index-program))
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

(defun jmail-search--run (query thread auto-fold-thread related &optional save)
  (unless (get-buffer jmail-search--buffer-name)
    (with-current-buffer (get-buffer-create jmail-search--buffer-name)
      (jmail-search-mode)))
  (with-jmail-search-buffer
   (jmail-search--setup-env)
   (setq jmail-search--current `(:query ,query
				 :thread ,thread
				 :auto-fold-thread ,auto-fold-thread
				 :related ,related))
   (when save
     (push jmail-search--current jmail-search--saved))
   (switch-to-buffer (current-buffer))
   (jmail-search--process (jmail-search--args query thread related))))

(defun jmail-search--thread-level-at-point ()
  (with-jmail-search-buffer
   (when-let* ((object (text-properties-at (point)))
	       (thread (plist-get object :meta)))
       (plist-get thread :level))))

(defun jmail-search--goto-root-thread ()
  (goto-char (line-beginning-position))
  (when-let* ((object (text-properties-at (point)))
	      (thread (plist-get object :meta))
	      (level (plist-get thread :level)))
    (unless (zerop level)
      (previous-line)
      (jmail-search--goto-root-thread))))

(defmacro jmail-search--foreach-line-thread (&rest body)
  `(with-jmail-search-buffer
    (save-excursion
      (jmail-search--goto-root-thread)
      (pcase-let ((`(,start ,end) (jmail-search--thread-range)))
        (when (and start end)
          (goto-char start)
          (while (and (jmail-search--thread-level-at-point)
                      (< (point) end)
		      (not (jmail-search--thread-orphan-at-point))
		      (not (eobp)))
	    ,@body
	    (forward-line)))))))

(defun jmail-search--region-range ()
  (if (region-active-p)
    (let* ((beg-region (region-beginning))
	   (beg (save-excursion
		  (goto-char beg-region)
		  (line-beginning-position)))
	   (end-region (region-end))
	   (end (save-excursion
		  (goto-char end-region)
		  (line-end-position))))
      (list beg end))
    (list (line-beginning-position) (line-end-position))))

(defmacro jmail-search--foreach-line-region (&rest body)
  `(with-jmail-search-buffer
    (save-excursion
      (pcase-let ((`(,start ,end) (jmail-search--region-range)))
        (when (and start end)
          (goto-char start)
          (while (and (< (point) end) (not (eobp)))
	    ,@body
	    (forward-line)))))))

(defun jmail-search--thread-orphan-at-point ()
  (when-let* ((object (text-properties-at (point)))
	      (thread (plist-get object :meta)))
       (plist-get thread :orphan)))

(defun jmail-search--thread-range ()
  (when-let ((root-level (jmail-search--thread-level-at-point)))
    (let* ((start (line-beginning-position))
	   (end start))
      (save-excursion
	(forward-line)
	(while (and (jmail-search--thread-level-at-point)
		    (> (jmail-search--thread-level-at-point) root-level)
		    (not (jmail-search--thread-orphan-at-point))
		    (not (eobp)))
	  (setq end (line-end-position))
	  (forward-line)))
      (unless (= start end)
	(list start end)))))

(defun jmail-search--objects-from-thread ()
  (let ((objects))
    (jmail-search--foreach-line-thread
     (when-let ((object (text-properties-at (point))))
       (add-to-list 'objects object t)))
    objects))

(defun jmail-search--find-fold-overlay (start end)
  (cl-find-if (lambda (ov)
		(and (<= (overlay-start ov) start)
		     (>= (overlay-end ov) end)))
	      jmail-search--fold-overlays))

(defun jmail-search--remove-fold-overlay (overlay)
  (setq jmail-search--fold-overlays (remove overlay jmail-search--fold-overlays))
  (delete-overlay overlay))

(defun jmail-search--update-fold-overlay ()
  (when-let* ((overlay (jmail-search--find-fold-overlay (line-end-position)
							(line-end-position)))
	      (start (overlay-start overlay))
	      (end (overlay-end overlay)))
    (jmail-search--remove-fold-overlay overlay)
    (jmail-search--add-fold-overlay start end)))

(defun jmail-search--fold-prefix (start end subject)
  (let (prefix (unread 0) (total 0))
    (save-excursion
      (goto-char (- start 1))
      (while (< (point) end)
	(cl-incf total)
	(when-let ((props (text-properties-at (point)))
		   (flags (jmail-search--flags props)))
	  (when (member 'unread flags)
	    (cl-incf unread)))
	(forward-line))
      (when (and jmail-search-bold-unread-message
		 (> unread 0))
	(setq subject (propertize subject 'face 'bold)))
      (setq prefix (propertize (format " [%d/%d] " unread total) 'face
			       (if (> unread 0) 'error 'jmail-search-overlay-fold-face)))
      (concat prefix subject))))

(defun jmail-search--add-fold-overlay (start end)
  (save-excursion
    (goto-char (- start 1))
    (let* ((object (text-properties-at (point)))
	   (subject (plist-get object :subject))
	   (prefix (jmail-search--fold-prefix start end subject))
	   (overlay (make-overlay start end)))
      (add-to-list 'jmail-search--fold-overlays overlay)
      (overlay-put overlay 'invisible t)
      (overlay-put overlay 'before-string prefix))))

(defun jmail-search--fold-current-thread (object)
  (when-let ((thread (plist-get object :meta)))
    (let ((level (plist-get thread :level))
	  (orphan (plist-get thread :orphan))
	  (start (plist-get object :subject-start))
	  (end (line-end-position)))
      (goto-char start)
      (if (zerop level)
	  (when (plist-get thread :has-child)
	    (jmail-search--add-fold-overlay start end))
	(unless orphan
	  (previous-line)
	  (move-beginning-of-line 1)
	  (when-let* ((root-start (jmail-search--subject-start))
		      (overlay (jmail-search--find-fold-overlay root-start root-start)))
	    (jmail-search--remove-fold-overlay overlay)
	    (jmail-search--add-fold-overlay root-start end))))
      (goto-char end))))

(defun jmail-search--remove-overlay ()
  (when-let* ((start (jmail-search--subject-start))
	      (overlay (jmail-search--find-flags start)))
    (setq jmail-search--flags-overlays (remove overlay jmail-search--flags-overlays))
    (delete-overlay overlay)))

(defun jmail-search--remove-overlay-range (start end)
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (jmail-search--remove-overlay)
      (forward-line))))

(defun jmail-search--delete-region (start end)
  (with-jmail-search-buffer
   (delete-region start end)))

(defun jmail-search--delete-line ()
  (jmail-search--delete-region (line-beginning-position)
			       (+ (line-end-position) 1)))

(defun jmail-search--delete-message ()
  (when-let ((path (jmail-search--path)))
    (delete-file path)))

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
   (when-let* ((path (jmail-search--path))
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

(defun jmail-search-find-path (target-path)
  (with-jmail-search-buffer
   (catch 'found
     (save-excursion
       (goto-char (point-min))
       (while (not (eobp))
	 (when-let ((path (jmail-search--path)))
	   (if (string= path target-path)
	       (throw 'found (point))
	     (forward-line))))))))

(defun jmail-search-delete-at-point-or-region (confirm)
  (interactive (list (yes-or-no-p (if (region-active-p)
				      "Delete all messages from region: "
				    "Delete message: "))))
  (when confirm
    (let ((range (jmail-search--region-range)))
      (jmail-search--foreach-line-region (jmail-search--delete-message))
      (apply #'jmail-search--remove-overlay-range range)
      (apply #'jmail-search--delete-region range)
      ;; delete empty line
      (jmail-search--delete-region (point) (1+ (point)))
      (jmail-refresh-all))))

(defun jmail-search-delete-thread (confirm)
  (interactive (list (yes-or-no-p "Delete whole thread: ")))
  (when confirm
    (jmail-search--goto-root-thread)
    (when-let ((range (jmail-search--thread-range)))
      (jmail-search--foreach-line-thread (jmail-search--delete-message))
      (apply #'jmail-search--remove-overlay-range range)
      (apply #'jmail-search--delete-region range)
      ;; delete empty line
      (jmail-search--delete-region (point) (1+ (point)))
      (jmail-refresh-all))))

(defun jmail-search-mark-at-point-or-region (flag)
  (interactive (list (completing-read (if (region-active-p)
					  "Mark on region as: "
					"Mark at point as: ")
				      (mapcar #'car jmail-search-mark-flags))))
  (let ((flag-func (assoc-default flag jmail-search-mark-flags)))
    (jmail-search--foreach-line-region
     (funcall flag-func)
     (jmail-search--update-fold-overlay))
    (jmail-refresh-all)))

(defun jmail-search-mark-thread (flag)
  (interactive (list (completing-read "Mark whole thread as: "
				      (mapcar #'car jmail-search-mark-flags))))
  (jmail-search--foreach-line-thread
   (funcall (assoc-default flag jmail-search-mark-flags)))
  (jmail-search--update-fold-overlay)
  (jmail-refresh-all))

(defun jmail-search-move-at-point-or-region (maildir)
  (interactive (list (completing-read (if (region-active-p)
					  "Move region to: "
					"Move to: ")
				      (jmail-maildirs (jmail-get-top-maildir)))))
  (jmail-search--foreach-line-region
   (jmail-search--move-message (concat (jmail-get-top-maildir) maildir)))
  (jmail-refresh-all))

(defun jmail-search-move-thread (maildir)
  (interactive (list (completing-read "Move whole thread to: "
				      (jmail-maildirs (jmail-get-top-maildir)))))
  (jmail-search--foreach-line-thread
   (jmail-search--move-message (concat (jmail-get-top-maildir) maildir)))
  (jmail-refresh-all))

(defun jmail-search-toggle-thread ()
  (let ((query (plist-get jmail-search--current :query))
	(thread (not (plist-get jmail-search--current :thread)))
	(auto-fold-thread (plist-get jmail-search--current :auto-fold-thread))
	(related (plist-get jmail-search--current :related)))
    (jmail-search--run query thread auto-fold-thread related)))

(defun jmail-search-toggle-auto-fold-thread ()
  (let ((query (plist-get jmail-search--current :query))
	(thread (plist-get jmail-search--current :thread))
	(auto-fold-thread (not (plist-get jmail-search--current :auto-fold-thread)))
	(related (plist-get jmail-search--current :related)))
    (jmail-search--run query thread auto-fold-thread related)))

(defun jmail-search-toggle-related ()
  (let ((query (plist-get jmail-search--current :query))
	(thread (plist-get jmail-search--current :thread))
	(auto-fold-thread (plist-get jmail-search--current :auto-fold-thread))
	(related (not (plist-get jmail-search--current :related))))
    (jmail-search--run query thread auto-fold-thread related)))

(defun jmail-search-toggle-default-view ()
  (setq jmail-view-thread-default-view (not jmail-view-thread-default-view)))

(defun jmail-search-toggle-options ()
  (interactive)
  (with-jmail-search-buffer
   (let* ((thread (plist-get jmail-search--current :thread))
	  (auto-fold-thread (plist-get jmail-search--current :auto-fold-thread))
	  (related (plist-get jmail-search--current :related))
	  (targets `(("thread" . (,thread jmail-search-toggle-thread))
		     ("auto-fold-thread" .
		      (,auto-fold-thread jmail-search-toggle-auto-fold-thread))
		     ("related" . (,related jmail-search-toggle-related))
		     ("thread-view" . (,jmail-view-thread-default-view
				       jmail-search-toggle-default-view))))
	  (options (mapcar (lambda (target)
			     (propertize (car target) 'face
					 (if (cadr target) 'success 'error)))
			   targets))
	  (option (completing-read "Toggle option: " options))
	  (target (assoc-default option targets)))
     (funcall (cadr target)))))

(defun jmail-search-fold-unfold-thread ()
  (interactive)
  (pcase-let ((`(,_ ,end) (jmail-search--thread-range)))
    (let ((start (jmail-search--subject-start)))
      (when (and start end)
        (if-let ((overlay (jmail-search--find-fold-overlay start end)))
	    (jmail-search--remove-fold-overlay overlay)
	  (jmail-search--add-fold-overlay start end))))))

(defun jmail-search-fold-unfold-all-thread ()
  (interactive)
  (if jmail-search--fold-overlays
      (mapc #'jmail-search--remove-fold-overlay jmail-search--fold-overlays)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
	(jmail-search-fold-unfold-thread)
	(line-move 1)))))

(defun jmail-search-refresh ()
  (interactive)
  (let ((query (plist-get jmail-search--current :query))
	(thread (plist-get jmail-search--current :thread))
	(auto-fold-thread (plist-get jmail-search--current :auto-fold-thread))
	(related (plist-get jmail-search--current :related)))
    (jmail-search--run query thread auto-fold-thread related)))

(defun jmail-search-rerun ()
  (interactive)
  (let* ((query (plist-get jmail-search--current :query))
	 (query (jmail-read-prompt "Search: " jmail-search-fields query))
	 (thread (plist-get jmail-search--current :thread))
	 (auto-fold-thread (plist-get jmail-search--current :auto-fold-thread))
	 (related (plist-get jmail-search--current :related)))
    (jmail-search--run query thread auto-fold-thread related)))

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
   (let* ((object (text-properties-at (point)))
	  (thread (plist-get object :meta)))
     (if (and jmail-view-thread-default-view thread
	      (not (plist-get thread :orphan)))
	 (jmail-view-thread object (jmail-search--objects-from-thread)
			    (current-buffer))
       (jmail-search--mark-as-read t)
       (jmail-search--update-fold-overlay)
       (jmail-view (jmail-search--path) (current-buffer))))))

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
  (with-jmail-search-buffer (kill-buffer)))

(defun jmail-search-back-queries ()
  (interactive)
  (jmail-search-quit)
  (jmail-display-queries))

(defun jmail-search (query thread auto-fold-thread related)
  (setq jmail-search--saved-index 0)
  (jmail-search--run query thread auto-fold-thread related t))

(defun jmail-search-default (query)
  (jmail-search query
		(plist-get jmail-default-query-options :thread)
		(plist-get jmail-default-query-options :auto-fold-thread)
		(plist-get jmail-default-query-options :related)))

(provide 'jmail-search)
