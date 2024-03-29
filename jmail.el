;;; jmail.el --- XXXX

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

(require 'subr-x)
(require 'jmail-actions)
(require 'jmail-compose)
(require 'jmail-count)
(require 'jmail-index)
(require 'jmail-org-msg)
(require 'jmail-rss)
(require 'jmail-search)
(require 'jmail-sync)
(require 'jmail-utils)

(defgroup jmail nil
  "Mail reader for Emacs."
  :group 'mail)

;;; Mail User Agent
(define-mail-user-agent 'jmail-user-agent
  'jmail-compose
  'message-send-and-exit
  'message-kill-buffer
  'message-send-hook)

;;; Mode

(defvar jmail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a"      'jmail-actions-apply)

    (define-key map "g"      'jmail-refresh-at-point)
    (define-key map "G"      'jmail-refresh-all)

    (define-key map "f"      'jmail-sync-refresh-at-point)
    (define-key map "F"      'jmail-sync-refresh-all)
    (define-key map "C"      'jmail-cancel-sync-refresh)

    (define-key map "u"      'jmail-unread-at-point)
    (define-key map "U"      'jmail-unread-all)

    (define-key map "s"      'jmail-search-prompt-at-point)
    (define-key map "S"      'jmail-search-prompt)

    (define-key map "j"      'jmail-jump-to-maildir)
    (define-key map "n"      'jmail-next-query)
    (define-key map "p"      'jmail-previous-query)
    (define-key map "q"      'jmail-quit)
    (define-key map [down]   'jmail-next-query)
    (define-key map [return] 'jmail-enter)
    (define-key map [up]     'jmail-previous-query)

    (define-key map "R"      'jmail-rss-fetch-refresh-now)
    map)
  "Keymap for `jmail-mode'")

(define-derived-mode jmail-mode fundamental-mode
  "jmail"
  (jmail--update-header-line jmail--default-header)
  (jmail--insert-header)
  (jmail--fill-buffer)
  (setq buffer-read-only t))

;;; Faces

(defface jmail-queries-face
  '((t :inherit font-lock-type-face :bold t))
  "Face for a Queries header"
  :group 'jmail)

(defface jmail-section-face
  '((t :inherit font-lock-variable-name-face))
  "Face for a section"
  :group 'jmail)

;;; Customization

(defcustom jmail-top-maildir nil
  "Path to the top of the maildir"
  :type 'string
  :group 'jmail)

(defcustom jmail-queries '((:queries nil))
  "Queries displayed in menu"
  :group 'jmail)

(defcustom jmail-default-query-options
  '(:thread nil :auto-fold-thread nil :related nil)
  "Default options used by interactive functions:
- `jmail-unread-all'
- `jmail-search-prompt'
- `jmail-jump-to-maildir'"
  :group 'jmail)

(defcustom jmail-sync-refresh-every nil
  "If non nil, sync and refresh every X seconds"
  :type 'integer
  :group 'jmail)

(defcustom jmail-unread-count-hook nil
  "Functions called when counting unread message"
  :type 'hook
  :group 'jmail)

(defcustom jmail-quit-hook nil
  "Functions called after jmail quit"
  :type 'hook
  :group 'jmail)

(defcustom jmail-smtp-config-file nil
  "Path to the smtp config file"
  :type 'string
  :group 'jmail)

;;; Internal Variables

(defconst jmail--buffer-name "*jmail*")

(defconst jmail--default-header "Welcome to Jmail !")
(defconst jmail--index-ongoing (propertize "Index ongoing ..." 'face 'warning))
(defconst jmail--index-error (propertize "Index Failed !" 'face 'error))
(defconst jmail--sync-ongoing (propertize "Sync ongoing ..." 'face 'warning))
(defconst jmail--sync-error (propertize "Sync Failed !" 'face 'error))

(defvar jmail--sync-refresh-timer nil)

;;; Internal Functions

(defun jmail--insert-header ()
  (insert "\n")
  (insert (propertize "  Queries\n" 'face 'jmail-queries-face))
  (insert "\n"))

(defun jmail--insert-query (query)
  (insert (propertize (format "%-5s %-18s" " " query) 'face 'bold)))

(defun jmail--insert-group (group)
  (insert (propertize (format "    ▶ %s" (upcase group))
		      'face 'jmail-section-face)))

(defun jmail--insert-queries (queries)
  (dolist (query queries)
    (jmail--insert-query (plist-get query :name))
    (insert "(/)")
    (add-text-properties (line-beginning-position)
			 (line-end-position)
			 query)
    (insert "\n")))

(defun jmail--fill-buffer ()
  (let ((queries (car jmail-queries))
	(groups (cdr jmail-queries)))
    (jmail--insert-queries (plist-get queries :queries))
    (insert "\n")
    (dolist (group groups)
      (jmail--insert-group (plist-get group :name))
      (add-text-properties (line-beginning-position)
			   (line-end-position)
			   (append group (list :skip-counts t)))
      (insert "\n")
      (jmail--insert-queries (plist-get group :queries))
      (insert "\n"))))

(defmacro with-jmail-buffer (&rest body)
  `(when (get-buffer jmail--buffer-name)
     (with-current-buffer jmail--buffer-name
       (let ((inhibit-read-only t))
	 ,@body))))

(defun jmail--update-header-line (str)
  (with-jmail-buffer
   (setq header-line-format (concat "       " str))
   (force-mode-line-update)))

(defun jmail--setup ()
  (with-current-buffer (get-buffer-create jmail--buffer-name)
    (jmail-mode))
  (when jmail-sync-refresh-every
    (jmail--restart-sync-refresh-timer))
  (jmail-rss-setup)
  (jmail--goto-first-query))

(defun jmail--goto-query-name ()
  (goto-char (line-beginning-position))
  (when (re-search-forward "[[:alnum:]]" nil t)
    (backward-char)))

(defun jmail--move-to-query (forward)
  (with-jmail-buffer
   (let ((limit (if forward #'eobp #'bobp))
	 pos)
     (save-excursion
       (while (and (not pos) (not (funcall limit)))
	 (forward-line (if forward 1 -1))
	 (when-let ((props (text-properties-at (point))))
	   (when (plist-get props :query)
	     (jmail--goto-query-name)
	     (setq pos (point))))))
     (when pos (goto-char pos)))))

(defun jmail--goto-first-query ()
  (with-jmail-buffer
   (goto-char (point-min))
   (catch 'found
     (while (not (eobp))
       (when-let ((props (text-properties-at (point))))
	 (when (plist-get props :query)
	   (jmail--goto-query-name)
	   (throw 'found (point))))
       (forward-line)))))

(defmacro jmail--foreach-query (&rest body)
  (declare (indent 2))
  `(with-jmail-buffer
    (save-excursion
      (let (current-pos)
	(jmail--goto-first-query)
	,@body
	(setq current-pos (point))
	(jmail--move-to-query t)
	(while (/= current-pos (point))
	  ,@body
	  (setq current-pos (point))
	  (jmail--move-to-query t))))))

(defun jmail--count-total-handler (line count)
  (with-jmail-buffer
   (save-excursion
     (goto-line line)
     (goto-char (line-end-position))
     (when (re-search-backward "/.*)$" nil t)
       (replace-match (format "/%d)" count))))))

(defun jmail--count-unread-handler (line count)
  (with-jmail-buffer
   (save-excursion
     (goto-line line)
     (run-hook-with-args 'jmail-unread-count-hook
			 (text-properties-at (point))
			 count)
     (goto-char (line-end-position))
     (when (re-search-backward "(.*/\\(.*\\))$" nil t)
       (let ((beg (point)))
	 (replace-match (format "(%d/\\1)" count))
	 (if (> count 0)
	     (jmail-bold-region beg (point) 'jmail-bold-region-face)
	   (jmail-unbold-region beg (point))))))))

(defun jmail--sync-refresh ()
  (jmail--update-header-line jmail--sync-ongoing)
  (jmail-sync (jmail-cb (jmail-refresh-all))
              (jmail-cb (jmail--update-header-line jmail--sync-error))))

(defun jmail--stop-sync-refresh-timer ()
  (when jmail--sync-refresh-timer
    (cancel-timer jmail--sync-refresh-timer)))

(defun jmail--start-sync-refresh-timer ()
  (setq jmail--sync-refresh-timer
	(run-at-time 1 jmail-sync-refresh-every 'jmail--sync-refresh)))

(defun jmail--restart-sync-refresh-timer ()
  (jmail--stop-sync-refresh-timer)
  (jmail--start-sync-refresh-timer))

(defun jmail--maildir-name-list ()
  (let (maildir)
    (jmail--foreach-query
	(when-let* ((props (text-properties-at (point)))
		    (query (plist-get props :query)))
	  (when (and (string-match "maildir:/\\(.*\\)" query)
		     (not (plist-get props :skip-counts)))
	    (add-to-list 'maildir (match-string 1 query)))))
    maildir))

(defun jmail--check-programs ()
  (unless (executable-find jmail-index-program)
    (jmail-abort (concat "Please install " jmail-index-program)))
  (unless (executable-find jmail-sync-program)
    (jmail-abort (concat "Please install " jmail-sync-program))))

(defun jmail--check-env ()
  (unless jmail-top-maildir
    (jmail-abort "Please set `jmail-top-maildir'"))
  (unless jmail-sync-config-file
    (jmail-abort "Please set `jmail-sync-config-file'"))
  (unless jmail-smtp-config-file
    (jmail-abort "Please set `jmail-smtp-config-file'")))

;;; External Functions

(defun jmail-reset-queries ()
  (setq jmail-queries '((:queries nil))))

(defun jmail-add-query (&rest plist)
  (let ((queries (plist-get (car jmail-queries) :queries)))
    (setf (plist-get (car jmail-queries) :queries)
	  (append queries (list plist)))))

(defun jmail-add-group (&rest plist)
  (let ((groups (cdr jmail-queries))
	(group (append plist (list :queries nil))))
    (setf (cdr jmail-queries) (append groups (list group)))))

(defun jmail-add-query-to-group (group-name &rest plist)
  (catch 'found
    (dolist (group (cdr jmail-queries))
      (when (string= group-name (plist-get group :name))
	(let ((queries (plist-get group :queries)))
	  (setf (plist-get group :queries)
		(append queries (list plist)))
	  (throw 'found t))))))

(defun jmail-autofill-queries-from-top-maildir (&rest plist)
  (let* ((path (expand-file-name jmail-top-maildir))
	 (dirs (directory-files-recursively path "cur$" t))
  	 (subdirs (jmail--maildir-subdirs-assoc path dirs))
	 (thread (plist-get plist :thread))
	 (auto-fold-thread (plist-get plist :auto-fold-thread))
	 (related (plist-get plist :related)))
    (pcase-dolist (`(,group . ,queries) subdirs)
      (let ((all (mapconcat #'cdr queries " or ")))
	(jmail-add-group :name group
			 :query all
			 :thread thread
			 :auto-fold-thread auto-fold-thread
			 :related related)
	(dolist (query queries)
	  (jmail-add-query-to-group group
				    :name (car query)
				    :query (cdr query)
				    :thread thread
				    :auto-fold-thread auto-fold-thread
				    :related related))))))

(defun jmail-autofill-queries-from-maildir (maildir &rest plist)
  (let* ((path (expand-file-name (format "%s/%s" jmail-top-maildir maildir)))
	 (dirs (directory-files-recursively path "cur$" t))
         (queries (mapcar (lambda (dir)
                            (let* ((pattern (format "%s/\\(.*\\)/cur" path))
                                   (subdir (replace-regexp-in-string pattern "\\1" dir))
                                   (query (format "maildir:/%s/%s" maildir subdir)))
                              (cons subdir query)))
                          dirs))
         (all (mapconcat #'cdr queries " or "))
         (thread (plist-get plist :thread))
	 (auto-fold-thread (plist-get plist :auto-fold-thread))
	 (related (plist-get plist :related)))
      (jmail-add-group :name maildir
		       :query all
		       :thread thread
		       :auto-fold-thread auto-fold-thread
		       :related related)
	(dolist (query queries)
	  (jmail-add-query-to-group maildir
				    :name (car query)
				    :query (cdr query)
				    :thread thread
				    :auto-fold-thread auto-fold-thread
				    :related related))))

(defun jmail-count-at-point ()
  (when-let* ((props (text-properties-at (point)))
	      (query (plist-get props :query))
	      (query-unread (format "(%s) and flag:unread" query))
	      (line (line-number-at-pos))
	      (handler (apply-partially #'jmail--count-total-handler line))
	      (handler-unread (apply-partially #'jmail--count-unread-handler line)))
    (unless (plist-get props :skip-counts)
      (jmail-count-get query handler)
      (jmail-count-get query-unread handler-unread))))

(defun jmail-refresh-at-point ()
  (interactive)
  (jmail--update-header-line jmail--index-ongoing)
  (jmail-index (jmail-cb (jmail-count-at-point)
                         (jmail--update-header-line jmail--default-header))
               (jmail-cb (jmail--update-header-line jmail--index-error))))

(defun jmail-refresh-all ()
  (interactive)
  (jmail--update-header-line jmail--index-ongoing)
  (jmail-index (jmail-cb (jmail--foreach-query (jmail-count-at-point))
                         (jmail--update-header-line jmail--default-header))
               (jmail-cb (jmail--update-header-line jmail--index-error))))

(defun jmail-sync-refresh-at-point ()
  (interactive)
  (when-let* ((props (text-properties-at (point)))
	      (query (plist-get props :query))
	      (regexp "maildir:/\\([[:alnum:]-_]+/[[:alnum:]-_]+\\)")
	      (maildirs (delq nil (mapcar (lambda (query)
					    (when (string-match regexp query)
					      (match-string 1 query)))
					  (split-string query)))))
    (jmail--update-header-line jmail--sync-ongoing)
    (jmail-sync-maildirs maildirs
                         (jmail-cb (jmail-refresh-all))
                         (jmail-cb (jmail--update-header-line jmail--sync-error)))))

(defun jmail-sync-refresh-all ()
  (interactive)
  (if jmail-sync-refresh-every
      (jmail--restart-sync-refresh-timer)
    (jmail--sync-refresh)))

(defun jmail-cancel-sync-refresh ()
  (interactive)
  (jmail-sync-quit)
  (jmail-index-quit)
  (jmail-count-quit)
  (jmail--update-header-line jmail--default-header))

(defun jmail-unread-at-point ()
  (interactive)
  (when-let* ((props (text-properties-at (point)))
	      (query (plist-get props :query))
	      (query-unread (format "(%s) and flag:unread" query)))
    (jmail-search query-unread
		  (plist-get props :thread)
		  (plist-get props :auto-fold-thread)
		  (plist-get props :related))))

(defun jmail-unread-all ()
  (interactive)
  (jmail-search-default "flag:unread"))

(defun jmail-search-prompt-at-point (query)
  (interactive (list (jmail-read-prompt "Search here: " jmail-search-fields)))
  (when-let* ((props (text-properties-at (point)))
	      (query-at-point (plist-get props :query)))
    (jmail-search (format "(%s) and %s" query-at-point query)
		  (plist-get props :thread)
		  (plist-get props :auto-fold-thread)
		  (plist-get props :related))))

(defun jmail-search-prompt (query)
  (interactive (list (jmail-read-prompt "Search: " jmail-search-fields)))
  (jmail-search-default query))

(defun jmail-jump-to-maildir (query)
  (interactive (list (completing-read "Jump to: " (jmail--maildir-name-list))))
  (jmail-search-default (concat "maildir:/" query)))

(defun jmail-next-query ()
  (interactive)
  (jmail--move-to-query t))

(defun jmail-previous-query ()
  (interactive)
  (jmail--move-to-query nil))

(defun jmail-quit ()
  (interactive)
  (jmail-search-quit)
  (jmail-rss-quit)
  (jmail--stop-sync-refresh-timer)
  (jmail-cancel-sync-refresh)
  (with-jmail-buffer (kill-this-buffer))
  (run-hooks 'jmail-quit-hook))

(defun jmail-enter ()
  (interactive)
  (when-let* ((props (text-properties-at (point)))
	      (query (plist-get props :query)))
    (jmail-search query
		  (plist-get props :thread)
		  (plist-get props :auto-fold-thread)
		  (plist-get props :related))))

(defun jmail-display-queries ()
  (unless (get-buffer jmail--buffer-name)
    (jmail--setup))
  (jmail-switch-to-buffer jmail--buffer-name))

(defun jmail ()
  (interactive)
  (let ((default-directory jmail-top-maildir))
    (jmail--check-env)
    (jmail--check-programs)
    (jmail-index-check)
    (jmail-display-queries)
    (jmail-refresh-all)))

(provide 'jmail)
