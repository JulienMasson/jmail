;;; jmail.el --- XXXX

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

(require 'subr-x)
(require 'jmail-compose)
(require 'jmail-count)
(require 'jmail-nntp)
(require 'jmail-org-msg)
(require 'jmail-rss)
(require 'jmail-search)
(require 'jmail-update)
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
    (define-key map "g"      'jmail-refresh-at-point)
    (define-key map "G"      'jmail-refresh-all)

    (define-key map "f"      'jmail-fetch-refresh-at-point)
    (define-key map "F"      'jmail-fetch-refresh-all)

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
    (define-key map "N"      'jmail-nntp)
    map)
  "Keymap for `jmail-mode'")

(define-derived-mode jmail-mode fundamental-mode
  "jmail"
  (jmail--update-header-line jmail--default-header)
  (jmail--insert-header)
  (jmail--insert-queries)
  (toggle-read-only t))

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

(defcustom jmail-queries nil
  "Queries displayed in menu"
  :group 'jmail)

(defcustom jmail-fetch-refresh-every nil
  "If non nil, fetch and refresh every X seconds"
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

(defcustom jmail-sync-config-file nil
  "Path to the config file used by `jmail-sync-program'"
  :type 'string
  :group 'jmail)

(defcustom jmail-smtp-config-file nil
  "Path to the smtp config file"
  :type 'string
  :group 'jmail)

;;; External Variables

(defconst jmail-index-program "mu")

(defconst jmail-sync-program "mbsync")

;;; Internal Variables

(defconst jmail--buffer-name "*jmail*")

(defconst jmail--unknown-count "(/)")

(defconst jmail--default-header "Welcome to Jmail !")
(defconst jmail--fetch-ongoing (propertize "Fetch ongoing ..." 'face 'warning))
(defconst jmail--fetch-error (propertize "Fetch Failed !" 'face 'error))

(defvar jmail--fetch-refresh-timer nil)

;;; Internal Functions

(defun jmail--insert-header ()
  (insert "\n")
  (insert (propertize "  Queries\n" 'face 'jmail-queries-face))
  (insert "\n"))

(defun jmail--insert-query (query)
  (insert (propertize (format "%-5s %-18s" " " query) 'face 'bold)))

(defun jmail--insert-section (section queries)
  (let ((beg (point))
	(queries-all (mapconcat (lambda (query)
				  (format "(%s)" (cdr query)))
				queries " or ")))
    (insert (propertize (format "    ▶ %s" (upcase section))
			'face 'jmail-section-face))
    (put-text-property beg (point) 'jmail-section queries-all)
    (insert "\n")))

(defun jmail--insert-queries ()
  (mapc (lambda (data)
	  (let ((section (car data))
		(queries (cdr data)))
	    (when section
	      (jmail--insert-section section queries))
	    (mapc (lambda (query)
		    (let ((beg (point)))
		      (jmail--insert-query (car query))
		      (insert jmail--unknown-count)
		      (put-text-property beg (point) 'jmail (cdr query))
		      (insert "\n")))
		  queries)
	    (insert "\n")))
	jmail-queries))

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
  (when jmail-fetch-refresh-every
    (jmail--restart-fetch-refresh-timer))
  (jmail-rss-setup)
  (jmail--goto-first-query))

(defun jmail--get-query (pos)
  (or (get-text-property pos 'jmail-section)
      (get-text-property pos 'jmail)))

(defun jmail--move-to-query (forward)
  (with-jmail-buffer
   (let* ((pos (jmail-find-alphanumeric-character (point) forward))
	  (prop (if pos (jmail--get-query pos))))
     (while (and pos (not prop))
       (save-excursion
	 (goto-char pos)
	 (setq pos (jmail-find-alphanumeric-character (point) forward))
	 (when pos
	   (setq prop (jmail--get-query pos)))))
     (when (and pos prop)
       (goto-char pos)))))

(defun jmail--goto-first-query ()
  (with-jmail-buffer
   (goto-char (point-min))
   (jmail-next-query)))

(defun jmail--line-first-query ()
  (with-jmail-buffer
   (save-excursion
     (jmail--goto-first-query)
     (line-number-at-pos))))

(defun jmail--goto-last-query ()
  (with-jmail-buffer
   (goto-char (point-max))
   (jmail-previous-query)))

(defun jmail--line-last-query ()
  (with-jmail-buffer
   (save-excursion
     (jmail--goto-last-query)
     (line-number-at-pos))))

(defmacro jmail--foreach-query (line query &rest body)
  (declare (indent 2))
  `(with-jmail-buffer
    (save-excursion
      (when-let ((first (jmail--line-first-query))
		 (last (jmail--line-last-query)))
	(goto-line first)
	(setq ,line first)
	(setq ,query (get-text-property (point) 'jmail))
	(when ,query
	  ,@body)
	(while (not (= ,line last))
	  (jmail-next-query)
	  (setq ,line (line-number-at-pos))
	  (setq ,query (get-text-property (point) 'jmail))
	  (when ,query
	    ,@body))))))

(defun jmail--count-total-handler (count data)
  (with-jmail-buffer
   (save-excursion
     (goto-line data)
     (end-of-line)
     (when (re-search-backward "/.*)$" nil t)
       (replace-match (format "/%d)" count))))))

(defun jmail--count-unread-handler (count data)
  (with-jmail-buffer
   (save-excursion
     (goto-line data)
     (run-hook-with-args 'jmail-unread-count-hook
     			 (get-text-property (point) 'jmail)
     			 count)
     (end-of-line)
     (when (re-search-backward "(.*/\\(.*\\))$" nil t)
       (let ((beg (point)))
	 (replace-match (format "(%d/\\1)" count))
	 (if (> count 0)
	     (jmail-bold-region beg (point) 'jmail-bold-region-face)
	   (jmail-unbold-region beg (point))))))))

(defun jmail--fetch-success-cb ()
  (jmail--update-header-line jmail--default-header)
  (jmail-refresh-all))

(defun jmail--fetch-error-cb ()
  (jmail--update-header-line jmail--fetch-error))

(defun jmail--start-fetch-refresh (&optional skip-sync)
  (jmail--update-header-line jmail--fetch-ongoing)
  (jmail-update #'jmail--fetch-success-cb
		#'jmail--fetch-error-cb
		skip-sync))

(defun jmail--stop-fetch-refresh-timer ()
  (when jmail--fetch-refresh-timer
    (cancel-timer jmail--fetch-refresh-timer)))

(defun jmail--start-fetch-refresh-timer ()
  (setq jmail--fetch-refresh-timer
	(run-at-time 1 jmail-fetch-refresh-every 'jmail--start-fetch-refresh)))

(defun jmail--restart-fetch-refresh-timer ()
  (jmail--stop-fetch-refresh-timer)
  (jmail--start-fetch-refresh-timer))

(defun jmail--maildir-name-list ()
  (let (maildir)
    (jmail--foreach-query line query
      (when (string-match "maildir:/\\(.*\\)" query)
	(add-to-list 'maildir (match-string 1 query))))
    maildir))

(defun jmail--check-programs ()
  (unless (jmail-find-program jmail-index-program)
    (jmail-abort (concat "Please install " jmail-index-program)))
  (unless (jmail-find-program jmail-sync-program)
    (jmail-abort (concat "Please install " jmail-sync-program))))

(defun jmail--check-env ()
  (unless jmail-top-maildir
    (jmail-abort "Please set `jmail-top-maildir'"))
  (when jmail-sync-config-file
    (unless (jmail-common-host jmail-top-maildir jmail-sync-config-file)
      (jmail-abort "`jmail-top-maildir' and `jmail-sync-config-file' doesn't have common host")))
  (unless jmail-smtp-config-file
    (jmail-abort "Please set `jmail-smtp-config-file'"))
  (unless jmail-queries
    (jmail-abort "Please set `jmail-queries'"))
  (jmail-update-check-database))

;;; External Functions

(defun jmail-refresh-at-point ()
  (interactive)
  (when-let ((query (jmail--get-query (point)))
	     (line (line-number-at-pos)))
    (jmail-count-get query #'jmail--count-total-handler line)
    (jmail-count-get (format "(%s) and flag:unread" query)
    		     #'jmail--count-unread-handler line)))

(defun jmail-refresh-all ()
  (interactive)
  (jmail--foreach-query line query
    (jmail-count-get query #'jmail--count-total-handler line)
    (jmail-count-get (format "(%s) and flag:unread" query)
    		     #'jmail--count-unread-handler line)))

(defun jmail-fetch-refresh-at-point ()
  (interactive)
  (when-let* ((query (jmail--get-query (point)))
	      (regexp "maildir:/\\([[:alnum:]-_]+/[[:alnum:]-_]+\\)")
	      (maildirs (delq nil (mapcar (lambda (query)
					    (when (string-match regexp query)
					      (match-string 1 query)))
					  (split-string query)))))
    (jmail--update-header-line jmail--fetch-ongoing)
    (jmail-update-maildirs maildirs #'jmail--fetch-success-cb
			   #'jmail--fetch-error-cb)))

(defun jmail-fetch-refresh-all (&optional skip-sync)
  (interactive)
  (if (and jmail-fetch-refresh-every (not skip-sync))
      (jmail--restart-fetch-refresh-timer)
    (jmail--start-fetch-refresh skip-sync)))

(defun jmail-unread-at-point ()
  (interactive)
  (when-let* ((query (jmail--get-query (point)))
	      (unread (format "(%s) and flag:unread" query)))
    (jmail-search unread)))

(defun jmail-unread-all ()
  (interactive)
  (jmail-search "flag:unread"))

(defun jmail-search-prompt-at-point (query)
  (interactive (list (jmail-read-prompt "Search here: " jmail-search-fields)))
  (when-let ((query-at-point (jmail--get-query (point))))
    (jmail-search (format "(%s) and %s" query-at-point query))))

(defun jmail-search-prompt (query)
  (interactive (list (jmail-read-prompt "Search: " jmail-search-fields)))
  (jmail-search query))

(defun jmail-jump-to-maildir (query)
  (interactive (list (completing-read "Jump to: "
				      (jmail--maildir-name-list))))
  (jmail-search (concat "maildir:/" query)))

(defun jmail-next-query ()
  (interactive)
  (jmail--move-to-query t))

(defun jmail-previous-query ()
  (interactive)
  (jmail--move-to-query nil))

(defun jmail-quit ()
  (interactive)
  (jmail-rss-quit)
  (jmail-search-quit)
  (jmail--stop-fetch-refresh-timer)
  (jmail-update-quit)
  (jmail-count-quit)
  (with-jmail-buffer (kill-this-buffer))
  (run-hooks 'jmail-quit-hook))

(defun jmail-enter ()
  (interactive)
  (when-let ((query (jmail--get-query (point))))
    (jmail-search query)))

(defun jmail ()
  (interactive)
  (jmail--check-env)
  (let ((default-directory jmail-top-maildir))
    (jmail--check-programs)
    (unless (get-buffer jmail--buffer-name)
      (jmail--setup))
    (jmail-refresh-all)
    (jmail-switch-to-buffer jmail--buffer-name)))

(provide 'jmail)
