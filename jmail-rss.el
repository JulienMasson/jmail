;;; jmail-rss.el --- XXXX

;; Copyright (C) 2019 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jmail
;; Created: 2019-10-29

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

(require 'json)

;;; Customization

(defcustom jmail-rss-config-file nil
  "Config file used by `jmail-rss-program'"
  :type 'string
  :group 'jmail)

(defcustom jmail-rss-fetch-refresh-every nil
  "If non nil, fetch RSS news and refresh every X seconds"
  :type 'integer
  :group 'jmail)

;;; External Variables

(defconst jmail-rss-program "feed2exec")

(defconst jmail-rss-process-name "jmail-rss")

;;; Internal Variables

(defconst jmail-rss--buffer-name "*jmail-rss*")

(defvar jmail-rss--fetch-refresh-timer nil)

(defvar jmail-rss--quit-ongoing nil)

;;; Internal Functions

(defun jmail-rss--get-args (cmd)
  (let ((args (list "--verbose" cmd)))
    (if jmail-rss-config-file
	(append (list "--config" jmail-rss-config-file) args)
      args)))

(defun jmail-rss--get-folders ()
  (let ((args (jmail-rss--get-args "ls"))
	folders)
    (with-temp-buffer
      (apply 'process-file jmail-rss-program nil
	     (current-buffer) nil args)
      (goto-char (point-min))
      (while (not (eobp))
	(when-let ((object (ignore-errors (json-read))))
	  (push object folders))
	(end-of-defun)))
    (mapcar (lambda (elem)
	      (let ((mailbox (file-name-as-directory
			      (assoc-default 'mailbox elem)))
		    (folder (assoc-default 'folder elem)))
		(file-name-as-directory (concat mailbox folder))))
	    folders)))

(defun jmail-rss--get-count (dir)
  (let* ((hostname (getenv "HOSTNAME"))
	 (regexp (format ".*%s,U=\\([0-9]+\\):2," hostname))
	 (last-file (car (last (directory-files dir)))))
    (if (string-match regexp last-file)
	(string-to-number (match-string 1 last-file))
      0)))

(defun jmail-rss--rename-file (file count)
  (let* ((hostname (getenv "HOSTNAME"))
	 (regexp (concat ".*" hostname "$")))
    (when (string-match regexp file)
      (rename-file file (format "%s,U=%d:2," file count)))))

(defun jmail-rss--rename-new-entries ()
  (let* ((default-directory jmail-top-maildir)
	 (folders (jmail-rss--get-folders)))
    (mapc (lambda (folder)
	    (let* ((cur-dir (concat folder "cur/"))
		   (new-dir (concat folder "new/"))
		   (count (max (jmail-rss--get-count cur-dir)
			       (jmail-rss--get-count new-dir)))
		   (new-files (directory-files new-dir t "^[^.]")))
	      (mapc (lambda (file)
		      (setq count (+ count 1))
		      (jmail-rss--rename-file file count))
		    new-files)))
	  folders)))

(defun jmail-rss--process-sentinel (process status)
  (when-let ((buffer (process-buffer process)))
    (if (or (zerop (process-exit-status process))
	    jmail-rss--quit-ongoing)
	(progn
	  (kill-buffer buffer)
	  (unless jmail-rss--quit-ongoing
	    (jmail-rss--rename-new-entries)
	    (jmail-fetch-refresh-all t)))
      (pop-to-buffer buffer)))
  (setq jmail-rss--quit-ongoing nil))

(defun jmail-rss--stop-fetch-refresh-timer ()
  (when jmail-rss--fetch-refresh-timer
    (cancel-timer jmail-rss--fetch-refresh-timer)))

(defun jmail-rss--start-fetch-refresh-timer ()
  (setq jmail-rss--fetch-refresh-timer (run-at-time 1 jmail-rss-fetch-refresh-every
						    'jmail-rss-fetch-refresh)))

(defun jmail-rss--restart-fetch-refresh-timer ()
  (jmail-rss--stop-fetch-refresh-timer)
  (jmail-rss--start-fetch-refresh-timer))

;;; External Functions

(defun jmail-rss-quit ()
  (when-let ((process (get-buffer-process jmail-rss--buffer-name)))
    (setq jmail-rss--quit-ongoing t)
    (delete-process process))
  (jmail-rss--stop-fetch-refresh-timer))

(defun jmail-rss-setup ()
  (when (and jmail-rss-fetch-refresh-every (executable-find jmail-rss-program))
    (jmail-rss--restart-fetch-refresh-timer)))

(defun jmail-rss-fetch-refresh ()
  (unless (get-buffer-process jmail-rss--buffer-name)
    (let* ((default-directory jmail-top-maildir)
	   (program (executable-find jmail-rss-program))
	   (args (jmail-rss--get-args "fetch"))
	   (buffer (get-buffer-create jmail-rss--buffer-name))
	   (process (apply 'start-file-process jmail-rss-process-name
			   buffer program args)))
      (with-current-buffer buffer
	(erase-buffer))
      (set-process-filter process 'jmail-process-filter)
      (set-process-sentinel process 'jmail-rss--process-sentinel))))

(defun jmail-rss-fetch-refresh-now ()
  (interactive)
  (if jmail-rss-fetch-refresh-every
      (jmail-rss--restart-fetch-refresh-timer)
    (jmail-rss-fetch-refresh)))

(provide 'jmail-rss)
