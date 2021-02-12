;;; jmail-update.el --- XXXX

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

;;; Customization

(defcustom jmail-update-ignore-channels nil
  "If non nil, list of channels to ignore when fetching mails"
  :type 'list
  :group 'jmail)

;;; External Variables

(defconst jmail-update-process-name "jmail-update")

;;; Internal Variables

(defconst jmail-update--buffer-name "*jmail-update*")

(defvar jmail-update--success-cb nil)

(defvar jmail-update--error-cb nil)

;;; Internal Functions

(defun jmail-update--process-filter (process str)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert (string-join (split-string str "") "\n"))))

(defun jmail-update--reset-env ()
  (setq jmail-update--success-cb nil)
  (setq jmail-update--error-cb nil))

(defmacro when-jmail-update-process-success (process &rest body)
  (declare (indent 2))
  `(let ((default-directory jmail-top-maildir))
     (if (and (zerop (process-exit-status ,process))
  	      (buffer-live-p (process-buffer ,process)))
	 (progn ,@body)
       (if (eq (process-status ,process) 'exit)
	   (jmail-funcall jmail-update--error-cb)
	 (kill-buffer (process-buffer process)))
       (jmail-update--reset-env))))

;; index
(defun jmail-update--index-process-sentinel (process status)
  (when-jmail-update-process-success process
      (jmail-funcall jmail-update--success-cb)
      (kill-buffer (process-buffer process))
      (jmail-update--reset-env)))

(defun jmail-update--index ()
  (let* ((default-directory jmail-top-maildir)
	 (program (jmail-find-program jmail-index-program))
	 (args (list "index" "--nocolor"))
	 (buffer (get-buffer-create jmail-update--buffer-name))
	 (process (apply 'start-file-process jmail-update-process-name
			 buffer program args)))
    (set-process-filter process 'jmail-update--process-filter)
    (set-process-sentinel process 'jmail-update--index-process-sentinel)))

;; sync
(defun jmail-update--sync-process-sentinel (process status)
  (when-jmail-update-process-success process
      (jmail-update--index)))

(defun jmail-update--config-data ()
  (let (data path channel maildir slave)
    (with-temp-buffer
      (insert-file-contents jmail-sync-config-file)
      (goto-char (point-min))
      (while (not (eobp))
	(let ((line (buffer-substring-no-properties (line-beginning-position)
						    (line-end-position))))
	  (cond ((string-match "^Path \\(.+\\)" line)
		 (setq path (file-name-nondirectory (directory-file-name
						     (match-string 1 line)))))
		((string-match "^Channel \\(.+\\)" line)
		 (setq channel (match-string 1 line)))
		((string-match "^Master :.*:\"\\(.+\\)\"" line)
		 (setq maildir (format "%s/%s" path (match-string 1 line))))
		((string-match "^Slave :.*:\\(.*\\)" line)
		 (setq slave (match-string 1 line))
		 (when (string-match "[[:alnum:]-_]+" slave)
		   (setq maildir (format "%s/%s" path slave)))
		 (add-to-list 'data (cons maildir channel) t)))
	  (forward-line))))
    data))

(defun jmail-update--get-sync-args (channels)
  (when-let ((args (cl-set-difference channels jmail-update-ignore-channels
				      :test #'string=))
	     (config (list "--config" (jmail-untramp-path jmail-sync-config-file))))
    (append config args)))

(defun jmail-update--sync (success error args)
  (let* ((default-directory jmail-top-maildir)
	 (program (jmail-find-program jmail-sync-program))
	 (buffer (get-buffer-create jmail-update--buffer-name))
	 (process (apply 'start-file-process jmail-update-process-name
			 buffer program args)))
    (with-current-buffer buffer
      (erase-buffer))
    (set-process-filter process 'jmail-update--process-filter)
    (set-process-sentinel process 'jmail-update--sync-process-sentinel)))

;; init
(defun jmail-update--check-database (program)
  (zerop (process-file program nil nil nil "info")))

(defun jmail-update--init-database (program)
  (let* ((maildir (concat "--maildir=" (jmail-untramp-path jmail-top-maildir)))
	 (exit-status (process-file program nil nil nil "init" maildir)))
    (unless (zerop exit-status)
      (jmail-abort "Failed to init database"))))

;;; External Functions

(defun jmail-update-check-database ()
  (let* ((default-directory jmail-top-maildir)
	 (program (jmail-find-program jmail-index-program)))
    (unless (jmail-update--check-database program)
      (jmail-update--init-database program))))

(defun jmail-update-quit ()
  (jmail-terminate-process-buffer jmail-update--buffer-name))

(defun jmail-update (success error &optional skip-sync)
  (unless (get-buffer-process jmail-update--buffer-name)
    (setq jmail-update--success-cb success)
    (setq jmail-update--error-cb error)
    (if skip-sync
	(jmail-update--index)
      (when-let* ((all-channels (mapcar #'cdr (jmail-update--config-data)))
		  (args (jmail-update--get-sync-args all-channels)))
	(jmail-update--sync success error args)))))

(defun jmail-update-maildirs (maildirs success error)
  (unless (get-buffer-process jmail-update--buffer-name)
    (when-let* ((config-data (jmail-update--config-data))
		(channels (delq nil (mapcar (lambda (maildir)
					      (assoc-default maildir config-data))
					    maildirs)))
		(args (jmail-update--get-sync-args channels)))
      (jmail-update--sync success error args))))

(provide 'jmail-update)
