;;; jmail-sync.el --- Synchronize emails

;; Copyright (C) 2022 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jmail
;; Created: 2022-10-14

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

(defcustom jmail-sync-config-file nil
  "Path to the config file used by `jmail-sync-program'"
  :type 'string
  :group 'jmail)

(defcustom jmail-sync-ignore-channels nil
  "If non nil, list of channels to ignore when syncing mails"
  :type 'list
  :group 'jmail)

;;; External Variables

(defconst jmail-sync-program "mbsync")

;;; Internal Variables

(defconst jmail-sync--process "jmail-sync")

(defconst jmail-sync--buffer "*jmail-sync*")

;;; Internal Functions

(defun jmail-sync--config-data ()
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
		((string-match "^Far :.*:\"\\(.+\\)\"" line)
		 (setq maildir (format "%s/%s" path (match-string 1 line))))
		((string-match "^Near :.*:\\(.*\\)" line)
		 (setq slave (match-string 1 line))
		 (when (string-match "[[:alnum:]-_]+" slave)
		   (setq maildir (format "%s/%s" path slave)))
		 (add-to-list 'data (cons maildir channel) t)))
	  (forward-line))))
    data))

(defun jmail-sync--get-args (channels)
  (when-let ((args (cl-set-difference channels jmail-sync-ignore-channels
				      :test #'string=))
	     (config (list "--config" jmail-sync-config-file)))
    (append config args)))

(defun jmail-sync--process-filter (process str)
  (when-let ((buffer (process-buffer process)))
    (when (buffer-live-p (process-buffer process))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert (string-join (split-string str "") "\n"))))))

(defun jmail-sync--process-sentinel (success error process status)
  (let ((default-directory jmail-top-maildir))
    (if (zerop (process-exit-status process))
        (when success (jmail-funcall success))
      (when error (jmail-funcall error)))
    (kill-buffer (process-buffer process))))

(defun jmail-sync--run (success error args)
  (let* ((default-directory jmail-top-maildir)
	 (program (executable-find jmail-sync-program))
	 (buffer (get-buffer-create jmail-sync--buffer))
	 (process (apply 'start-file-process jmail-sync--process buffer program args)))
    (with-current-buffer buffer
      (erase-buffer))
    (set-process-filter process 'jmail-sync--process-filter)
    (set-process-sentinel process (apply-partially #'jmail-sync--process-sentinel
                                                   success error))))

;;; External Functions

(defun jmail-sync-quit ()
  (jmail-terminate-process-buffer jmail-sync--buffer))

(defun jmail-sync (&optional success error)
  (unless (get-buffer-process jmail-sync--buffer)
    (when-let* ((all-channels (mapcar #'cdr (jmail-sync--config-data)))
		(args (jmail-sync--get-args all-channels)))
      (jmail-sync--run success error args))))

(defun jmail-sync-maildirs (maildirs &optional success error)
  (unless (get-buffer-process jmail-sync--buffer)
    (when-let* ((config-data (jmail-sync--config-data))
		(channels (delq nil (mapcar (lambda (maildir)
					      (assoc-default maildir config-data))
					    maildirs)))
		(args (jmail-sync--get-args channels)))
      (jmail-sync--run success error args))))

(provide 'jmail-sync)
