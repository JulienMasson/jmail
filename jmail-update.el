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

(require 'jmail-rss)

;;; Customization

(defcustom jmail-update--ignore-channels nil
  "If non nil, list of channels to ignore when fetching mails"
  :type 'list
  :group 'jmail)

;;; External Variables

(defconst jmail-update-process-name "jmail-update")

;;; Internal Variables

(defconst jmail-update--buffer-name "*jmail-update*")

(defvar jmail-update--ongoing nil)

(defvar jmail-update--success-cb nil)

(defvar jmail-update--error-cb nil)

;;; Internal Functions

(defun jmail-update--process-filter (process str)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert (mapconcat 'identity (split-string str "") "\n"))))

(defun jmail-update--reset-env ()
  (setq jmail-update--ongoing nil)
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
	 (maildir (concat "--maildir=" (jmail-untramp-path jmail-top-maildir)))
	 (args (list "index" "--nocolor" maildir))
	 (buffer (get-buffer jmail-update--buffer-name))
	 (process (apply 'start-file-process jmail-update-process-name
			 buffer program args)))
    (set-process-filter process 'jmail-update--process-filter)
    (set-process-sentinel process 'jmail-update--index-process-sentinel)))

;; sync
(defun jmail-update--sync-process-sentinel (process status)
  (when-jmail-update-process-success process
      (if (jmail-rss-enabled)
	  (jmail-rss-fetch jmail-update--buffer-name #'jmail-update--index)
	(jmail-update--index))))

(defun jmail-update--get-channels ()
  (let (channels)
    (with-current-buffer (find-file-noselect jmail-sync-config-file)
      (goto-char (point-min))
      (while (re-search-forward "^Channel " nil t)
	(add-to-list 'channels
		     (buffer-substring (point) (line-end-position)) t))
      (kill-buffer))
    channels))

(defun jmail-update--get-sync-args ()
  (when-let* ((all-channels (jmail-update--get-channels))
	      (channels (cl-set-difference all-channels
					   jmail-update--ignore-channels
					   :test #'string=)))
    (append (list "--config" (jmail-untramp-path jmail-sync-config-file))
	    channels)))

(defun jmail-update--sync ()
  (let* ((default-directory jmail-top-maildir)
	 (program (jmail-find-program jmail-sync-program))
	 (args (jmail-update--get-sync-args))
	 (buffer (get-buffer-create jmail-update--buffer-name))
	 (process (apply 'start-file-process jmail-update-process-name
			 buffer program args)))
    (with-current-buffer buffer
      (erase-buffer))
    (set-process-filter process 'jmail-update--process-filter)
    (set-process-sentinel process 'jmail-update--sync-process-sentinel)))

(defun jmail-update--running ()
  (and jmail-update--ongoing
       (get-buffer-process jmail-update--buffer-name)))

;;; External Functions

(defun jmail-update-quit ()
  (jmail-terminate-process-buffer jmail-update--buffer-name))

(defun jmail-update (success error)
  (interactive)
  (unless (jmail-update--running)
    (setq jmail-update--ongoing t)
    (setq jmail-update--success-cb success)
    (setq jmail-update--error-cb error)
    (jmail-update--sync)))

(provide 'jmail-update)
