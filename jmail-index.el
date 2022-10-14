;;; jmail-index.el --- Index emails

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

;;; External Variables

(defconst jmail-index-program "mu")

;;; Internal Variables

(defconst jmail-index--process "jmail-index")

(defconst jmail-index--buffer "*jmail-index*")

(defvar jmail-index-checked nil)

;;; Internal Functions

(defun jmail-index--init ()
  (let* ((maildir (concat "--maildir=" jmail-top-maildir)))
    (unless (zerop (process-file jmail-index-program nil nil nil "init" maildir))
      (jmail-abort "Failed to init database"))))

(defun jmail-index--process-filter (process str)
  (when-let ((buffer (process-buffer process)))
    (when (buffer-live-p (process-buffer process))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert (string-join (split-string str "") "\n"))))))

(defun jmail-index--process-sentinel (success error process status)
  (let ((default-directory jmail-top-maildir))
    (if (zerop (process-exit-status process))
        (when success (jmail-funcall success))
      (when error (jmail-funcall error)))
    (kill-buffer (process-buffer process))))

;;; External Functions

(defun jmail-index-quit ()
  (jmail-terminate-process-buffer jmail-index--buffer))

(defun jmail-index-check ()
  (unless jmail-index-checked
    (unless (zerop (process-file jmail-index-program nil nil nil "info"))
      (jmail-index--init))
    (setq jmail-index-checked t)))

(defun jmail-index (&optional success error)
  (let* ((default-directory jmail-top-maildir)
	 (program (executable-find jmail-index-program))
	 (args (list "index" "--nocolor"))
	 (buffer (get-buffer-create jmail-index--buffer))
	 (process (apply 'start-file-process jmail-index--process
			 buffer program args)))
    (set-process-filter process 'jmail-index--process-filter)
    (set-process-sentinel process (apply-partially #'jmail-index--process-sentinel
                                                   success error))))

(provide 'jmail-index)
