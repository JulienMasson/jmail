;;; jmail-count.el --- XXXX

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

;;; Internal Variables

(defconst jmail-count--buffer-name "*jmail-count*")

(defvar jmail-count--queues nil)

(defvar jmail-count--current nil)

;;; Internal Functions

(defun jmail-count--program (query)
  (format "%s find --fields p %s | wc -l"
	  (jmail-find-program jmail-index-program)
	  (shell-quote-argument query)))

(defun jmail-count--call-cb (count)
  (when jmail-count--current
    (when-let ((cb (cadr jmail-count--current)))
      (jmail-funcall cb count))))

(defun jmail-count--number (buffer)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (string-to-number (buffer-substring (line-beginning-position)
					  (line-end-position))))))

(defun jmail-count--process-sentinel (process status)
  (when (eq (process-status process) 'exit)
    (if (and (eq (process-exit-status process) 0)
	     (buffer-live-p (process-buffer process)))
	(jmail-count--call-cb (jmail-count--number (process-buffer process)))
      (jmail-count--call-cb 0)))
  (if jmail-count--queues
      (apply #'jmail-count--process (pop jmail-count--queues))
    (setq jmail-count--current nil)
    (kill-buffer jmail-count--buffer-name)))

(defun jmail-count--process (query cb)
  (when-let* ((default-directory jmail-top-maildir)
	      (program (jmail-count--program query))
	      (buffer (get-buffer-create jmail-count--buffer-name))
	      (process (start-file-process-shell-command
			"jmail-count" buffer program)))
    (with-current-buffer buffer
      (erase-buffer))
    (setq jmail-count--current (list query cb))
    (set-process-filter process 'jmail-process-filter)
    (set-process-sentinel process 'jmail-count--process-sentinel)))

;;; External Functions

(defun jmail-count-quit ()
  (setq jmail-count--queues nil)
  (setq jmail-count--current nil)
  (jmail-terminate-process-buffer jmail-count--buffer-name))

(defun jmail-count-get (query cb)
  (if jmail-count--current
      (add-to-list 'jmail-count--queues (list query cb) t)
    (jmail-count--process query cb)))

(provide 'jmail-count)
