;;; jmail-html.el --- XXXX

;; Copyright (C) 2021 Julien Masson.

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

(require 'webkit)

;;; Internal Variables

(defvar-local jmail-html-view--dir nil)

;;; Internal Functions

(defun jmail-html--remove-dir ()
  (when (and jmail-html-view--dir (file-exists-p jmail-html-view--dir))
    (delete-directory jmail-html-view--dir t)))

;;; External Functions

(defun jmail-html-view-quit ()
  (interactive)
  (jmail-html--remove-dir)
  (let ((kill-buffer-query-functions nil))
    (kill-current-buffer)))

(defun jmail-html-open (file)
  (when-let ((dir (file-name-directory file))
	     (url (concat "file://" file)))
    (browse-url url)))
    ;; (with-current-buffer (webkit-browse-url url)
    ;;   (define-key webkit-mode-map "q" 'jmail-html-view-quit)
    ;;   (setq jmail-html-view--dir dir))))

(provide 'jmail-html)
