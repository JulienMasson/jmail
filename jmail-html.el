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

(require 'xwidget)

;;; Mode

(defvar jmail-html-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'jmail-html-view-quit)
    map)
  "Keymap for `jmail-html-view-mode'")

(define-derived-mode jmail-html-view-mode xwidget-webkit-mode "jmail html view")

;;; Internal Variables

(defvar-local jmail-html-view--dir nil)

(defvar jmail-html-view-buffer "*jmail-html-view*")

;;; Internal Functions

(defun jmail-html--xwidget-webkit-callback (xwidget xwidget-event-type)
  (when (and (buffer-live-p (xwidget-buffer xwidget))
	     (eq xwidget-event-type 'decide-policy))
    (let ((url (nth 3 last-input-event)))
      (when (string-match "^http.*" url)
	(browse-url url)))))

(defun jmail-html--new-session (dir url)
  (with-current-buffer (get-buffer-create jmail-html-view-buffer)
    (setq xwidget-webkit-last-session-buffer (current-buffer))
    (insert (propertize url 'invisible t))
    (let* ((width (xwidget-window-inside-pixel-width (selected-window)))
	   (height (xwidget-window-inside-pixel-height (selected-window)))
	   (xw (xwidget-insert (point-min) 'webkit (buffer-name) width height)))
      (xwidget-put xw 'callback #'jmail-html--xwidget-webkit-callback)
      (jmail-html-view-mode)
      (xwidget-webkit-goto-uri xw url)
      (setq jmail-html-view--dir dir)
      (switch-to-buffer (current-buffer)))))

(defun jmail-html--remove-dir ()
  (when-let ((buffer (get-buffer jmail-html-view-buffer)))
    (with-current-buffer buffer
      (when (and jmail-html-view--dir (file-exists-p jmail-html-view--dir))
	(delete-directory jmail-html-view--dir t)))))

;;; External Functions

(defun jmail-html-view-quit ()
  (interactive)
  (jmail-html--remove-dir)
  (let ((kill-buffer-query-functions nil))
    (kill-current-buffer))
  (xwidget-delete-zombies))

(defun jmail-html-open (file)
  (if-let ((dir (file-name-directory file))
	   (url (concat "file://" file))
	   (xwidget (xwidget-webkit-current-session)))
      (xwidget-webkit-goto-uri xwidget url)
    (jmail-html--new-session dir url)))

(provide 'jmail-html)
