;;; jmail-compose.el --- XXXX

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

(require 'jmail-capf)
(require 'jmail-font-lock)

;;; Mode

(defvar jmail-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a") 'jmail-compose-attach-dired-files)
    map)
  "Keymap for `jmail-compose-mode'")

(define-derived-mode jmail-compose-mode message-mode
  "jmail compose"
  (setq-local font-lock-defaults '(jmail-font-lock t)))

;;; Customization

(defcustom jmail-compose-hook nil
  "Functions called when composing message"
  :type 'hook
  :group 'jmail)

;;; Internal Functions

(defun jmail-compose--account-list ()
  (when-let ((accounts (jmail-get-accounts jmail-smtp-config-file)))
    (mapcar #'car accounts)))

;;; External Functions

(defun jmail-set-account (account)
  (interactive (list (completing-read "Use account: " (jmail-compose--account-list))))
  (let* ((accounts (jmail-get-accounts jmail-smtp-config-file))
	 (from (assoc-default account accounts))
	 (from-email (plist-get from :email)))
    (save-excursion
      (message-goto-from)
      (message-beginning-of-line)
      (delete-region (point) (line-end-position))
      (insert (jmail-make-address-str from))
      (jmail-compose-set-extra-arguments account from-email))))

(defun jmail-compose-attach-dired-files ()
  (interactive)
  (mapc #'mml-attach-file (dired-get-all-marked)))

(defun jmail-compose-set-extra-arguments (account email)
  (setq-local message-sendmail-extra-arguments
	      (list (concat "--file=" jmail-smtp-config-file)
		    (concat "--account=" account)
		    (concat "--user=" email))))

(defun jmail-compose-setup-send-mail ()
  (setq-local send-mail-function 'message-send-mail-with-sendmail)
  (setq-local message-send-mail-function 'message-send-mail-with-sendmail)
  (setq-local sendmail-program (executable-find "msmtp")))

(defun jmail-compose (account)
  (interactive (list (completing-read "Compose with: " (jmail-compose--account-list))))
  (let* ((accounts (jmail-get-accounts jmail-smtp-config-file))
	 (from (assoc-default account accounts))
	 (from-email (plist-get from :email))
	 (buffer (message-buffer-name "mail")))
    (with-current-buffer (get-buffer-create buffer)
      (message-setup `((From . ,(jmail-make-address-str from))
		       (To . "")
		       (Subject . "")))
      (message-sort-headers)
      (message-hide-headers)
      (set-buffer-modified-p nil)
      (jmail-compose-mode)
      (run-hooks 'jmail-compose-hook)
      (jmail-capf-setup)
      (jmail-compose-setup-send-mail)
      (jmail-compose-set-extra-arguments account from-email)
      (message-goto-body)
      (jmail-switch-to-buffer (current-buffer)))))

(provide 'jmail-compose)
