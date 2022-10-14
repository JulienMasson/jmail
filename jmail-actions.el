;;; jmail-actions.el --- XXXX

;; Copyright (C) 2020 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jmail
;; Created: 2020-01-14

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

(defcustom jmail-actions '(("patch"        . jmail-apply-patch)
			   ("patch-series" . jmail-apply-patch-series)
                           ("fetch-mbox"   . jmail-fetch-mbox))
  "Alist of actions to apply in `jmail-search-mode'"
  :type 'alist
  :group 'jmail)

(defcustom jmail-actions-projects nil
  "Alist of (name . path) projects"
  :type 'alist
  :group 'jmail)

;;; Actions

(defun jmail-patch-prompt-dir (prompt)
  (let* ((names (mapcar #'car jmail-actions-projects))
         (collection (append names (list "interactive")))
         (target (completing-read prompt collection)))
    (if (string= target "interactive")
        (read-directory-name prompt)
      (assoc-default target jmail-actions-projects))))

(defun jmail-apply-patch ()
  (interactive)
  (with-jmail-search-buffer
   (when-let* ((dir (jmail-patch-prompt-dir "Apply patch: "))
               (object (text-properties-at (point)))
	       (msg (plist-get object :path))
	       (subject (plist-get object :subject))
	       (default-directory dir))
     (when (string-match "^\\[.*PATCH" subject)
       (shell-command (concat "git am " msg))))))

(defun jmail-apply-patch-series ()
  (interactive)
  (when-let ((dir (jmail-patch-prompt-dir "Apply patch series: ")))
    (jmail-search--foreach-line-thread
     (when-let* ((object (text-properties-at (point)))
	         (thread (plist-get object :meta))
	         (level (plist-get thread :level))
	         (msg (plist-get object :path))
	         (subject (plist-get object :subject))
	         (default-directory dir))
       (when (and (string-match "^\\[.*PATCH " subject) (= level 1))
         (shell-command (concat "git am " msg)))))))

(defun jmail-fetch-mbox (message-id)
  (interactive "sFetch message-id: ")
  (let* ((default-directory (temporary-file-directory))
         (maildir (completing-read "Save to: " (jmail--maildir-name-list)))
         (mbox "jmail.mbox")
         (url (format "https://lore.kernel.org/all/%s/t.mbox.gz" message-id))
         (fetch (format "wget -q %s -O %s.gz" url mbox))
         (extract (format "gunzip -q %s.gz" mbox))
         (convert (format "mb2md -s %s -d %s/%s" (concat default-directory mbox)
                          (expand-file-name jmail-top-maildir) maildir))
         (remove (concat "rm " mbox))
         (cmds (list fetch extract convert remove)))
    (shell-command (string-join cmds " && "))
    (message (concat "mbox saved to " (propertize maildir 'face 'success)))
    (jmail-refresh-all)))

;;; External Functions

(defun jmail-actions-apply (action)
  (interactive (list (completing-read "Apply action: " (mapcar #'car jmail-actions))))
  (call-interactively (assoc-default action jmail-actions)))

(provide 'jmail-actions)
