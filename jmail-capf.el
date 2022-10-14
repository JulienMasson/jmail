;;; jmail-capf.el --- Completion at point function

;; Copyright (C) 2022 Julien Masson.

;; Author: Julien Masson
;; URL: https://github.com/JulienMasson/jmail
;; Created: 2022-08-27

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

;;; Internal Functions

(defun jmail-capf--candidate-at-line ()
  (when-let ((str (buffer-substring (line-beginning-position)
				    (line-end-position)))
	     (mail-regexp "\\([[:graph:]]*\\)@\\([[:graph:]]*\\)$"))
    (cond ((string-match (concat "\\(.*\\) " mail-regexp) str)
	   (format "%s <%s@%s>" (match-string 1 str)
		   (match-string 2 str)
		   (match-string 3 str)))
	  ((string-match mail-regexp str)
	   (format "<%s@%s>" (match-string 1 str)
		   (match-string 2 str))))))

(defun jmail-capf--candidates (arg)
  (let ((args (list "cfind" "--nocolor" arg))
	candidates)
    (with-temp-buffer
      (apply #'process-file "mu" nil (current-buffer) nil args)
      (goto-char (point-min))
      (while (not (eobp))
	(add-to-list 'candidates (jmail-capf--candidate-at-line))
	(forward-line)))
    candidates))

(defun jmail-capf--expand-name ()
  (let ((beg (save-excursion
               (skip-chars-backward "^\n:,") (skip-chars-forward " \t")
               (point)))
        (end (save-excursion
               (skip-chars-forward "^\n,") (skip-chars-backward " \t")
               (point))))
    (when (< beg end)
      (let ((candidates (jmail-capf--candidates (buffer-substring beg end))))
        (list beg end `(lambda (string pred action)
                         (pcase action
                           ('metadata '(metadata (category . email)))
                           ('lambda t)
                           ('nil (try-completion string ,candidates))
                           ('t (all-completions "" ',candidates pred)))))))))

(defun jmail-capf--in-header ()
  (let ((mail-abbrev-mode-regexp message-email-recipient-header-regexp))
    (mail-abbrev-in-expansion-header-p)))

;;; External Functions

(defun jmail-completion-at-point ()
  (when (jmail-capf--in-header)
    (jmail-capf--expand-name)))

(defun jmail-capf-setup ()
  (add-hook 'completion-at-point-functions #'jmail-completion-at-point nil t))

(provide 'jmail-capf)
