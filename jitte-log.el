;;; jitte-log.el --- Revision history  -*- lexical-binding:t -*-

;; Copyright (C) 2025 Jitte Contributors

;; Author: Jitte Contributors
;; Maintainer: Jitte Contributors

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements the log buffer which shows revision history
;; for a jj repository.

;;; Code:

(require 'jitte-core)
(require 'jitte-jj)
(require 'jitte-mode)

;;; Options

(defgroup jitte-log nil
  "Inspect and manipulate commit history."
  :group 'jitte-modes)

(defcustom jitte-log-mode-hook nil
  "Hook run after entering Jitte-Log mode."
  :group 'jitte-log
  :type 'hook)

(defcustom jitte-log-default-arguments
  '("--graph")
  "Default arguments for jj log."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-log
  :type '(repeat string))

(defcustom jitte-log-section-commit-count 256
  "How many commits to show in certain log sections.
These sections are `recent commits', `unpushed to <remote>'
and `unpulled from <remote>'."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-log
  :type 'integer)

(defcustom jitte-log-auto-more nil
  "Insert more log entries automatically when moving past the last entry.
Only considered when moving past the last entry with
`jitte-next-line' or `jitte-previous-line'."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-log
  :type 'boolean)

(defcustom jitte-log-margin-width nil
  "Width of the margin used to show dates and authors in log buffers."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-log
  :type '(choice (const :tag "Use default" nil)
                 (integer :tag "Characters")))

(defcustom jitte-log-show-refname-after-summary nil
  "Whether to show refnames after the commit summary.
This is useful if you use very long branch names."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-log
  :type 'boolean)

;;; Mode

(defvar jitte-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map jitte-mode-map)
    (define-key map "j" #'jitte-log-move-to-revision)
    (define-key map "=" #'jitte-log-toggle-commit-limit)
    (define-key map "+" #'jitte-log-double-commit-limit)
    (define-key map "-" #'jitte-log-half-commit-limit)
    map)
  "Keymap for `jitte-log-mode'.")

(define-derived-mode jitte-log-mode jitte-mode "Jitte Log"
  "Mode for browsing jj commit history.

\\{jitte-log-mode-map}"
  :group 'jitte-log
  (hack-dir-local-variables-non-file-buffer)
  (setq truncate-lines t)
  (run-hooks 'jitte-log-mode-hook))

;;; Setup

(defun jitte-log-setup-buffer (&optional revset args files)
  "Setup and return the log buffer for REVSET with ARGS and FILES."
  (let ((buffer (jitte-setup-buffer 'jitte-log-mode)))
    (with-current-buffer buffer
      (setq jitte-buffer-revisions revset)
      (setq jitte-buffer-log-args args)
      (setq jitte-buffer-log-files files)
      (jitte-log-refresh-buffer))
    (jitte-display-buffer buffer)
    buffer))

;;;###autoload
(defun jitte-log (&optional revset args files)
  "Show log for REVSET with ARGS and FILES."
  (interactive)
  (let ((revset (or revset "@-"))
        (args (or args jitte-log-default-arguments)))
    (jitte-log-setup-buffer revset args files)))

;;;###autoload
(defun jitte-log-current (&optional args files)
  "Show log for current revision with ARGS and FILES."
  (interactive)
  (jitte-log "@" args files))

;;;###autoload
(defun jitte-log-all (&optional args files)
  "Show log for all revisions with ARGS and FILES."
  (interactive)
  (jitte-log "all()" args files))

;;;###autoload
(defun jitte-log-oneline (&optional revset args)
  "Show oneline log for REVSET with ARGS."
  (interactive)
  (let ((args (append args '("-T" "oneline"))))
    (jitte-log revset args)))

;;; Refresh

(defun jitte-log-refresh-buffer ()
  "Refresh the current log buffer."
  (when (derived-mode-p 'jitte-log-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (jitte-insert-log-buffer-contents))))

(defun jitte-insert-log-buffer-contents ()
  "Insert the contents of the log buffer."
  (magit-insert-section (log)
    (jitte-insert-log-headers)
    (jitte-insert-log-entries)))

(defun jitte-insert-log-headers ()
  "Insert log headers."
  (magit-insert-section (headers)
    (magit-insert-heading "Log")
    (when jitte-buffer-revisions
      (insert (format "Revset: %s\n" jitte-buffer-revisions)))
    (when jitte-buffer-log-args
      (insert (format "Arguments: %s\n" 
                       (mapconcat #'identity jitte-buffer-log-args " "))))
    (insert "\n")))

(defun jitte-insert-log-entries ()
  "Insert log entries."
  (let* ((revset (or jitte-buffer-revisions "@-"))
         (args (or jitte-buffer-log-args jitte-log-default-arguments))
         (files jitte-buffer-log-files)
         (output (jitte-get-log-output revset args files)))
    (when output
      (jitte-wash-log output))))

(defun jitte-get-log-output (revset args files)
  "Get log output for REVSET with ARGS and FILES."
  (let ((cmd-args (append (list "log")
                          (when revset (list "-r" revset))
                          args
                          files)))
    (ignore-errors (apply #'jitte-run-jj-sync cmd-args))))

(defun jitte-wash-log (output)
  "Wash log OUTPUT into sections."
  (let ((lines (split-string output "\n")))
    (dolist (line lines)
      (unless (string-empty-p line)
        (jitte-insert-log-line line)))))

(defun jitte-insert-log-line (line)
  "Insert a log LINE."
  (cond
   ((string-match "^\\([a-f0-9]\\{12\\}\\) \\(.*\\)$" line)
    (let ((commit-id (match-string 1 line))
          (summary (match-string 2 line)))
      (jitte-insert-commit-line commit-id summary)))
   (t
    (insert line "\n"))))

(defun jitte-insert-commit-line (commit-id summary)
  "Insert a commit line for COMMIT-ID with SUMMARY."
  (magit-insert-section (commit commit-id)
    (insert (propertize commit-id 'font-lock-face 'jitte-hash))
    (insert " ")
    (insert summary)
    (insert "\n")))

;;; Navigation

(defun jitte-log-move-to-revision ()
  "Move to a specific revision."
  (interactive)
  (let ((revision (read-string "Move to revision: ")))
    (goto-char (point-min))
    (when (search-forward revision nil t)
      (beginning-of-line))))

;;; Limit Management

(defvar jitte-log-commit-limit 256
  "Current commit limit for log buffer.")

(defun jitte-log-toggle-commit-limit ()
  "Toggle between limited and unlimited commits."
  (interactive)
  (setq jitte-log-commit-limit
        (if (eq jitte-log-commit-limit 'unlimited)
            jitte-log-section-commit-count
          'unlimited))
  (message "Commit limit: %s" jitte-log-commit-limit)
  (jitte-log-refresh-buffer))

(defun jitte-log-double-commit-limit ()
  "Double the commit limit."
  (interactive)
  (when (numberp jitte-log-commit-limit)
    (setq jitte-log-commit-limit (* 2 jitte-log-commit-limit))
    (message "Commit limit: %d" jitte-log-commit-limit)
    (jitte-log-refresh-buffer)))

(defun jitte-log-half-commit-limit ()
  "Halve the commit limit."
  (interactive)
  (when (and (numberp jitte-log-commit-limit)
             (> jitte-log-commit-limit 1))
    (setq jitte-log-commit-limit (/ jitte-log-commit-limit 2))
    (message "Commit limit: %d" jitte-log-commit-limit)
    (jitte-log-refresh-buffer)))

;;; Commands

(defun jitte-show-commit (&optional revision)
  "Show commit REVISION."
  (interactive)
  (let ((revision (or revision
                      (jitte-section-value-if 'commit)
                      (read-string "Show revision: "))))
    (when revision
      (jitte-diff-setup-buffer revision))))

(defun jitte-log-cherry-pick ()
  "Cherry-pick the commit at point."
  (interactive)
  (when-let ((commit (jitte-section-value-if 'commit)))
    (when (y-or-n-p (format "Cherry-pick %s? " commit))
      (jitte-run-jj-async "new" commit)
      (jitte-refresh))))

(defun jitte-log-edit ()
  "Edit the commit at point."
  (interactive)
  (when-let ((commit (jitte-section-value-if 'commit)))
    (jitte-edit commit)))

(defun jitte-log-describe ()
  "Describe the commit at point."
  (interactive)
  (when-let ((commit (jitte-section-value-if 'commit)))
    (let ((message (read-string "Description: ")))
      (when (not (string-empty-p message))
        (jitte-run-jj-async "describe" "-r" commit "-m" message)
        (jitte-refresh)))))

;;; Utilities

(defun jitte-section-value-if (type)
  "Return the value of the current section if it is of TYPE."
  (when-let ((section (magit-current-section)))
    (when (eq (magit-section-type section) type)
      (magit-section-value section))))

(defun jitte-log-format-margin ()
  "Format margin for log entries."
  (when jitte-log-margin-width
    (format "%%%ds" jitte-log-margin-width)))

;;; Revset Support

(defun jitte-read-revset (prompt &optional default)
  "Read a revset from the user with PROMPT and optional DEFAULT."
  (let ((history 'jitte-revset-history))
    (read-string prompt default history)))

(defvar jitte-revset-history nil
  "History for revset inputs.")

;;; Log Templates

(defun jitte-log-format-template ()
  "Return the template for log formatting."
  (concat "change_id.short() \" \" "
          "if(description, description.first_line(), \"(no description)\") "
          "if(bookmarks, \" (\" ++ bookmarks.join(\", \") ++ \")\")"))

;;; Integration Commands

(defun jitte-log-bookmark (&optional bookmark)
  "Show log for BOOKMARK."
  (interactive)
  (let ((bookmark (or bookmark
                      (jitte-read-bookmark "Show log for bookmark: "))))
    (when bookmark
      (jitte-log bookmark))))

(defun jitte-log-file (&optional file)
  "Show log for FILE."
  (interactive)
  (let ((file (or file
                  (read-file-name "Show log for file: " nil nil t))))
    (when file
      (jitte-log nil nil (list file)))))

(defun jitte-log-revset (&optional revset)
  "Show log for REVSET."
  (interactive)
  (let ((revset (or revset
                    (jitte-read-revset "Show log for revset: "))))
    (when revset
      (jitte-log revset))))

(provide 'jitte-log)

;;; jitte-log.el ends here