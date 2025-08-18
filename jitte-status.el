;;; jitte-status.el --- Working copy overview  -*- lexical-binding:t -*-

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

;; This library implements the status buffer which provides an overview
;; of the working copy in a jj repository.

;;; Code:

(require 'jitte-core)
(require 'jitte-jj)
(require 'jitte-mode)
(require 'jitte-process)

;;; Options

(defgroup jitte-status nil
  "Inspect and manipulate jj repositories."
  :group 'jitte-modes)

(defcustom jitte-status-mode-hook nil
  "Hook run after entering Jitte-Status mode."
  :group 'jitte-status
  :type 'hook)

(defcustom jitte-status-headers-hook
  (list #'jitte-insert-error-header
        #'jitte-insert-working-copy-header
        #'jitte-insert-bookmarks-header)
  "Hook run to insert headers into the status buffer.

This hook is run by `jitte-insert-status-headers', which in turn
has to be a member of `jitte-status-sections-hook' to be used at
all."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-status
  :type 'hook
  :options (list #'jitte-insert-error-header
                 #'jitte-insert-working-copy-header
                 #'jitte-insert-bookmarks-header))

(defcustom jitte-status-sections-hook
  (list #'jitte-insert-status-headers
        #'jitte-insert-working-copy-changes
        #'jitte-insert-recent-changes)
  "Hook run to insert sections into a status buffer."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-status
  :type 'hook)

(defcustom jitte-status-initial-section '(1)
  "The section point is placed on when a status buffer is created.

When such a buffer is merely being refreshed or being shown again
after it was merely buried, then this option has no effect.

If this is nil, then point remains on the very first section as
usual.  Otherwise it has to be a list of integers and section
identity lists.  The members of that list are tried in order
until a matching section is found."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-status
  :type '(choice (const :tag "Top" nil)
                 (repeat (choice (number :tag "Section number")
                                 (sexp :tag "Section identity")))))

;;; Mode

(defvar jitte-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map jitte-mode-map)
    (define-key map "j" #'jitte-status-jump)
    (define-key map "n" #'jitte-new)
    (define-key map "e" #'jitte-edit)
    (define-key map "d" #'jitte-describe)
    (define-key map "s" #'jitte-split)
    (define-key map "S" #'jitte-squash)
    map)
  "Keymap for `jitte-status-mode'.")

(define-derived-mode jitte-status-mode jitte-mode "Jitte"
  "Mode for working copy status of jj repositories.

\\{jitte-status-mode-map}"
  :group 'jitte-status
  (hack-dir-local-variables-non-file-buffer)
  (setq jitte-buffer-revision "@")
  (add-hook 'post-command-hook #'jitte-status-maybe-update-revision-buffer nil t)
  (run-hooks 'jitte-status-mode-hook))

;;; Setup

(defun jitte-status-setup-buffer (&optional directory)
  "Setup and return the status buffer for DIRECTORY."
  (let ((default-directory (or directory (jitte-toplevel))))
    (unless (jitte-inside-repository-p)
      (user-error "Not in a jj repository"))
    (let ((buffer (jitte-setup-buffer 'jitte-status-mode t)))
      (jitte-display-buffer buffer)
      buffer)))

;;;###autoload
(defun jitte-status (&optional directory)
  "Show the working tree status for the current jj repository.
If optional DIRECTORY is provided, use that directory instead."
  (interactive
   (list (and current-prefix-arg
              (jitte-read-repository))))
  (jitte-status-setup-buffer directory))

;;; Refresh

(defun jitte-refresh-buffer ()
  "Refresh the current status buffer."
  (when (derived-mode-p 'jitte-status-mode)
    (let ((inhibit-read-only t)
          (section-line (count-lines (point-min) (point))))
      (erase-buffer)
      (jitte-insert-status-buffer-contents)
      (goto-char (point-min))
      (forward-line (min section-line (count-lines (point-min) (point-max)))))))

(defun jitte-insert-status-buffer-contents ()
  "Insert the contents of the status buffer."
  (magit-insert-section (status)
    (run-hooks 'jitte-status-sections-hook)))

;;; Headers

(defun jitte-insert-status-headers ()
  "Insert status headers."
  (magit-insert-section (headers)
    (magit-insert-heading "Status")
    (run-hooks 'jitte-status-headers-hook)
    (insert "\n")))

(defun jitte-insert-error-header ()
  "Insert error header if there are any errors."
  (when jitte-this-error
    (magit-insert-section (error jitte-this-error)
      (insert (propertize "Error: " 'font-lock-face 'error))
      (insert jitte-this-error "\n"))))

(defun jitte-insert-working-copy-header ()
  "Insert working copy information header."
  (magit-insert-section (working-copy)
    (let ((change-id (ignore-errors (jitte-current-working-copy-change-id)))
          (commit-id (ignore-errors (jitte-current-working-copy-commit-id))))
      (insert (format "Working copy: %s"
                      (or change-id "unknown")))
      (when commit-id
        (insert (format " (%s)" (substring commit-id 0 12))))
      (insert "\n"))))

(defun jitte-insert-bookmarks-header ()
  "Insert bookmarks header."
  (when-let ((bookmarks (ignore-errors (jitte-bookmark-list))))
    (magit-insert-section (bookmarks)
      (insert "Bookmarks: ")
      (insert (mapconcat #'identity bookmarks ", "))
      (insert "\n"))))

;;; Sections

(defun jitte-insert-working-copy-changes ()
  "Insert working copy changes section."
  (let ((files (ignore-errors (jitte-status-files))))
    (when files
      (magit-insert-section (working-copy-changes)
        (magit-insert-heading "Working copy changes")
        (dolist (file files)
          (jitte-insert-status-file file))
        (insert "\n")))))

(defun jitte-insert-status-file (file)
  "Insert FILE in the status buffer."
  (magit-insert-section (file file)
    (insert (format "  %s\n" file))))

(defun jitte-insert-recent-changes ()
  "Insert recent changes section."
  (let ((log-output (ignore-errors (jitte-log-oneline "@-" 10))))
    (when (and log-output (not (string-empty-p log-output)))
      (magit-insert-section (recent-changes)
        (magit-insert-heading "Recent changes")
        (dolist (line (split-string log-output "\n" t))
          (when (not (string-empty-p line))
            (jitte-insert-log-line line)))
        (insert "\n")))))

(defun jitte-insert-log-line (line)
  "Insert LOG line in the status buffer."
  (magit-insert-section (commit line)
    (insert (format "  %s\n" line))))

;;; Navigation

(defun jitte-status-jump ()
  "Jump to a section in the status buffer."
  (interactive)
  (let ((sections '(("Working copy changes" . working-copy-changes)
                    ("Recent changes" . recent-changes))))
    (when-let ((choice (completing-read "Jump to: " sections)))
      (when-let ((section-type (cdr (assoc choice sections))))
        (jitte-status-goto-section section-type)))))

(defun jitte-status-goto-section (section-type)
  "Go to section of SECTION-TYPE."
  (goto-char (point-min))
  (when-let ((section (magit-get-section `(,section-type))))
    (goto-char (magit-section-start section))))

;;; Commands

(defun jitte-new ()
  "Create a new working copy change."
  (interactive)
  (when (y-or-n-p "Create new working copy change? ")
    (jitte-run-jj-async "new")
    (jitte-refresh)))

(defun jitte-edit ()
  "Edit a revision."
  (interactive)
  (let ((revision (read-string "Edit revision: " "@")))
    (jitte-run-jj-async "edit" revision)
    (jitte-refresh)))

(defun jitte-describe ()
  "Describe the current working copy change."
  (interactive)
  (let ((message (read-string "Description: ")))
    (when (not (string-empty-p message))
      (jitte-run-jj-async "describe" "-m" message)
      (jitte-refresh))))

(defun jitte-split ()
  "Split the current working copy change."
  (interactive)
  (when (y-or-n-p "Split current change? ")
    (jitte-run-jj-async "split")
    (jitte-refresh)))

(defun jitte-squash ()
  "Squash changes into parent."
  (interactive)
  (when (y-or-n-p "Squash changes into parent? ")
    (jitte-run-jj-async "squash")
    (jitte-refresh)))

;;; Utilities

(defun jitte-status-maybe-update-revision-buffer ()
  "Update revision buffer if point moved to a different revision."
  (when (derived-mode-p 'jitte-status-mode)
    (when-let ((section (magit-current-section)))
      (when (eq (magit-section-type section) 'commit)
        (let ((revision (magit-section-value section)))
          (unless (equal revision jitte-buffer-revision)
            (setq jitte-buffer-revision revision)))))))

(defun jitte-read-repository ()
  "Read repository directory from user."
  (file-name-as-directory
   (expand-file-name
    (read-directory-name "Repository: " default-directory))))

(provide 'jitte-status)

;;; jitte-status.el ends here