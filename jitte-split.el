;;; jitte-split.el --- Change splitting interface  -*- lexical-binding:t -*-

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

;; This library implements the split interface for jj, which allows
;; interactive splitting of changes into multiple commits.  This
;; replaces the staging area concept from git with jj's approach
;; of splitting existing changes.

;;; Code:

(require 'jitte-core)
(require 'jitte-jj)
(require 'jitte-mode)
(require 'jitte-diff)

;;; Options

(defgroup jitte-split nil
  "Split changes into multiple commits."
  :group 'jitte-modes)

(defcustom jitte-split-mode-hook nil
  "Hook run after entering Jitte-Split mode."
  :group 'jitte-split
  :type 'hook)

(defcustom jitte-split-show-instructions t
  "Whether to show instructions in split buffers."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-split
  :type 'boolean)

(defcustom jitte-split-default-template
  "if(description, description, \"(no description)\")"
  "Default template for split commit descriptions."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-split
  :type 'string)

;;; Mode

(defvar jitte-split-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map jitte-mode-map)
    (define-key map "s" #'jitte-split-stage-hunk)
    (define-key map "u" #'jitte-split-unstage-hunk)
    (define-key map "S" #'jitte-split-stage-file)
    (define-key map "U" #'jitte-split-unstage-file)
    (define-key map "a" #'jitte-split-stage-all)
    (define-key map "k" #'jitte-split-discard-hunk)
    (define-key map "K" #'jitte-split-discard-file)
    (define-key map "RET" #'jitte-split-visit-file)
    (define-key map "c" #'jitte-split-commit)
    (define-key map "C" #'jitte-split-commit-and-continue)
    (define-key map "x" #'jitte-split-abort)
    (define-key map "?" #'jitte-split-help)
    map)
  "Keymap for `jitte-split-mode'.")

(define-derived-mode jitte-split-mode jitte-mode "Jitte Split"
  "Mode for interactively splitting changes.

This mode provides an interface similar to git's staging area,
but for jj's split command.  You can select which files and hunks
to include in the first commit, with the remainder going to the
second commit.

\\{jitte-split-mode-map}"
  :group 'jitte-split
  (hack-dir-local-variables-non-file-buffer)
  (setq truncate-lines nil)
  (run-hooks 'jitte-split-mode-hook))

;;; Variables

(defvar-local jitte-split-revision nil
  "The revision being split.")

(defvar-local jitte-split-staged-files nil
  "Files staged for the first commit.")

(defvar-local jitte-split-staged-hunks nil
  "Hunks staged for the first commit.")

(defvar-local jitte-split-process nil
  "The split process.")

;;; Setup

(defun jitte-split-setup-buffer (&optional revision)
  "Setup and return the split buffer for REVISION."
  (let ((buffer (jitte-setup-buffer 'jitte-split-mode)))
    (with-current-buffer buffer
      (setq jitte-split-revision (or revision "@"))
      (setq jitte-split-staged-files nil)
      (setq jitte-split-staged-hunks nil)
      (jitte-split-refresh-buffer))
    (jitte-display-buffer buffer)
    buffer))

;;;###autoload
(defun jitte-split (&optional revision)
  "Split REVISION interactively."
  (interactive)
  (let ((revision (or revision "@")))
    (jitte-split-setup-buffer revision)))

;;;###autoload
(defun jitte-split-current ()
  "Split the current working copy change."
  (interactive)
  (jitte-split "@"))

;;;###autoload
(defun jitte-split-paths (&optional paths)
  "Split by selecting specific PATHS."
  (interactive)
  (let ((paths (or paths
                   (jitte-split-read-paths))))
    (when paths
      (jitte-split-by-paths paths))))

;;;###autoload
(defun jitte-split-hunks ()
  "Split by selecting specific hunks."
  (interactive)
  (jitte-split "@"))

;;; Refresh

(defun jitte-split-refresh-buffer ()
  "Refresh the current split buffer."
  (when (derived-mode-p 'jitte-split-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (jitte-insert-split-buffer-contents))))

(defun jitte-insert-split-buffer-contents ()
  "Insert the contents of the split buffer."
  (magit-insert-section (split)
    (when jitte-split-show-instructions
      (jitte-insert-split-instructions))
    (jitte-insert-split-headers)
    (jitte-insert-split-staged)
    (jitte-insert-split-unstaged)))

(defun jitte-insert-split-instructions ()
  "Insert split instructions."
  (magit-insert-section (instructions)
    (magit-insert-heading "Instructions")
    (insert "Select files and hunks to include in the first commit.\n")
    (insert "The remainder will go to the second commit.\n")
    (insert "\n")
    (insert "Commands:\n")
    (insert "  s - stage hunk/file for first commit\n")
    (insert "  u - unstage hunk/file from first commit\n")
    (insert "  c - commit split\n")
    (insert "  x - abort split\n")
    (insert "\n")))

(defun jitte-insert-split-headers ()
  "Insert split headers."
  (magit-insert-section (headers)
    (magit-insert-heading "Split")
    (insert (format "Splitting revision: %s\n" jitte-split-revision))
    (when jitte-split-staged-files
      (insert (format "Staged files: %d\n" (length jitte-split-staged-files))))
    (insert "\n")))

(defun jitte-insert-split-staged ()
  "Insert staged changes section."
  (magit-insert-section (staged)
    (magit-insert-heading "Staged for first commit")
    (if (or jitte-split-staged-files jitte-split-staged-hunks)
        (progn
          (jitte-insert-split-staged-files)
          (jitte-insert-split-staged-hunks))
      (insert "  (no changes staged)\n"))
    (insert "\n")))

(defun jitte-insert-split-unstaged ()
  "Insert unstaged changes section."
  (magit-insert-section (unstaged)
    (magit-insert-heading "Available changes")
    (jitte-insert-split-available-changes)
    (insert "\n")))

(defun jitte-insert-split-staged-files ()
  "Insert staged files."
  (dolist (file jitte-split-staged-files)
    (magit-insert-section (file file)
      (insert (format "  %s (staged)\n" file)))))

(defun jitte-insert-split-staged-hunks ()
  "Insert staged hunks."
  (dolist (hunk jitte-split-staged-hunks)
    (magit-insert-section (hunk hunk)
      (insert (format "  %s (staged hunk)\n" hunk)))))

(defun jitte-insert-split-available-changes ()
  "Insert available changes."
  (let* ((diff-output (jitte-get-diff-output jitte-split-revision nil nil))
         (files (jitte-split-get-changed-files)))
    (if files
        (dolist (file files)
          (jitte-insert-split-file file))
      (insert "  (no changes available)\n"))))

(defun jitte-insert-split-file (file)
  "Insert FILE in the split buffer."
  (magit-insert-section (file file)
    (let ((staged-p (member file jitte-split-staged-files)))
      (insert (format "  %s%s\n" 
                       file 
                       (if staged-p " (staged)" ""))))))

;;; Staging Operations

(defun jitte-split-stage-hunk ()
  "Stage the hunk at point for the first commit."
  (interactive)
  (when-let ((hunk (jitte-split-hunk-at-point)))
    (unless (member hunk jitte-split-staged-hunks)
      (push hunk jitte-split-staged-hunks)
      (message "Staged hunk")
      (jitte-split-refresh-buffer))))

(defun jitte-split-unstage-hunk ()
  "Unstage the hunk at point from the first commit."
  (interactive)
  (when-let ((hunk (jitte-split-hunk-at-point)))
    (setq jitte-split-staged-hunks 
          (remove hunk jitte-split-staged-hunks))
    (message "Unstaged hunk")
    (jitte-split-refresh-buffer)))

(defun jitte-split-stage-file ()
  "Stage the file at point for the first commit."
  (interactive)
  (when-let ((file (jitte-split-file-at-point)))
    (unless (member file jitte-split-staged-files)
      (push file jitte-split-staged-files)
      (message "Staged file: %s" file)
      (jitte-split-refresh-buffer))))

(defun jitte-split-unstage-file ()
  "Unstage the file at point from the first commit."
  (interactive)
  (when-let ((file (jitte-split-file-at-point)))
    (setq jitte-split-staged-files 
          (remove file jitte-split-staged-files))
    (message "Unstaged file: %s" file)
    (jitte-split-refresh-buffer)))

(defun jitte-split-stage-all ()
  "Stage all changes for the first commit."
  (interactive)
  (let ((files (jitte-split-get-changed-files)))
    (setq jitte-split-staged-files files)
    (message "Staged all files")
    (jitte-split-refresh-buffer)))

;;; Discard Operations

(defun jitte-split-discard-hunk ()
  "Discard the hunk at point."
  (interactive)
  (when-let ((hunk (jitte-split-hunk-at-point)))
    (when (y-or-n-p "Discard this hunk? ")
      (message "Discard hunk (not yet implemented)"))))

(defun jitte-split-discard-file ()
  "Discard changes to the file at point."
  (interactive)
  (when-let ((file (jitte-split-file-at-point)))
    (when (y-or-n-p (format "Discard changes to %s? " file))
      (message "Discard file: %s (not yet implemented)" file))))

;;; Split Operations

(defun jitte-split-commit ()
  "Commit the split."
  (interactive)
  (if (or jitte-split-staged-files jitte-split-staged-hunks)
      (jitte-split-execute)
    (user-error "No changes staged for split")))

(defun jitte-split-commit-and-continue ()
  "Commit the split and continue with another split."
  (interactive)
  (jitte-split-commit)
  (jitte-split "@"))

(defun jitte-split-execute ()
  "Execute the split operation."
  (let ((first-commit-files jitte-split-staged-files)
        (description1 (read-string "Description for first commit: "))
        (description2 (read-string "Description for second commit: ")))
    (when (and (not (string-empty-p description1))
               (not (string-empty-p description2)))
      (jitte-split-perform-split first-commit-files description1 description2))))

(defun jitte-split-perform-split (files desc1 desc2)
  "Perform the actual split with FILES, DESC1, and DESC2."
  (message "Performing split (placeholder implementation)")
  (message "First commit: %s - %s" desc1 (mapconcat #'identity files ", "))
  (message "Second commit: %s" desc2)
  (quit-window t))

(defun jitte-split-abort ()
  "Abort the split operation."
  (interactive)
  (when (y-or-n-p "Abort split? ")
    (quit-window t)))

;;; Utilities

(defun jitte-split-file-at-point ()
  "Return the file at point in the split buffer."
  (when-let ((section (magit-current-section)))
    (when (eq (magit-section-type section) 'file)
      (magit-section-value section))))

(defun jitte-split-hunk-at-point ()
  "Return the hunk at point in the split buffer."
  (when-let ((section (magit-current-section)))
    (when (eq (magit-section-type section) 'hunk)
      (magit-section-value section))))

(defun jitte-split-get-changed-files ()
  "Get list of changed files for the current split."
  (let ((files (ignore-errors (jitte-diff-files jitte-split-revision))))
    (or files nil)))

(defun jitte-split-visit-file ()
  "Visit the file at point."
  (interactive)
  (when-let ((file (jitte-split-file-at-point)))
    (find-file file)))

;;; Path Selection

(defun jitte-split-read-paths ()
  "Read paths for splitting."
  (let ((files (jitte-split-get-changed-files)))
    (when files
      (delq nil
            (mapcar (lambda (file)
                      (when (y-or-n-p (format "Include %s in first commit? " file))
                        file))
                    files)))))

(defun jitte-split-by-paths (paths)
  "Split by selecting specific PATHS."
  (let ((description1 (read-string "Description for first commit: "))
        (description2 (read-string "Description for second commit: ")))
    (when (and (not (string-empty-p description1))
               (not (string-empty-p description2)))
      (message "Split by paths: %s (not yet implemented)" 
               (mapconcat #'identity paths ", ")))))

;;; Help

(defun jitte-split-help ()
  "Show help for split mode."
  (interactive)
  (message "Split mode help:
s - stage hunk/file
u - unstage hunk/file  
S - stage entire file
U - unstage entire file
a - stage all changes
c - commit split
x - abort split
? - show this help"))

;;; Integration Commands

(defun jitte-split-lines ()
  "Split by selecting specific lines."
  (interactive)
  (message "Line-by-line splitting not yet implemented"))

(provide 'jitte-split)

;;; jitte-split.el ends here