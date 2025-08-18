;;; jitte-diff.el --- Inspect diffs  -*- lexical-binding:t -*-

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

;; This library implements support for looking at jj diffs and changes.

;;; Code:

(require 'jitte-core)
(require 'jitte-jj)
(require 'jitte-mode)

(require 'diff-mode)

;;; Options

(defgroup jitte-diff nil
  "Inspect and manipulate diffs."
  :group 'jitte-modes)

(defcustom jitte-diff-mode-hook nil
  "Hook run after entering Jitte-Diff mode."
  :group 'jitte-diff
  :type 'hook)

(defcustom jitte-diff-refine-hunk t
  "Whether to refine hunks by default."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-diff
  :type 'boolean)

(defcustom jitte-diff-adjust-tab-width nil
  "Whether to adjust the tab width in diff buffers."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-diff
  :type 'boolean)

(defcustom jitte-diff-paint-whitespace t
  "Whether to paint whitespace in diff buffers."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-diff
  :type 'boolean)

(defcustom jitte-diff-highlight-trailing t
  "Whether to highlight trailing whitespace in diff buffers."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-diff
  :type 'boolean)

(defcustom jitte-diff-context-lines 3
  "Number of context lines to show around hunks."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-diff
  :type 'integer)

;;; Mode

(defvar jitte-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map jitte-mode-map)
    (define-key map "j" #'jitte-diff-jump-to-file)
    (define-key map "+" #'jitte-diff-more-context)
    (define-key map "-" #'jitte-diff-less-context)
    (define-key map "0" #'jitte-diff-default-context)
    (define-key map "SPC" #'scroll-up-command)
    (define-key map "DEL" #'scroll-down-command)
    map)
  "Keymap for `jitte-diff-mode'.")

(define-derived-mode jitte-diff-mode jitte-mode "Jitte Diff"
  "Mode for inspecting diffs.

\\{jitte-diff-mode-map}"
  :group 'jitte-diff
  (hack-dir-local-variables-non-file-buffer)
  (setq truncate-lines nil)
  (when jitte-diff-adjust-tab-width
    (setq tab-width jitte-diff-tab-width))
  (when jitte-diff-paint-whitespace
    (jitte-diff-paint-whitespace-mode 1))
  (run-hooks 'jitte-diff-mode-hook))

;;; Setup

(defun jitte-diff-setup-buffer (&optional revision args files)
  "Setup and return the diff buffer for REVISION with ARGS and FILES."
  (let ((buffer (jitte-setup-buffer 'jitte-diff-mode)))
    (with-current-buffer buffer
      (setq jitte-buffer-revision revision)
      (setq jitte-buffer-diff-args args)
      (setq jitte-buffer-diff-files files)
      (jitte-diff-refresh-buffer))
    (jitte-display-buffer buffer)
    buffer))

;;;###autoload
(defun jitte-diff (&optional revision args files)
  "Show diff for REVISION with ARGS and FILES."
  (interactive)
  (let ((revision (or revision "@")))
    (jitte-diff-setup-buffer revision args files)))

;;;###autoload
(defun jitte-diff-working-copy (&optional args files)
  "Show diff for working copy with ARGS and FILES."
  (interactive)
  (jitte-diff "@" args files))

;;;###autoload
(defun jitte-diff-revision (&optional revision args files)
  "Show diff for REVISION with ARGS and FILES."
  (interactive)
  (let ((revision (or revision (read-string "Revision: " "@"))))
    (jitte-diff revision args files)))

;;;###autoload
(defun jitte-diff-range (&optional range args files)
  "Show diff for RANGE with ARGS and FILES."
  (interactive)
  (let ((range (or range (read-string "Range: "))))
    (jitte-diff range args files)))

;;;###autoload
(defun jitte-show (&optional revision args)
  "Show REVISION with ARGS."
  (interactive)
  (let ((revision (or revision (read-string "Show revision: " "@"))))
    (jitte-diff revision args)))

;;; Refresh

(defun jitte-diff-refresh-buffer ()
  "Refresh the current diff buffer."
  (when (derived-mode-p 'jitte-diff-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (jitte-insert-diff-buffer-contents))))

(defun jitte-insert-diff-buffer-contents ()
  "Insert the contents of the diff buffer."
  (magit-insert-section (diff)
    (jitte-insert-diff-headers)
    (jitte-insert-diff-content)))

(defun jitte-insert-diff-headers ()
  "Insert diff headers."
  (magit-insert-section (headers)
    (magit-insert-heading "Diff")
    (when jitte-buffer-revision
      (insert (format "Revision: %s\n" jitte-buffer-revision)))
    (when jitte-buffer-diff-args
      (insert (format "Arguments: %s\n" 
                       (mapconcat #'identity jitte-buffer-diff-args " "))))
    (when jitte-buffer-diff-files
      (insert (format "Files: %s\n" 
                       (mapconcat #'identity jitte-buffer-diff-files " "))))
    (insert "\n")))

(defun jitte-insert-diff-content ()
  "Insert diff content."
  (let* ((revision jitte-buffer-revision)
         (args jitte-buffer-diff-args)
         (files jitte-buffer-diff-files)
         (output (jitte-get-diff-output revision args files)))
    (when output
      (jitte-wash-diff output))))

(defun jitte-get-diff-output (revision args files)
  "Get diff output for REVISION with ARGS and FILES."
  (let ((cmd-args (append (list "diff")
                          (when revision (list "-r" revision))
                          args
                          files)))
    (ignore-errors (apply #'jitte-run-jj-sync cmd-args))))

(defun jitte-wash-diff (output)
  "Wash diff OUTPUT into sections."
  (let ((lines (split-string output "\n" t)))
    (jitte-wash-diff-lines lines)))

(defun jitte-wash-diff-lines (lines)
  "Wash diff LINES into sections."
  (let ((current-file nil)
        (current-hunk nil)
        (hunk-lines nil))
    (dolist (line lines)
      (cond
       ((string-match "^diff --git a/\\(.*\\) b/\\(.*\\)$" line)
        (when current-file
          (jitte-finish-diff-file current-file current-hunk hunk-lines))
        (setq current-file (match-string 1 line))
        (setq current-hunk nil)
        (setq hunk-lines nil)
        (jitte-start-diff-file current-file line))
       
       ((string-match "^@@" line)
        (when current-hunk
          (jitte-finish-diff-hunk current-hunk hunk-lines))
        (setq current-hunk line)
        (setq hunk-lines (list line)))
       
       ((and current-hunk (string-match "^[+-]" line))
        (push line hunk-lines))
       
       ((and current-hunk (string-match "^ " line))
        (push line hunk-lines))
       
       (t
        (when current-file
          (insert line "\n")))))
    
    (when current-file
      (jitte-finish-diff-file current-file current-hunk hunk-lines))))

(defun jitte-start-diff-file (file header)
  "Start a diff file section for FILE with HEADER."
  (magit-insert-section (file file)
    (magit-insert-heading file)
    (insert header "\n")))

(defun jitte-finish-diff-file (file current-hunk hunk-lines)
  "Finish a diff file section for FILE."
  (when current-hunk
    (jitte-finish-diff-hunk current-hunk hunk-lines)))

(defun jitte-finish-diff-hunk (hunk hunk-lines)
  "Finish a diff hunk section for HUNK with HUNK-LINES."
  (magit-insert-section (hunk hunk)
    (dolist (line (reverse hunk-lines))
      (jitte-insert-diff-line line))))

(defun jitte-insert-diff-line (line)
  "Insert a diff LINE with appropriate formatting."
  (let ((face (cond
               ((string-prefix-p "+" line) 'diff-added)
               ((string-prefix-p "-" line) 'diff-removed)
               ((string-prefix-p "@@" line) 'diff-hunk-header)
               (t 'diff-context))))
    (insert (propertize line 'font-lock-face face) "\n")))

;;; Context Management

(defun jitte-diff-more-context ()
  "Increase context lines and refresh."
  (interactive)
  (setq jitte-diff-context-lines (+ jitte-diff-context-lines 3))
  (message "Context lines: %d" jitte-diff-context-lines)
  (jitte-diff-refresh-buffer))

(defun jitte-diff-less-context ()
  "Decrease context lines and refresh."
  (interactive)
  (setq jitte-diff-context-lines (max 0 (- jitte-diff-context-lines 3)))
  (message "Context lines: %d" jitte-diff-context-lines)
  (jitte-diff-refresh-buffer))

(defun jitte-diff-default-context ()
  "Reset context lines to default and refresh."
  (interactive)
  (setq jitte-diff-context-lines 3)
  (message "Context lines: %d" jitte-diff-context-lines)
  (jitte-diff-refresh-buffer))

;;; Navigation

(defun jitte-diff-jump-to-file ()
  "Jump to a file in the diff."
  (interactive)
  (let ((files (jitte-diff-get-files)))
    (when files
      (let ((file (completing-read "Jump to file: " files)))
        (jitte-diff-goto-file file)))))

(defun jitte-diff-get-files ()
  "Get list of files in the current diff."
  (let ((files nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^diff --git a/\\(.*\\) b/" nil t)
        (push (match-string 1) files)))
    (reverse files)))

(defun jitte-diff-goto-file (file)
  "Go to FILE in the diff buffer."
  (goto-char (point-min))
  (when (search-forward file nil t)
    (beginning-of-line)))

;;; File Visiting

(defun jitte-diff-visit-file (&optional other-window)
  "Visit the file at point.
If OTHER-WINDOW is non-nil, open in other window."
  (interactive "P")
  (when-let ((file (jitte-diff-file-at-point)))
    (if other-window
        (find-file-other-window file)
      (find-file file))))

(defun jitte-diff-file-at-point ()
  "Return the file at point in the diff buffer."
  (when-let ((section (magit-current-section)))
    (when (eq (magit-section-type section) 'file)
      (magit-section-value section))))

;;; Commands

(defun jitte-diff-stage-file ()
  "Stage the file at point."
  (interactive)
  (when-let ((file (jitte-diff-file-at-point)))
    (message "Stage file: %s (not yet implemented)" file)))

(defun jitte-diff-unstage-file ()
  "Unstage the file at point."
  (interactive)
  (when-let ((file (jitte-diff-file-at-point)))
    (message "Unstage file: %s (not yet implemented)" file)))

(defun jitte-diff-discard-file ()
  "Discard changes to the file at point."
  (interactive)
  (when-let ((file (jitte-diff-file-at-point)))
    (when (y-or-n-p (format "Discard changes to %s? " file))
      (message "Discard file: %s (not yet implemented)" file))))

;;; Utilities

(defun jitte-diff-hunk-at-point ()
  "Return the hunk at point in the diff buffer."
  (when-let ((section (magit-current-section)))
    (when (eq (magit-section-type section) 'hunk)
      (magit-section-value section))))

;;; Whitespace Handling

(define-minor-mode jitte-diff-paint-whitespace-mode
  "Highlight whitespace issues in diff buffers."
  :lighter " Whitespace"
  (if jitte-diff-paint-whitespace-mode
      (progn
        (when jitte-diff-highlight-trailing
          (setq show-trailing-whitespace t)))
    (setq show-trailing-whitespace nil)))

;;; Integration

(defun jitte-diff-paths (&optional paths)
  "Show diff for PATHS."
  (interactive)
  (let ((paths (or paths
                   (list (read-file-name "Show diff for path: ")))))
    (jitte-diff "@" nil paths)))

(defun jitte-diff-worktree ()
  "Show diff for entire worktree."
  (interactive)
  (jitte-diff "@"))

(provide 'jitte-diff)

;;; jitte-diff.el ends here