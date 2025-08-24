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
  '() ; cli flags go here if needed
  "Default arguments for jj log."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-log
  :type '(repeat string))

;;; Faces

(defface jitte-log-current-commit
  '((t :foreground "orange" :weight bold))
  "Face for the current commit (@) symbol."
  :group 'jitte-faces)

(defface jitte-log-mutable-commit
  '((t :foreground "cyan" :weight bold))
  "Face for mutable commit (○) symbols."
  :group 'jitte-faces)

(defface jitte-log-immutable-commit
  '((t :foreground "green" :weight bold))
  "Face for immutable commit (◆) symbols."
  :group 'jitte-faces)

(defface jitte-log-elided
  '((t :inherit jitte-dimmed))
  "Face for elided commits (~) symbol."
  :group 'jitte-faces)

(defface jitte-log-graph
  '((t :inherit jitte-dimmed))
  "Face for graph lines."
  :group 'jitte-faces)

(defface jitte-log-author
  '((((class color) (background light)) :foreground "firebrick")
    (((class color) (background  dark)) :foreground "tomato"))
  "Face for author names in log."
  :group 'jitte-faces)

(defface jitte-log-date
  '((((class color) (background light)) :foreground "grey50")
    (((class color) (background  dark)) :foreground "grey50"))
  "Face for dates in log."
  :group 'jitte-faces)

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

(defcustom jitte-log-debug nil
  "Whether to show debug information for jj log commands."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-log
  :type 'boolean)

;;; Variables

(defvar-local jitte-log-buffer-args nil
  "Arguments used to generate the current log buffer.")

(defvar-local jitte-log-buffer-revs nil
  "Revisions shown in the current log buffer.")

;;; Regexps for parsing jj log output

(defconst jitte-log-commit-re
  (concat "^\\([@ ◆○~│├─┬┐┘┌╮╯╰╭╲╱─┴┤]*\\) +"        ; Graph characters (group 1)
          "\\([a-z0-9]+\\)"                          ; Change ID (group 2) 
          " +\\([^0-9]+\\)"                          ; Author (group 3)
          " +\\([0-9]+-[0-9]+-[0-9]+ [0-9:]+\\)"     ; Date (group 4)
          "\\(?: +\\([a-z0-9_()@]+\\)\\)?"           ; Branch/refs (group 5)
          "\\(?: +\\([a-z0-9]+\\)\\)?"              ; Hash (group 6)
          )
  "Regexp to match jj log commit lines.")

;;; Mode

(defvar jitte-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map jitte-mode-map)
    (define-key map "j" #'jitte-log-move-to-revision)
    (define-key map "=" #'jitte-log-toggle-commit-limit)
    (define-key map "+" #'jitte-log-double-commit-limit)
    (define-key map "-" #'jitte-log-half-commit-limit)
    (define-key map "RET" #'jitte-log-show-commit)
    (define-key map "l" #'jitte-log-refresh)
    (define-key map "e" #'jitte-log-edit)
    (define-key map "n" #'jitte-log-new)
    (define-key map "d" #'jitte-log-describe)
    (define-key map "R" #'jitte-log-rebase-interactive)
    (define-key map "r" #'jitte-log-rebase-prompt)
    (define-key map "A" #'jitte-log-cherry-pick)
    map)
  "Keymap for `jitte-log-mode'.")

;;; Section Keymaps

(defvar jitte-commit-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "RET" #'jitte-log-show-commit)
    (define-key map "l" #'jitte-log-refresh)
    (define-key map "e" #'jitte-log-edit)
    (define-key map "n" #'jitte-log-new)
    (define-key map "d" #'jitte-log-describe)
    (define-key map "R" #'jitte-log-rebase-interactive)
    (define-key map "r" #'jitte-log-rebase-prompt)
    (define-key map "A" #'jitte-log-cherry-pick)
    map)
  "Keymap for `commit' sections in jitte-log buffers.")

;; Register the section map
(with-eval-after-load 'magit-section
  (put 'commit :magit-section-keymap jitte-commit-section-map))

(define-derived-mode jitte-log-mode jitte-mode "Jitte Log"
  "Mode for browsing jj commit history.

\<jitte-log-mode-map>\
Type \\[jitte-refresh] to refresh the current buffer.
Type \\[jitte-log-show-commit] to visit the commit at point.
Type \\[jitte-log-edit] to edit the commit at point.
Type \\[jitte-log-new] to create a new commit on top of the commit at point.
Type \\[jitte-log-rebase-interactive] to rebase commit at point interactively.
Type \\[jitte-log-rebase-prompt] to rebase commit at point with prompted destination.

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
      (jitte-wash-log-from-output output))))

(defun jitte-get-log-output (revset args files)
  "Get log output for REVSET with ARGS and FILES."
  (let ((cmd-args (append (list "log")
                          (when revset (list "-r" revset))
                          args
                          files)))
    (when jitte-log-debug
      (message "Running jj command: jj %s (in %s)" 
               (string-join cmd-args " ")
               default-directory))
    (condition-case err
        (let ((output (apply #'jitte-run-jj-sync cmd-args)))
          (when jitte-log-debug
            (message "jj log returned %d characters of output" (length output)))
          output)
      (error 
       (let ((error-msg (error-message-string err)))
         (message "jj log command failed: %s" error-msg)
         (format "Error running jj log command:\nCommand: jj %s\nDirectory: %s\nError: %s\n\nThis usually means:\n- No jj repository in current directory\n- Invalid revset syntax\n- jj executable not found\n\nTry running 'jj status' to check if this is a valid jj repository."
                 (string-join cmd-args " ")
                 default-directory
                 error-msg))))))

(defun jitte-wash-log-from-output (output)
  "Wash log OUTPUT into magit sections."
  (if (string-prefix-p "Error running jj log command:" output)
      ;; This is an error message, display it as-is with appropriate face
      (magit-insert-section (error)
        (insert (propertize output 'font-lock-face 'error)))
    ;; Normal log output, process as usual
    (let ((lines (split-string output "\n" t)))
      (jitte-wash-log-lines lines))))

(defun jitte-wash-log-lines (lines)
  "Process LINES of jj log output into commit sections."
  (let ((current-commit nil)
        (current-lines nil))
    (dolist (line lines)
      (if (string-match jitte-log-commit-re line)
          (progn
            ;; Process previous commit if exists
            (when current-commit
              (jitte-insert-commit-section current-commit (nreverse current-lines)))
            ;; Start new commit
            (setq current-commit (match-string 2 line))
            (setq current-lines (list line)))
        ;; This is a continuation line (description)
        (when current-commit
          (push line current-lines))))
    ;; Process the last commit
    (when current-commit
      (jitte-insert-commit-section current-commit (nreverse current-lines)))))

(defun jitte-insert-commit-section (change-id lines)
  "Insert a commit section for CHANGE-ID with LINES."
  (magit-insert-section (commit change-id)
    (dolist (line lines)
      (insert (jitte-log-colorize-line line) "\n"))))

(defun jitte-log-colorize-line (line)
  "Add color properties to a jj log LINE while preserving format."
  (with-temp-buffer
    (insert line)
    (goto-char (point-min))
    
    ;; Color commit symbols with different colors for mutable vs immutable
    (while (re-search-forward "\\(@\\|○\\|◆\\|~\\)" nil t)
      (let ((symbol (match-string 1))
            (start (match-beginning 1))
            (end (match-end 1)))
        (put-text-property start end 'font-lock-face
                           (cond
                            ((string= symbol "@") 'jitte-log-current-commit)
                            ((string= symbol "○") 'jitte-log-mutable-commit)
                            ((string= symbol "◆") 'jitte-log-immutable-commit)
                            ((string= symbol "~") 'jitte-log-elided)))))
    
    ;; Color graph lines (but not the commit symbols we just colored)
    (goto-char (point-min))
    (while (re-search-forward "\\([│├─┬┐┘┌╮╯╰╭╲╱┴┤]\\)" nil t)
      (put-text-property (match-beginning 1) (match-end 1) 
                         'font-lock-face 'jitte-log-graph))
    
    ;; Color change ID (8-char hex after graph)
    (goto-char (point-min))
    (when (re-search-forward "\\b\\([a-z0-9]\\{8\\}\\)\\b" nil t)
      (put-text-property (match-beginning 1) (match-end 1)
                         'font-lock-face 'jitte-hash))
    
    ;; Color email addresses
    (goto-char (point-min))
    (when (re-search-forward "\\([a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]\\{2,\\}\\)" nil t)
      (put-text-property (match-beginning 1) (match-end 1)
                         'font-lock-face 'jitte-log-author))
    
    ;; Color dates
    (goto-char (point-min))
    (when (re-search-forward "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)" nil t)
      (put-text-property (match-beginning 1) (match-end 1)
                         'font-lock-face 'jitte-log-date))
    
    ;; Color git hash at end of line
    (goto-char (point-min))
    (when (re-search-forward "\\b\\([a-f0-9]\\{7,40\\}\\)\\s-*$" nil t)
      (put-text-property (match-beginning 1) (match-end 1)
                         'font-lock-face 'jitte-hash))
    
    ;; Color bookmarks and refs
    (goto-char (point-min))
    (when (re-search-forward "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}" nil t)
      (while (re-search-forward "\\b\\([a-zA-Z0-9_/@*()-][a-zA-Z0-9_/@*().-]*[a-zA-Z0-9_/@*().-]\\|[a-zA-Z0-9_/@*()-]\\)\\b" 
                                (line-end-position) t)
        (let ((candidate (match-string 1)))
          ;; Skip if it looks like a hash (all lowercase hex)
          (unless (and (>= (length candidate) 7)
                       (string-match-p "^[a-f0-9]+$" candidate))
            (put-text-property (match-beginning 1) (match-end 1)
                               'font-lock-face 'jitte-branch-remote)))))
    
    (buffer-string)))


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

(defun jitte-log-show-commit (&optional revision)
  "Show commit REVISION in a dedicated buffer."
  (interactive)
  (let ((revision (or revision
                      (jitte-log-section-value-if 'commit)
                      (read-string "Show revision: "))))
    (when revision
      (jitte-log-show-commit-buffer revision))))

(defun jitte-log-show-commit-buffer (commit)
  "Show COMMIT in a dedicated buffer."
  (let* ((buffer-name (format "*jitte-revision: %s*" (substring commit 0 8)))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (jitte-revision-mode)
        (jitte-revision-insert-commit commit)
        (goto-char (point-min))))
    (display-buffer buffer)))

(defun jitte-show-commit (&optional revision)
  "Show commit REVISION."
  (interactive)
  (let ((revision (or revision
                      (jitte-log-section-value-if 'commit)
                      (read-string "Show revision: "))))
    (when revision
      (jitte-diff-setup-buffer revision))))

(defun jitte-log-cherry-pick ()
  "Cherry-pick the commit at point."
  (interactive)
  (when-let ((commit (jitte-log-section-value-if 'commit)))
    (when (y-or-n-p (format "Cherry-pick %s? " commit))
      (jitte-run-jj-async "new" commit)
      (jitte-refresh))))

(defun jitte-log-edit (&optional commit)
  "Edit the commit at point or COMMIT."
  (interactive)
  (let ((change-id (or commit 
                       (jitte-log-section-value-if 'commit)
                       (user-error "No commit at point"))))
    (when (y-or-n-p (format "Edit commit %s? " (substring change-id 0 8)))
      (jitte-run-jj-async "edit" change-id)
      (message "Editing commit %s" (substring change-id 0 8))
      (jitte-refresh))))

(defun jitte-log-new (&optional commit)
  "Create a new commit on top of the commit at point or COMMIT."
  (interactive)
  (let ((change-id (or commit 
                       (jitte-log-section-value-if 'commit)
                       (user-error "No commit at point"))))
    (when (y-or-n-p (format "Create new commit on top of %s? " (substring change-id 0 8)))
      (jitte-run-jj-async "new" change-id)
      (message "Created new commit on top of %s" (substring change-id 0 8))
      (jitte-refresh))))

(defun jitte-log-describe (&optional commit)
  "Describe the commit at point or COMMIT."
  (interactive)
  (let ((change-id (or commit
                       (jitte-log-section-value-if 'commit)
                       (user-error "No commit at point"))))
    (let ((message (read-string "Description: ")))
      (when (not (string-empty-p message))
        (jitte-run-jj-async "describe" "-r" change-id "-m" message)
        (message "Updated description for %s" (substring change-id 0 8))
        (jitte-refresh)))))

;;; Utilities

(defun jitte-log-section-value-if (type)
  "Return the value of the current section if it is of TYPE."
  (when-let ((section (magit-current-section)))
    (when (eq (magit-section-type section) type)
      (magit-section-value section))))

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

;;; Revision Show Mode

(defvar jitte-revision-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map jitte-mode-map)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `jitte-revision-mode'.")

(define-derived-mode jitte-revision-mode jitte-mode "Jitte Revision"
  "Mode for viewing a single jujutsu commit."
  :group 'jitte-log
  (hack-dir-local-variables-non-file-buffer)
  (setq truncate-lines nil))

(defun jitte-revision-insert-commit (commit)
  "Insert commit COMMIT details and diff."
  (jitte-with-repository nil
    ;; First section: Commit Info
    (magit-insert-section (commit-info)
      (magit-insert-heading "Commit Info")
      (jitte-revision-wash-show commit)
      (insert "\n"))
    
    ;; Second section: Diff  
    (magit-insert-section (commit-diff)
      (magit-insert-heading "Diff")
      (jitte-revision-wash-diff commit))))

(defun jitte-revision-wash-show (commit)
  "Insert jj show output for COMMIT."
  (let ((output (jitte-run-jj-sync "show" commit)))
    (when output
      (let ((lines (split-string output "\n")))
        (jitte-revision-parse-show-output lines)))))

(defun jitte-revision-parse-show-output (lines)
  "Parse jj show output LINES into formatted display."
  (let (commit-id change-id bookmarks author committer description)
    ;; Parse the metadata
    (dolist (line lines)
      (cond
       ((string-match "^Commit ID: \\(.+\\)$" line)
        (setq commit-id (match-string 1 line)))
       ((string-match "^Change ID: \\(.+\\)$" line)
        (setq change-id (match-string 1 line)))
       ((string-match "^Bookmarks: \\(.+\\)$" line)
        (setq bookmarks (match-string 1 line)))
       ((string-match "^Author   : \\(.+\\)$" line)
        (setq author (match-string 1 line)))
       ((string-match "^Committer: \\(.+\\)$" line)
        (setq committer (match-string 1 line)))))
    
    ;; Extract description (simple version for now)
    (let ((desc-start (cl-position "" lines :test #'string=)))
      (when desc-start
        (let ((desc-lines (nthcdr (1+ desc-start) lines)))
          (setq description (string-join 
                           (cl-remove-if (lambda (l) (string-match "^\\(Modified\\|Added\\|Deleted\\)" l)) desc-lines)
                           "\n")))))
    
    ;; Insert formatted output
    (when bookmarks
      (insert (propertize bookmarks 'font-lock-face 'jitte-branch-remote) " "))
    (when commit-id
      (insert (propertize commit-id 'font-lock-face 'jitte-hash) "\n"))
    (when change-id
      (insert "Change:     " (propertize change-id 'font-lock-face 'jitte-hash) "\n"))
    (when author
      (insert "Author:     " (propertize author 'font-lock-face 'jitte-log-author) "\n"))
    (when committer
      (insert "Committer:  " (propertize committer 'font-lock-face 'jitte-log-author) "\n"))
    (insert "\n")
    (when description
      (insert (string-trim description) "\n"))))

(defun jitte-revision-wash-diff (commit)
  "Insert jj diff output for COMMIT."
  (let ((output (jitte-run-jj-sync "diff" "-r" commit)))
    (when output
      (insert output))))

;;; Rebase Operations

(defun jitte-log-rebase-prompt (&optional commit)
  "Rebase COMMIT with prompted destination."
  (interactive)
  (let ((source-change-id (or commit 
                              (jitte-log-section-value-if 'commit)
                              (user-error "No commit at point")))
        (destination (read-string "Rebase destination: " "@")))
    (when (y-or-n-p (format "Rebase %s onto %s? " 
                            (substring source-change-id 0 8)
                            destination))
      (jitte-log-run-jj-command "rebase" "-r" source-change-id "-d" destination))))

(defvar-local jitte-rebase-source-commit nil
  "The source commit for interactive rebase.")

(defvar jitte-rebase-select-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map jitte-log-mode-map)
    (define-key map "RET" #'jitte-rebase-select-destination)
    (define-key map "C-c C-c" #'jitte-rebase-select-destination)
    (define-key map "q" #'jitte-rebase-select-quit)
    (define-key map "C-c C-k" #'jitte-rebase-select-quit)
    map)
  "Keymap for jj rebase destination selection.")

(define-derived-mode jitte-rebase-select-mode jitte-log-mode "Jitte Rebase Select"
  "Mode for selecting rebase destination.

\\<jitte-rebase-select-mode-map>
Type \\[jitte-rebase-select-destination] to select destination at point.
Type \\[jitte-rebase-select-quit] to abort rebase selection."
  :group 'jitte-log)

(defun jitte-log-rebase-interactive (&optional commit)
  "Interactively rebase COMMIT by selecting destination."
  (interactive)
  (let ((source-commit (or commit
                           (jitte-log-section-value-if 'commit)
                           (user-error "No commit at point"))))
    (jitte-rebase-select-setup-buffer source-commit)))

(defun jitte-rebase-select-setup-buffer (source-commit)
  "Set up rebase selection buffer for SOURCE-COMMIT."
  (let ((buffer (jitte-setup-buffer 'jitte-rebase-select-mode)))
    (with-current-buffer buffer
      (setq jitte-rebase-source-commit source-commit)
      (setq jitte-buffer-revisions "all()")
      (setq jitte-buffer-log-args jitte-log-default-arguments)
      (jitte-log-refresh-buffer)
      (setq header-line-format
            (format "Select destination for rebasing %s (RET to confirm, q to quit)"
                    (substring source-commit 0 8))))
    (jitte-display-buffer buffer)
    buffer))

(defun jitte-rebase-select-destination ()
  "Select commit at point as rebase destination."
  (interactive)
  (let ((destination (jitte-log-section-value-if 'commit)))
    (unless destination
      (user-error "No commit at point"))
    (when (string= destination jitte-rebase-source-commit)
      (user-error "Cannot rebase commit onto itself"))
    (quit-window t)
    (when (y-or-n-p (format "Rebase %s onto %s? " 
                            (substring jitte-rebase-source-commit 0 8)
                            (substring destination 0 8)))
      (jitte-log-run-jj-command "rebase" "-r" jitte-rebase-source-commit "-d" destination))))

(defun jitte-rebase-select-quit ()
  "Quit rebase selection."
  (interactive)
  (quit-window t)
  (message "Rebase cancelled"))

;; Helper function to run jj commands for log operations
(defun jitte-log-run-jj-command (command &rest args)
  "Run jj COMMAND with ARGS and display result."
  (jitte-with-repository nil
    (message "Running: jj %s %s" command (string-join args " "))
    (let ((result (apply #'jitte-run-jj command args)))
      (if (zerop result)
          (progn
            (message "jj %s completed successfully" command)
            (jitte-refresh))
        (user-error "jj %s failed" command)))))

;;; Transient Interface

(require 'transient)

;;;###autoload (autoload 'jitte-log-transient "jitte-log" nil t)
(transient-define-prefix jitte-log-transient ()
  "Show Jujutsu commit log with options."
  :class 'transient-prefix
  ["Arguments"
   ("-n" "Limit commits" "--limit=" :reader transient-read-number-N+)
   ("-r" "Revisions" "-r" :reader jitte-log-read-revisions)
   ("--no-graph" "Disable graph" "--no-graph")
   ("-T" "Template" "-T" :reader jitte-log-read-template)]
  ["Log"
   ("l" "current" jitte-log-current)
   ("a" "all" jitte-log-all)
   ("o" "other" jitte-log-other)])

(defun jitte-log-read-revisions (&rest _)
  "Read revisions for jj log."
  (read-string "Revisions: " "@"))

(defun jitte-log-read-template (&rest _)
  "Read template for jj log."
  (read-string "Template: " "oneline"))

(defun jitte-log-other (revisions &optional args)
  "Show log for REVISIONS with ARGS."
  (interactive (list (jitte-log-read-revisions)
                     (transient-args 'jitte-log-transient)))
  (jitte-log-setup-buffer revisions args))

(defun jitte-log-refresh ()
  "Refresh the current log buffer."
  (interactive)
  (jitte-refresh))

(provide 'jitte-log)

;;; jitte-log.el ends here
