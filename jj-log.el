;;; jj-log.el --- View Jujutsu commit history -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Spencer Nystrom
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (magit "3.0.0") (transient "0.3.0"))
;; Keywords: vc, tools
;; URL: https://github.com/snystrom/jitte

;;; Commentary:

;; This package provides a Magit-style log viewer for Jujutsu (jj).
;; It allows you to view commit history with navigation and diff viewing
;; capabilities similar to Magit's log mode.

;;; Code:

(require 'magit)
(require 'transient)
(require 'jj-describe)

;;; Configuration

(defgroup jj-log nil
  "View Jujutsu commit history."
  :prefix "jj-log-"
  :group 'tools)

(defcustom jj-log-executable "jj"
  "The Jujutsu executable."
  :type 'string
  :group 'jj-log)

(defcustom jj-log-mode-hook nil
  "Hook run after entering jj-log mode."
  :group 'jj-log
  :type 'hook)

;;; Helper Functions

(defun jj-log--find-root ()
  "Find the root of the current jj repository."
  (let ((dir (locate-dominating-file default-directory ".jj")))
    (when dir
      (expand-file-name dir))))

;;; Faces

(defface jj-log-current-commit
  '((t :foreground "orange" :weight bold))
  "Face for the current commit (@) symbol."
  :group 'jj-log)

(defface jj-log-mutable-commit
  '((t :inherit magit-cherry-unmatched :weight bold))
  "Face for mutable commit (○) symbols."
  :group 'jj-log)

(defface jj-log-immutable-commit
  '((t :inherit magit-cherry-equivalent :weight bold))
  "Face for immutable commit (◆) symbols."
  :group 'jj-log)

(defface jj-log-elided
  '((t :inherit shadow))
  "Face for elided commits (~) symbol."
  :group 'jj-log)

(defface jj-log-graph
  '((t :inherit magit-log-graph))
  "Face for graph lines."
  :group 'jj-log)

;;; Variables

(defvar-local jj-log-buffer-args nil
  "Arguments used to generate the current log buffer.")

(defvar-local jj-log-buffer-revs nil
  "Revisions shown in the current log buffer.")

;;; Regexps for parsing jj log output

(defconst jj-log-commit-re
  (concat "^\\([@ ◆○~│├─┬┐┘┌╮╯╰╭╲╱─┴┤]*\\) +"        ; Graph characters (group 1)
          "\\([a-z0-9]+\\)"                          ; Change ID (group 2) 
          " +\\([^0-9]+\\)"                          ; Author (group 3)
          " +\\([0-9]+-[0-9]+-[0-9]+ [0-9:]+\\)"     ; Date (group 4)
          "\\(?: +\\([a-z0-9_()@]+\\)\\)?"           ; Branch/refs (group 5)
          "\\(?: +\\([a-z0-9]+\\)\\)?")              ; Hash (group 6)
  "Regexp to match jj log commit lines.")

;;; Mode Definition

(defvar-keymap jj-log-mode-map
  :doc "Keymap for `jj-log-mode'."
  :parent magit-mode-map
  "RET" #'jj-show-commit
  "l"   #'jj-log
  "g"   #'magit-refresh
  "q"   #'magit-log-bury-buffer
  "e"   #'jj-edit
  "D"   #'jj-log-describe
  "d"   #'jj-diff-at-point
  "n"   #'jj-new
  "c"   #'jj-log-commit
  "u"   #'jj-quick-undo
  "R"   #'jj-rebase-interactive
  "r"   #'jj-rebase-prompt)

(define-derived-mode jj-log-mode magit-mode "JJ Log"
  "Mode for looking at Jujutsu commit history.

\\<jj-log-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[jj-show-commit] to visit the commit at point.
Type \\[jj-log] to change log arguments.
Type \\[jj-edit] to edit the commit at point.
Type \\[jj-new] to create a new commit on top of the commit at point.
Type \\[jj-diff-at-point] to open diff viewer for commit at point.
Type \\[jj-rebase-interactive] to rebase commit at point with interactive destination selection.
Type \\[jj-rebase-prompt] to rebase commit at point with prompted destination.

\\{jj-log-mode-map}"
  :group 'jj-log
  (magit-hack-dir-local-variables)
  (setq magit--imenu-item-types 'commit)
  ;; Override problematic magit-section bindings
  (define-key jj-log-mode-map "n" #'jj-new)
  (add-hook 'magit-refresh-buffer-hook #'jj-log-refresh-buffer nil t))

;;; Transient Interface

;;;###autoload (autoload 'jj-log "jj-log" nil t)
(transient-define-prefix jj-log ()
  "Show Jujutsu commit log."
  :class 'transient-prefix
  ["Arguments"
   ("-n" "Limit commits" "--limit=" :reader transient-read-number-N+)
   ("-r" "Revisions" "-r" :reader jj-log-read-revisions)]
  ["Log"
   ("l" "default" jj-log-default)
   ("c" "current" jj-log-current)
   ("a" "all" jj-log-all)
   ("o" "other" jj-log-other)])

(defun jj-log-read-revisions (&rest _)
  "Read revisions for jj log."
  (read-string "Revisions: " "@"))

(transient-define-argument jj-log:--limit ()
  :description "Limit number of commits"
  :class 'transient-option
  :argument "--limit="
  :reader #'transient-read-number-N+)

;;; Log Commands

;;;###autoload
(defun jj-log-current (&optional args)
  "Show log for current revision and ancestors."
  (interactive (list (transient-args 'jj-log)))
  (jj-log-setup-buffer "@" args))

;;;###autoload  
(defun jj-log-default (&optional args)
  "Show default log view."
  (interactive (list (transient-args 'jj-log)))
  ;; this is the builtin revsets.log value $(jj config get revsets.log)
  ;; TODO: we should read this (or edit jj-log-setup-buffer to use a default revset rather than require one)
  (jj-log-setup-buffer "present(@) | ancestors(immutable_heads().., 2) | present(trunk())" args))

(defun jj-log-all (&optional args)
  "Show log for all revisions."
  (interactive (list (transient-args 'jj-log)))
  (jj-log-setup-buffer "::" args))

;;;###autoload
(defun jj-log-other (revisions &optional args)
  "Show log for REVISIONS."
  (interactive (list (jj-log-read-revisions)
                     (transient-args 'jj-log)))
  (jj-log-setup-buffer revisions args))

;;; Buffer Setup

(defun jj-log-setup-buffer (revs args)
  "Set up log buffer for REVS with ARGS."
  (require 'magit)
  (magit-setup-buffer #'jj-log-mode nil
    (jj-log-buffer-revs revs)
    (jj-log-buffer-args args)))

;;; Buffer Contents

(defun jj-log-refresh-buffer ()
  "Refresh the current jj-log buffer."
  (magit-insert-section (logbuf)
    (jj-log--insert-log jj-log-buffer-revs jj-log-buffer-args)))

(defun jj-log--insert-log (revs args)
  "Insert log for REVS with ARGS."
  (let ((default-directory (jj-log--find-root)))
    (unless default-directory
      (user-error "Not in a jj repository"))
    (let ((cmd-args (append 
                     ;; First add any args from transient
                     (if (listp args) args (list args))
                     ;; Then add -r flag and revisions
                     (when revs (list "-r" revs)))))
      (apply #'jj-log--run-and-wash #'jj-log-wash-log
             jj-log-executable "log" 
             (delq nil (mapcar (lambda (x) (if (stringp x) x nil)) cmd-args))))))

(defun jj-log--run-and-wash (washer program &rest args)
  "Run PROGRAM with ARGS and apply WASHER to output."
  (let ((exit-code (apply #'call-process program nil t nil args)))
    (unless (zerop exit-code)
      (user-error "%s exited with code %d" program exit-code))
    (goto-char (point-min))
    (funcall washer)))

(defun jj-log-wash-log ()
  "Parse jj log output into sections."
  (magit-wash-sequence #'jj-log-wash-commit))

(defun jj-log-wash-commit ()
  "Parse a single commit from jj log output."
  (when (looking-at jj-log-commit-re)
    (let ((change-id (match-string 2))
          (line-start (point))
          (commit-line (buffer-substring (point) (progn (end-of-line) (point)))))
      (forward-line 1)
      ;; Look for description on next line
      (let ((description-line ""))
        (when (and (not (eobp)) 
                   (looking-at "^[│ ]*\\(.+\\)$"))
          (setq description-line (buffer-substring (point) (progn (end-of-line) (point))))
          (forward-line 1))
        ;; Delete the processed lines
        (delete-region line-start (point))
        ;; Insert the commit section preserving original formatting but adding colors
        (magit-insert-section (commit change-id)
          (insert (jj-log--colorize-line commit-line) "\n")
          (when (not (string-empty-p description-line))
            (insert (jj-log--colorize-line description-line) "\n")))
        t))))

(defun jj-log--colorize-line (line)
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
                            ((string= symbol "@") 'jj-log-current-commit)
                            ((string= symbol "○") 'jj-log-mutable-commit)
                            ((string= symbol "◆") 'jj-log-immutable-commit)
                            ((string= symbol "~") 'jj-log-elided)))))
    
    ;; Color graph lines (but not the commit symbols we just colored)
    (goto-char (point-min))
    (while (re-search-forward "\\([│├─┬┐┘┌╮╯╰╭╲╱┴┤]\\)" nil t)
      (put-text-property (match-beginning 1) (match-end 1) 
                         'font-lock-face 'jj-log-graph))
    
    ;; Color change ID (8-char hex after graph)
    (goto-char (point-min))
    (when (re-search-forward "\\b\\([a-z0-9]\\{8\\}\\)\\b" nil t)
      (put-text-property (match-beginning 1) (match-end 1)
                         'font-lock-face 'magit-hash))
    
    ;; Color email addresses
    (goto-char (point-min))
    (when (re-search-forward "\\([a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]\\{2,\\}\\)" nil t)
      (put-text-property (match-beginning 1) (match-end 1)
                         'font-lock-face 'magit-log-author))
    
    ;; Color dates
    (goto-char (point-min))
    (when (re-search-forward "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)" nil t)
      (put-text-property (match-beginning 1) (match-end 1)
                         'font-lock-face 'magit-log-date))
    
    ;; Color git hash at end of line
    (goto-char (point-min))
    (when (re-search-forward "\\b\\([a-f0-9]\\{7,40\\}\\)\\s-*$" nil t)
      (put-text-property (match-beginning 1) (match-end 1)
                         'font-lock-face 'magit-hash))
    
    ;; Color bookmarks and refs - look for patterns after date but before hash
    ;; This captures things like: main, spencer/feature-branch, git_head(), push-xyz*, @origin, etc.
    (goto-char (point-min))
    (when (re-search-forward "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}" nil t)
      ;; Look for bookmark-like strings after the date
      (while (re-search-forward "\\b\\([a-zA-Z0-9_/@*()-][a-zA-Z0-9_/@*().-]*[a-zA-Z0-9_/@*().-]\\|[a-zA-Z0-9_/@*()-]\\)\\b" 
                                (line-end-position) t)
        (let ((candidate (match-string 1)))
          ;; Skip if it looks like a hash (all lowercase hex) or change ID with ? or *
          (unless (or (and (>= (length candidate) 7)
                           (string-match-p "^[a-f0-9]+$" candidate))
                      ;; Skip if it looks like a JJ change ID (contains ?? or * suffix)
                      (string-match-p "\\?\\?$\\|\\*$" candidate)
                      ;; Skip if it looks like a change ID with ? or * characters
                      (and (>= (length candidate) 8)
                           (string-match-p "^[a-z0-9?*]+$" candidate)))
            (put-text-property (match-beginning 1) (match-end 1)
                               'font-lock-face 'magit-branch-remote)))))
    
    (buffer-string)))

;;; Commit Viewing

(defun jj-show-commit (&optional commit)
  "Show the commit at point or COMMIT."
  (interactive)
  (let ((commit (or commit 
                    (magit-section-value-if 'commit)
                    (user-error "No commit at point"))))
    (jj-show-commit-buffer commit)))

(defun jj-show-commit-buffer (commit)
  "Show COMMIT in a dedicated buffer."
  (magit-setup-buffer #'jj-revision-mode nil
    (jj-revision-buffer-commit commit)))

(defvar-keymap jj-revision-mode-map
  :doc "Keymap for `jj-revision-mode'."
  :parent magit-mode-map)

(define-derived-mode jj-revision-mode magit-mode "JJ Revision"
  "Mode for viewing a single Jujutsu commit."
  :group 'jj-log
  (magit-hack-dir-local-variables)
  (add-hook 'magit-refresh-buffer-hook #'jj-revision-refresh-buffer nil t))

(defvar-local jj-revision-buffer-commit nil
  "The commit shown in the current revision buffer.")

(defun jj-revision-refresh-buffer ()
  "Refresh the current jj-revision buffer."
  (jj-revision--insert-commit jj-revision-buffer-commit)
  ;; Move to buffer beginning to show the header
  (goto-char (point-min)))

(defun jj-revision--insert-commit (commit)
  "Insert commit COMMIT details and diff."
  (let ((default-directory (jj-log--find-root)))
    (unless default-directory
      (user-error "Not in a jj repository"))
    
    ;; First section: Commit Info
    (magit-insert-section (commit-info)
      (magit-insert-heading "Commit Info")
      ;; Get commit info from jj show
      (jj-log--run-and-wash #'jj-revision-wash-show
                            jj-log-executable "show" commit)
      ;; Ensure there's content in the section
      (when (= (point) (magit-section-start (magit-current-section)))
        (insert "  [Commit info would go here]\n")))
    
    (insert "\n")
    
    ;; Second section: Diff  
    (magit-insert-section (commit-diff)
      (magit-insert-heading "Diff")
      (jj-log--run-and-wash #'jj-revision-wash-diff
                            jj-log-executable "diff" "-r" commit))))

(defun jj-revision-wash-show ()
  "Parse jj show output into magit-style sections."
  (goto-char (point-min))
  (let (commit-id change-id bookmarks author committer description)
    
    ;; Parse the metadata
    (when (re-search-forward "^Commit ID: \\(.+\\)$" nil t)
      (setq commit-id (match-string 1)))
    (when (re-search-forward "^Change ID: \\(.+\\)$" nil t)
      (setq change-id (match-string 1)))
    (when (re-search-forward "^Bookmarks: \\(.+\\)$" nil t)
      (setq bookmarks (match-string 1)))
    (when (re-search-forward "^Author   : \\(.+\\)$" nil t)
      (setq author (match-string 1)))
    (when (re-search-forward "^Committer: \\(.+\\)$" nil t)
      (setq committer (match-string 1)))
    
    ;; Extract description (everything between blank line and file changes)
    (goto-char (point-min))
    (when (re-search-forward "^$" nil t) ; Find first blank line
      (forward-line 1)
      (let ((desc-start (point)))
        (if (re-search-forward "^\\(Modified\\|Added\\|Deleted\\)" nil t)
            (progn
              (goto-char (match-beginning 0))
              (skip-chars-backward " \t\n")
              (setq description (string-trim (buffer-substring desc-start (point)))))
          ;; If no file changes, take rest of buffer
          (setq description (string-trim (buffer-substring desc-start (point-max)))))))
    
    ;; Clear buffer and insert magit-style format
    (delete-region (point-min) (point-max))

    ;; Insert change ID
    (when change-id
      (insert "Change:     " (propertize change-id 'font-lock-face 'magit-hash) "\n"))

    (when commit-id
      (insert "Commit:     " (propertize commit-id 'font-lock-face 'magit-hash) "\n"))

    ;; Insert author/committer info
    (when author
      (insert "Author:     " (propertize author 'font-lock-face 'magit-log-author) "\n"))
    (when committer
      (insert "Committer:  " (propertize committer 'font-lock-face 'magit-log-author) "\n"))

    ;; Insert header line with bookmarks/refs and commit ID
    (when bookmarks
      (insert "Bookmarks:\n" (propertize bookmarks 'font-lock-face 'magit-branch-remote) "\n"))
    
    (insert "\n")
    
    ;; Insert commit message
    (when description
      (insert description "\n"))))

(defun jj-revision-wash-diff ()
  "Parse jj diff output."
  ;; Just insert the diff output as-is for now
  (goto-char (point-min))
  (let ((content (buffer-string)))
    (delete-region (point-min) (point-max))
    (insert content)))

;;; Section Keymaps

(defvar-keymap jj-commit-section-map
  :doc "Keymap for `commit' sections in jj-log buffers."
  "<remap> <magit-visit-thing>" #'jj-show-commit
  "RET" #'jj-show-commit
  "l"   #'jj-log
  "e"   #'jj-edit
  "n"   #'jj-new
  "c"   #'jj-log-commit
  "d"   #'jj-diff-at-point
  "R"   #'jj-rebase-interactive
  "r"   #'jj-rebase-prompt
  "u"   #'jj-quick-undo
  "g"   #'magit-refresh
  "<1>" (magit-menu-item "Show commit" #'jj-show-commit))

;; Register the section map with magit
(put 'commit :magit-section-keymap 'jj-commit-section-map)

;;; Integration with magit-mode

(add-hook 'jj-log-mode-hook #'magit-load-config-extensions)

;;; Integration with Evil mode

(eval-after-load 'evil
  '(progn
     ;; Ensure n key binds to jj-new in jj-log-mode instead of evil-ex-search-next
     (evil-define-key 'normal jj-log-mode-map
       "n" #'jj-new
       "c" #'jj-log-commit
       "e" #'jj-edit
       "D" #'jj-log-describe
       "d" #'jj-diff-at-point
       "R" #'jj-rebase-interactive
       "r" #'jj-rebase-prompt
       "u" #'jj-quick-undo)
     
     ;; Also ensure section-specific bindings work
     (evil-define-key 'normal jj-commit-section-map
       "n" #'jj-new
       "c" #'jj-log-commit
       "e" #'jj-edit
       "D" #'jj-log-describe
       "d" #'jj-diff-at-point
       "R" #'jj-rebase-interactive
       "r" #'jj-rebase-prompt
       "u" #'jj-quick-undo)
     
     ;; For rebase selection mode
     (evil-define-key 'normal jj-rebase-select-mode-map
       "n" #'jj-new
       (kbd "RET") #'jj-rebase-select-destination
       (kbd "C-c C-c") #'jj-rebase-select-destination
       "q" #'jj-rebase-select-quit
       (kbd "C-c C-k") #'jj-rebase-select-quit)))

;;; Interactive Commands
(defun jj-log-describe (&optional revision)
  "Edit the description of REVISION (defaults to @).
Opens the description in an Emacs buffer for editing."
  (interactive)
  (jj-describe revision)
  (magit-refresh))

(defun jj-log-commit (&optional revision)
  "Describe commit at REVISION and create new on top."
  (interactive)
  (jj-describe revision)
  (jj-new revision)
  (jj-log-default)
  (magit-refresh))

(defun jj--run-command (command &rest args)
  "Run jj COMMAND with ARGS and display result."
  (let ((default-directory (jj-log--find-root)))
    (unless default-directory
      (user-error "Not in a jj repository"))
    (message "Running: jj %s %s" command (string-join args " "))
    (let ((result (apply #'call-process jj-log-executable nil "*jj-output*" nil command args)))
      (if (zerop result)
          (progn
            (message "jj %s completed successfully" command)
            (magit-refresh))
        (with-current-buffer "*jj-output*"
          (user-error "jj %s failed: %s" command (buffer-string)))))))

(defun jj-edit (&optional commit)
  "Run jj edit on COMMIT at point."
  (interactive)
  (let ((change-id (or commit 
                       (magit-section-value-if 'commit)
                       (user-error "No commit at point"))))
    (jj--run-command "edit" change-id)))

(defun jj-new (&optional commit)
  "Run jj new on COMMIT at point."
  (interactive)
  (let ((change-id (or commit 
                       (magit-section-value-if 'commit)
                       (user-error "No commit at point"))))
    (jj--run-command "new" change-id)))

(defun jj-quick-undo (&optional args)
  "Run jj undo to undo the immediately previous change. ignore ARGS."
  (interactive)
  ;; TODO: for now just runs default, not configurable...
  (jj--run-command "undo"))

(defun jj-rebase-prompt (&optional commit)
  "Run jj rebase -r on COMMIT at point with prompted destination."
  (interactive)
  (let ((source-change-id (or commit 
                              (magit-section-value-if 'commit)
                              (user-error "No commit at point")))
        (destination (read-string "Rebase destination: ")))
    (jj--run-command "rebase" "-r" source-change-id "-d" destination)))

(defvar-local jj-rebase-source-commit nil
  "The source commit for interactive rebase.")

(defvar-keymap jj-rebase-select-mode-map
  :doc "Keymap for jj rebase destination selection."
  :parent jj-log-mode-map
  "RET" #'jj-rebase-select-destination
  "C-c C-c" #'jj-rebase-select-destination
  "q" #'jj-rebase-select-quit
  "C-c C-k" #'jj-rebase-select-quit)

(define-derived-mode jj-rebase-select-mode jj-log-mode "JJ Rebase Select"
  "Mode for selecting rebase destination.

\\<jj-rebase-select-mode-map>
Type \\[jj-rebase-select-destination] to select destination at point.
Type \\[jj-rebase-select-quit] to abort rebase selection."
  :group 'jj-log)

(defun jj-rebase-select-setup-buffer (source-commit revs args)
  "Set up rebase selection buffer."
  (require 'magit)
  (magit-setup-buffer #'jj-rebase-select-mode nil
    (jj-log-buffer-revs revs)
    (jj-log-buffer-args args))
  (setq jj-rebase-source-commit source-commit)
  (magit-set-header-line-format
   (format "Select destination for rebasing %s (RET to confirm, q to quit)"
           (substring source-commit 0 8))))

(defun jj-rebase-select-destination ()
  "Select commit at point as rebase destination."
  (interactive)
  (let ((destination (magit-section-value-if 'commit)))
    (unless destination
      (user-error "No commit at point"))
    (when (string= destination jj-rebase-source-commit)
      (user-error "Cannot rebase commit onto itself"))
    (magit-mode-bury-buffer 'kill)
    (jj--run-command "rebase" "-r" jj-rebase-source-commit "-d" destination)))

(defun jj-rebase-select-quit ()
  "Quit rebase selection."
  (interactive)
  (magit-mode-bury-buffer 'kill)
  (message "Rebase cancelled"))

(defvar jj-rebase-current-commit nil
  "The current commit for rebase operations.")

(defun jj-rebase-read-revset (&rest _)
  "Read a revset for jj rebase."
  (read-string "Revset: " (or jj-rebase-current-commit "@")))

(defun jj-rebase-read-destination (&rest _)
  "Read destination for jj rebase."
  (read-string "Destination: " "@"))

(defun jj-rebase-execute-args (args)
  "Execute jj rebase with ARGS."
  (interactive (list (transient-args 'jj-rebase-interactive)))
  (let ((source (transient-arg-value "--source=" args))
        (branch (transient-arg-value "--branch=" args))
        (revisions (transient-arg-value "--revisions=" args))
        (destination (transient-arg-value "--destination=" args))
        (insert-after (transient-arg-value "--insert-after=" args))
        (insert-before (transient-arg-value "--insert-before=" args))
        (other-flags (seq-filter (lambda (arg) 
                                   (not (string-match "^--\\(source\\|branch\\|revisions\\|destination\\|insert-after\\|insert-before\\)=" arg)))
                                 args)))
    (let ((cmd-args (append
                     (when source (list "--source" source))
                     (when branch (list "--branch" branch))
                     (when revisions (list "--revisions" revisions))
                     (when destination (list "--destination" destination))
                     (when insert-after (list "--insert-after" insert-after))
                     (when insert-before (list "--insert-before" insert-before))
                     other-flags)))
      (if cmd-args
          (apply #'jj--run-command "rebase" cmd-args)
        (user-error "Must specify rebase source and destination")))))

(defun jj-rebase-source-destination ()
  "Rebase source commit to destination."
  (interactive)
  (let ((source (or jj-rebase-current-commit
                    (user-error "No commit selected")))
        (destination (jj-rebase-read-destination)))
    (jj--run-command "rebase" "--source" source "--destination" destination)))

(defun jj-rebase-branch-destination ()
  "Rebase branch to destination."
  (interactive)
  (let ((branch (or jj-rebase-current-commit
                    (user-error "No commit selected")))
        (destination (jj-rebase-read-destination)))
    (jj--run-command "rebase" "--branch" branch "--destination" destination)))

(defun jj-rebase-revisions-destination ()
  "Rebase specific revisions to destination."
  (interactive)
  (let ((revisions (jj-rebase-read-revset))
        (destination (jj-rebase-read-destination)))
    (jj--run-command "rebase" "--revisions" revisions "--destination" destination)))

;;;###autoload (autoload 'jj-rebase-interactive "jj-log" nil t)
(transient-define-prefix jj-rebase-interactive (&optional commit)
  "Rebase commits interactively with jj."
  :class 'transient-prefix
  ["Revision Selection"
   ("-s" "Source (with descendants)" "--source=" :reader jj-rebase-read-revset)
   ("-b" "Branch (relative to destination)" "--branch=" :reader jj-rebase-read-revset) 
   ("-r" "Revisions (without descendants)" "--revisions=" :reader jj-rebase-read-revset)]
  ["Destination"
   ("-d" "Destination" "--destination=" :reader jj-rebase-read-destination)
   ("-A" "Insert after" "--insert-after=" :reader jj-rebase-read-destination)
   ("-B" "Insert before" "--insert-before=" :reader jj-rebase-read-destination)]
  ["Options"
   ("--skip-emptied" "Skip emptied commits" "--skip-emptied")
   ("--keep-divergent" "Keep divergent commits" "--keep-divergent")]
  ["Actions"
   ("s" "Source → Destination" jj-rebase-source-destination)
   ("b" "Branch → Destination" jj-rebase-branch-destination)  
   ("r" "Revisions → Destination" jj-rebase-revisions-destination)
   ("RET" "Rebase with args" jj-rebase-execute-args)]
  (interactive (list (magit-section-value-if 'commit)))
  (setq jj-rebase-current-commit (or commit 
                                     (magit-section-value-if 'commit)
                                     "@"))
  (transient-setup 'jj-rebase-interactive))

;;; Auto-load helpers

;;;###autoload
(defun jj-log-enable ()
  "Enable jj-log integration."
  (interactive)
  (message "jj-log ready. Use M-x jj-log to view commit history."))

;; Auto-enable if in a jj repository
;;;###autoload
(when (and (boundp 'after-init-hook)
           (fboundp 'jj-log--find-root)
           (jj-log--find-root))
  (add-hook 'after-init-hook #'jj-log-enable))

(provide 'jj-log)

;;; jj-log.el ends here
