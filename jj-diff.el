;;; jj-diff.el --- View Jujutsu diff output -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Spencer Nystrom
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (magit "3.0.0") (transient "0.3.0"))
;; Keywords: vc, tools
;; URL: https://github.com/snystrom/jitte

;;; Commentary:

;; This package provides a Magit-style diff viewer for Jujutsu (jj).
;; It allows you to view diffs with customizable options through a
;; transient menu interface, supporting all jj diff command-line flags.

;;; Code:

(require 'magit)
(require 'transient)

;;; Configuration

(defgroup jj-diff nil
  "View Jujutsu diff output."
  :prefix "jj-diff-"
  :group 'tools)

(defcustom jj-diff-executable "jj"
  "The Jujutsu executable."
  :type 'string
  :group 'jj-diff)

(defcustom jj-diff-mode-hook nil
  "Hook run after entering jj-diff mode."
  :group 'jj-diff
  :type 'hook)

(defcustom jj-diff-default-args '("--color-words")
  "Default arguments for jj diff commands."
  :group 'jj-diff
  :type '(repeat string))

;;; Helper Functions

(defun jj-diff--find-root ()
  "Find the root of the current jj repository."
  (let ((dir (locate-dominating-file default-directory ".jj")))
    (when dir
      (expand-file-name dir))))

;;; Faces

(defface jj-diff-added
  '((t :inherit diff-added))
  "Face for added lines in jj diff output."
  :group 'jj-diff)

(defface jj-diff-removed
  '((t :inherit diff-removed))
  "Face for removed lines in jj diff output."
  :group 'jj-diff)

(defface jj-diff-context
  '((t :inherit diff-context))
  "Face for context lines in jj diff output."
  :group 'jj-diff)

(defface jj-diff-hunk-header
  '((t :inherit diff-hunk-header))
  "Face for hunk headers in jj diff output."
  :group 'jj-diff)

(defface jj-diff-file-header
  '((t :inherit diff-file-header))
  "Face for file headers in jj diff output."
  :group 'jj-diff)

;;; Variables

(defvar-local jj-diff-buffer-args nil
  "Arguments used to generate the current diff buffer.")

(defvar-local jj-diff-buffer-revisions nil
  "Revisions compared in the current diff buffer.")

;;; Mode Definition

(defvar-keymap jj-diff-mode-map
  :doc "Keymap for `jj-diff-mode'."
  :parent magit-mode-map
  "d" #'jj-diff-at-point
  "g" #'magit-refresh
  "q" #'magit-log-bury-buffer)

(define-derived-mode jj-diff-mode magit-mode "JJ Diff"
  "Mode for looking at Jujutsu diff output.

\\<jj-diff-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[jj-diff-at-point] to change diff arguments.

\\{jj-diff-mode-map}"
  :group 'jj-diff
  (magit-hack-dir-local-variables)
  (add-hook 'magit-refresh-buffer-hook #'jj-diff-refresh-buffer nil t))

;;; Transient Interface

;;;###autoload (autoload 'jj-diff "jj-diff" nil t)
(transient-define-prefix jj-diff ()
  "Show Jujutsu diff with customizable options."
  :class 'transient-prefix
  ["Revision Selection"
   ("-r" "Revisions" "--revisions=" :reader jj-diff-read-revisions)
   ("-f" "From revision" "--from=" :reader jj-diff-read-revision)
   ("-t" "To revision" "--to=" :reader jj-diff-read-revision)]
  ["Output Format"
   ("-s" "Summary only" "--summary")
   ("--stat" "Statistics" "--stat")
   ("--name-only" "Names only" "--name-only")
   ("--git" "Git format" "--git")
   ("--color-words" "Color words" "--color-words")
   ("--types" "Show type changes" "--types")]
  ["Whitespace"
   ("-w" "Ignore all whitespace" "-w")
   ("-b" "Ignore space changes" "-b")]
  ["Actions"
   ("d" "Show diff" jj-diff-show)
   ("w" "Working copy diff" jj-diff-working-copy)
   ("c" "Between commits" jj-diff-between-revisions)])

;;;###autoload (autoload 'jj-diff-at-point "jj-diff" nil t)
(transient-define-prefix jj-diff-at-point ()
  "Show Jujutsu diff with commit at point as default."
  :class 'transient-prefix
  ["Revision Selection"
   ("-r" "Revisions" "--revisions=" :reader jj-diff-read-revisions-with-context)
   ("-f" "From revision" "--from=" :reader jj-diff-read-revision-with-context)
   ("-t" "To revision" "--to=" :reader jj-diff-read-revision-with-context)]
  ["Output Format"
   ("-s" "Summary only" "--summary")
   ("--stat" "Statistics" "--stat")
   ("--name-only" "Names only" "--name-only")
   ("--git" "Git format" "--git")
   ("--color-words" "Color words" "--color-words")
   ("--types" "Show type changes" "--types")]
  ["Whitespace"
   ("-w" "Ignore all whitespace" "-w")
   ("-b" "Ignore space changes" "-b")]
  ["Actions"
   ("d" "Show diff" jj-diff-show)
   ("w" "Working copy diff" jj-diff-working-copy)
   ("c" "Between commits" jj-diff-between-revisions)])

(defun jj-diff-read-revisions (&rest _)
  "Read revisions for jj diff."
  (read-string "Revisions: " "@"))

(defun jj-diff-read-revision (&rest _)
  "Read a single revision for jj diff."
  (read-string "Revision: " "@"))

(defun jj-diff-read-revisions-with-context (&rest _)
  "Read revisions for jj diff, using commit at point as default if available."
  (let ((default (or (magit-section-value-if 'commit) "@")))
    (read-string "Revisions: " default)))

(defun jj-diff-read-revision-with-context (&rest _)
  "Read a single revision for jj diff, using commit at point as default if available."
  (let ((default (or (magit-section-value-if 'commit) "@")))
    (read-string "Revision: " default)))

;;; Buffer Setup

(defun jj-diff-setup-buffer (args &optional revisions)
  "Set up diff buffer with ARGS and optional REVISIONS."
  (require 'magit)
  (magit-setup-buffer #'jj-diff-mode nil
    (jj-diff-buffer-args args)
    (jj-diff-buffer-revisions revisions)))

;;; Buffer Contents

(defun jj-diff-refresh-buffer ()
  "Refresh the current jj-diff buffer."
  (magit-insert-section (diffbuf)
    (jj-diff--insert-diff jj-diff-buffer-args jj-diff-buffer-revisions)))

(defun jj-diff--insert-diff (args revisions)
  "Insert diff with ARGS and optional REVISIONS."
  (let ((default-directory (jj-diff--find-root)))
    (unless default-directory
      (user-error "Not in a jj repository"))
    (let ((cmd-args (append 
                     (if (listp args) args (list args))
                     (when revisions (list revisions)))))
      (apply #'jj-diff--run-and-wash #'jj-diff-wash-diff
             jj-diff-executable "diff" 
             (delq nil (mapcar (lambda (x) (if (stringp x) x nil)) cmd-args))))))

(defun jj-diff--run-and-wash (washer program &rest args)
  "Run PROGRAM with ARGS and apply WASHER to output."
  (let ((exit-code (apply #'call-process program nil t nil args)))
    (unless (zerop exit-code)
      (user-error "%s exited with code %d" program exit-code))
    (goto-char (point-min))
    (funcall washer)))

(defun jj-diff-wash-diff ()
  "Parse and colorize jj diff output."
  (goto-char (point-min))
  (while (not (eobp))
    (let ((line-start (point)))
      (end-of-line)
      (let ((line (buffer-substring line-start (point))))
        (cond
         ;; File headers (start with "--- " or "+++ " or "diff ")
         ((string-match-p "^\\(---|\\+\\+\\+|diff \\)" line)
          (put-text-property line-start (point) 'font-lock-face 'jj-diff-file-header))
         ;; Hunk headers (start with "@@")
         ((string-match-p "^@@" line)
          (put-text-property line-start (point) 'font-lock-face 'jj-diff-hunk-header))
         ;; Added lines (start with "+")
         ((string-match-p "^\\+" line)
          (put-text-property line-start (point) 'font-lock-face 'jj-diff-added))
         ;; Removed lines (start with "-")
         ((string-match-p "^-" line)
          (put-text-property line-start (point) 'font-lock-face 'jj-diff-removed))
         ;; Context lines (everything else)
         (t
          (put-text-property line-start (point) 'font-lock-face 'jj-diff-context))))
      (forward-line 1))))

;;; Interactive Commands

;;;###autoload
(defun jj-diff-show (&optional args)
  "Show diff with specified ARGS."
  (interactive (list (transient-args 'jj-diff)))
  (let ((revisions (transient-arg-value "--revisions=" args))
        (from-rev (transient-arg-value "--from=" args))
        (to-rev (transient-arg-value "--to=" args))
        (filtered-args (seq-remove (lambda (arg) 
                                     (string-match-p "^--\\(revisions\\|from\\|to\\)=" arg)) 
                                   args)))
    (cond
     ((and from-rev to-rev)
      (jj-diff-setup-buffer (append filtered-args (list "--from" from-rev "--to" to-rev))))
     (revisions
      (jj-diff-setup-buffer (append filtered-args (list "--revisions" revisions))))
     (from-rev
      (jj-diff-setup-buffer (append filtered-args (list "--from" from-rev))))
     (to-rev
      (jj-diff-setup-buffer (append filtered-args (list "--to" to-rev))))
     (t
      (jj-diff-setup-buffer (if filtered-args filtered-args jj-diff-default-args))))))

;;;###autoload
(defun jj-diff-working-copy (&optional args)
  "Show diff of working copy changes."
  (interactive (list (transient-args 'jj-diff)))
  (jj-diff-setup-buffer (append args (list "--revisions" "@"))))

;;;###autoload
(defun jj-diff-between-revisions (from to &optional args)
  "Show diff between FROM and TO revisions."
  (interactive (list (read-string "From revision: " "@~")
                     (read-string "To revision: " "@")
                     (transient-args 'jj-diff)))
  (jj-diff-setup-buffer (append args (list "--from" from "--to" to))))

;;; Integration with Evil mode

(eval-after-load 'evil
  '(progn
     (evil-define-key 'normal jj-diff-mode-map
       "d" #'jj-diff-at-point
       "g" #'magit-refresh
       "q" #'magit-log-bury-buffer)))

;;; Auto-load helpers

;;;###autoload
(defun jj-diff-enable ()
  "Enable jj-diff integration."
  (interactive)
  (message "jj-diff ready. Use M-x jj-diff to view diffs."))

;; Auto-enable if in a jj repository
;;;###autoload
(when (and (boundp 'after-init-hook)
           (fboundp 'jj-diff--find-root)
           (jj-diff--find-root))
  (add-hook 'after-init-hook #'jj-diff-enable))

(provide 'jj-diff)

;;; jj-diff.el ends here