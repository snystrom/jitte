;n;; jj-describe.el --- Edit Jujutsu commit descriptions -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (with-editor "3.0.0"))
;; Keywords: vc, tools
;; URL: 

;;; Commentary:

;; This package provides Magit-style commit message editing for Jujutsu (jj).
;; It allows you to run `jj describe` and edit the commit description in an
;; Emacs buffer with C-c C-c to finish and C-c C-k to cancel.

;;; Code:

(require 'with-editor)

;;; Configuration

(defgroup jj-describe nil
  "Edit Jujutsu commit descriptions."
  :prefix "jj-describe-"
  :group 'tools)

(defcustom jj-describe-executable "jj"
  "The Jujutsu executable."
  :type 'string
  :group 'jj-describe)

(defcustom jj-describe-major-mode #'text-mode
  "Major mode used to edit Jujutsu commit descriptions.
The major mode configured here is turned on by the minor mode
`jj-describe-mode'."
  :group 'jj-describe
  :type '(radio (function-item text-mode)
                (function-item markdown-mode)
                (function-item org-mode)
                (function-item fundamental-mode)
                (function :tag "Another mode")))

(defcustom jj-describe-setup-hook nil
  "Hook run at the end of `jj-describe-setup'."
  :group 'jj-describe
  :type 'hook)

;;; Variables

(defconst jj-describe-filename-regexp "/jj-describe-.*\\.txt\\'"
  "Regexp matching Jujutsu description message files.")

(defvar jj-describe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'with-editor-finish)
    (define-key map (kbd "C-c C-k") 'with-editor-cancel)
    map)
  "Keymap used by `jj-describe-mode'.")

;;; Helper Functions

(defun jj-describe--find-root ()
  "Find the root of the current jj repository."
  (let ((dir (locate-dominating-file default-directory ".jj")))
    (when dir
      (expand-file-name dir))))

(defun jj-describe--run-with-editor (&rest args)
  "Run jj with ARGS, setting up Emacs as the editor."
  ;; Enable global mode to catch the temp file
  (unless global-jj-describe-mode
    (global-jj-describe-mode 1))

  ;; Run jj describe with editor setup (similar to magit's approach)
  (with-editor* "JJ_EDITOR"
    (let ((process (apply #'start-process "jj-describe" nil
                         jj-describe-executable args)))
      (set-process-sentinel process #'jj-describe--process-sentinel)
      (message "Running jj %s..." (string-join args " "))
      process)))

(defun jj-describe--success-cleanup ()
  "Close describe buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (message "jj describe completed"))

(defun jj-describe--process-sentinel (process event)
  "Handle completion of jj describe PROCESS with EVENT."
  (when (memq (process-status process) '(exit signal))
    (let ((exit-code (process-exit-status process)))
      (if (= exit-code 0)
          (jj-describe--success-cleanup)
        (message "jj describe failed with exit code %d" exit-code)))))

;;; Mode Definition

(define-minor-mode jj-describe-mode
  "Minor mode for editing Jujutsu commit descriptions."
  :lighter " JJ-Desc"
  :keymap jj-describe-mode-map
  :group 'jj-describe)

(define-minor-mode global-jj-describe-mode
  "Global minor mode for editing Jujutsu commit descriptions.

This global mode arranges for `jj-describe-setup' to be called
when a Jujutsu description file is opened."
  :group 'jj-describe
  :type 'boolean
  :global t
  :init-value nil
  (cond
   (global-jj-describe-mode
    (add-hook 'find-file-hook #'jj-describe-setup-check-buffer))
   (t
    (remove-hook 'find-file-hook #'jj-describe-setup-check-buffer))))

;;; Setup Functions

(defun jj-describe-setup-check-buffer ()
  "Check if current buffer is a Jujutsu description file and set it up."
  (when (and buffer-file-name
             (string-match-p jj-describe-filename-regexp buffer-file-name))
    (jj-describe-setup)))

(defun jj-describe-setup ()
  "Set up current buffer for editing Jujutsu commit descriptions."
  ;; Set up the major mode
  (when jj-describe-major-mode
    (funcall jj-describe-major-mode))
  
  ;; Enable with-editor-mode for C-c C-c and C-c C-k
  (unless with-editor-mode
    (with-editor-mode 1))
  
  ;; Enable our minor mode
  (jj-describe-mode 1)
  

  ;; Set up evil-mode integration if available
  (jj-describe--setup-evil)
  
  ;; Run setup hooks
  (run-hooks 'jj-describe-setup-hook)
  
  ;; Show usage message
  (message "Type C-c C-c to finish, C-c C-k to cancel"))

(defun jj-describe--setup-evil ()
  "Set up evil-mode integration for jj-describe buffers."
  (when (and (boundp 'evil-mode) evil-mode)
    ;; Make buffer-local evil ex-command map
    (make-local-variable 'evil-ex-commands)
    ;; Add our commands to the buffer-local map
    (evil-ex-define-cmd "wq" 'with-editor-finish)
    (evil-ex-define-cmd "q!" 'with-editor-cancel) 
    (evil-ex-define-cmd "x" 'with-editor-finish)
    ;; Start in insert state for commit editing
    (when (fboundp 'evil-insert-state)
      (evil-insert-state))))


;;; Interactive Commands

;;;###autoload
(defun jj-describe (&optional revision)
  "Edit the description of REVISION (defaults to @).
Opens the description in an Emacs buffer for editing."
  (interactive)
  (let ((rev (or revision "@"))
        (default-directory (jj-describe--find-root)))
    (unless default-directory
      (user-error "Not in a jj repository"))
    (jj-describe--run-with-editor "describe" rev)))

;;;###autoload
(defun jj-describe-revision (revision)
  "Edit the description of a specific REVISION.
Prompts for the revision to describe."
  (interactive "sRevision to describe: ")
  (jj-describe revision))


;;; Autoload and Activation

;;;###autoload
(defun jj-describe-enable ()
  "Enable jj-describe integration."
  (interactive)
  (global-jj-describe-mode 1)
  (message "jj-describe integration enabled"))

;; Auto-enable if in a jj repository
;;;###autoload
(when (and (boundp 'after-init-hook)
           (fboundp 'jj-describe--find-root)
           (jj-describe--find-root))
  (add-hook 'after-init-hook #'jj-describe-enable))

(provide 'jj-describe)

;;; jj-describe.el ends here
