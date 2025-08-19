;;; jitte-mode.el --- Create and refresh Jitte buffers  -*- lexical-binding:t -*-

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

;; This library implements the abstract major-mode `jitte-mode' from
;; which almost all other Jitte major-modes derive.  The code in here
;; is mostly concerned with creating and refreshing Jitte buffers.

;;; Code:

(require 'jitte-base)

(require 'magit-section)
(require 'transient)
(require 'help-mode)
(require 'browse-url)

;;; Options

(defcustom jitte-mode-hook nil
  "Hook run when entering a mode derived from Jitte mode."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-modes
  :type 'hook)

(defcustom jitte-setup-buffer-hook nil
  "Hook run by `jitte-setup-buffer'.

This is run right after displaying the buffer and right before
generating or updating its content.  `jitte-mode-hook' and other,
more specific, `jitte-mode-*-hook's on the other hand are run
right before displaying the buffer.  Usually one of these hooks
should be used instead of this one."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-modes
  :type 'hook)

(defcustom jitte-pre-refresh-hook nil
  "Hook run before refreshing in `jitte-refresh'.

This hook, or `jitte-post-refresh-hook', should be used
for functions that are not tied to a particular buffer.

To run a function with a particular buffer current, use
`jitte-refresh-buffer-hook' and use `derived-mode-p'
inside your function."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-refresh
  :type 'hook)

(defcustom jitte-post-refresh-hook nil
  "Hook run after refreshing in `jitte-refresh'.

This hook, or `jitte-pre-refresh-hook', should be used
for functions that are not tied to a particular buffer.

To run a function with a particular buffer current, use
`jitte-refresh-buffer-hook' and use `derived-mode-p'
inside your function."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-refresh
  :type 'hook)

(defcustom jitte-refresh-buffer-hook nil
  "Hook run after refreshing a Jitte buffer.
Use `derived-mode-p' inside your function to only act on certain buffer types."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-refresh
  :type 'hook)

(defcustom jitte-display-buffer-function #'jitte-display-buffer-traditional
  "The function used to display a Jitte buffer.

All Jitte buffers (buffers whose major-modes derive from
`jitte-mode') are displayed using `jitte-display-buffer',
which in turn uses the function specified here."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-buffers
  :type '(radio (function-item jitte-display-buffer-traditional)
                (function-item display-buffer)
                (function :tag "Function")))

(defcustom jitte-pre-display-buffer-hook nil
  "Hook run by `jitte-display-buffer' before displaying the buffer."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-buffers
  :type 'hook)

(defcustom jitte-post-display-buffer-hook nil
  "Hook run by `jitte-display-buffer' after displaying the buffer."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-buffers
  :type 'hook)

;;; Variables

(defvar-local jitte-buffer-revision nil
  "The revision being displayed in the current buffer.")

(defvar-local jitte-buffer-revisions nil
  "The revisions being displayed in the current buffer.")

(defvar-local jitte-buffer-diff-type nil
  "The diff type shown in the current buffer.")

(defvar-local jitte-buffer-diff-args nil
  "The diff arguments used in the current buffer.")

(defvar-local jitte-buffer-diff-files nil
  "The files shown in the current buffer.")

(defvar-local jitte-buffer-log-args nil
  "The log arguments used in the current buffer.")

(defvar-local jitte-buffer-log-files nil
  "The files shown in the current buffer.")

(defvar-local jitte-buffer-range nil
  "The range shown in the current buffer.")

(defvar-local jitte-buffer-refname nil
  "The refname shown in the current buffer.")

(defvar-local jitte-buffer-file-name nil
  "The file shown in the current buffer.")

(defvar-local jitte-refresh-args nil
  "The arguments used to refresh the current buffer.")

(defvar jitte-buffer-margin nil
  "The margin configuration for Jitte buffers.")

;;; Modes

(defvar jitte-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "C-c C-c") #'jitte-dispatch-popup)
    (define-key map (kbd "C-c C-e") #'jitte-edit-line)
    (define-key map "+" #'jitte-diff-more-context)
    (define-key map "-" #'jitte-diff-less-context)
    (define-key map "0" #'jitte-diff-default-context)
    (define-key map "a" #'jitte-apply)
    (define-key map "A" #'jitte-cherry-pick)
    (define-key map "b" #'jitte-branch)
    (define-key map "B" #'jitte-bisect)
    (define-key map "c" #'jitte-commit)
    (define-key map "C" #'jitte-clone)
    (define-key map "d" #'jitte-diff)
    (define-key map "D" #'jitte-diff-refresh)
    (define-key map "e" #'jitte-edit)
    (define-key map "E" #'jitte-ediff)
    (define-key map "f" #'jitte-fetch)
    (define-key map "F" #'jitte-pull)
    (define-key map "g" #'jitte-refresh)
    (define-key map "G" #'jitte-refresh-all)
    (define-key map "h" #'jitte-help)
    (define-key map "H" #'jitte-section-toggle)
    (define-key map "i" #'jitte-init)
    (define-key map "I" #'jitte-ignore)
    (define-key map "j" #'jitte-status-jump)
    (define-key map "J" #'jitte-display-repository)
    (define-key map "k" #'jitte-delete-thing)
    (define-key map "K" #'jitte-file-untrack)
    (define-key map "l" #'jitte-log)
    (define-key map "L" #'jitte-log-refresh)
    (define-key map "m" #'jitte-merge)
    (define-key map "M" #'jitte-remote)
    (define-key map "n" #'jitte-new)
    (define-key map "N" #'jitte-notes)
    (define-key map "o" #'jitte-submodule)
    (define-key map "O" #'jitte-subtree)
    (define-key map "p" #'jitte-push)
    (define-key map "P" #'jitte-patch)
    (define-key map "q" #'jitte-mode-bury-buffer)
    (define-key map "Q" #'jitte-mode-quit-window)
    (define-key map "r" #'jitte-rebase)
    (define-key map "R" #'jitte-file-rename)
    (define-key map "s" #'jitte-stage)
    (define-key map "S" #'jitte-stage-modified)
    (define-key map "t" #'jitte-tag)
    (define-key map "T" #'jitte-worktree)
    (define-key map "u" #'jitte-unstage)
    (define-key map "U" #'jitte-unstage-all)
    (define-key map "v" #'jitte-reverse)
    (define-key map "V" #'jitte-revert)
    (define-key map "w" #'jitte-am)
    (define-key map "W" #'jitte-patch-apply)
    (define-key map "x" #'jitte-reset)
    (define-key map "X" #'jitte-reset-keep)
    (define-key map "y" #'jitte-show-refs)
    (define-key map "Y" #'jitte-cherry)
    (define-key map "z" #'jitte-stash)
    (define-key map "Z" #'jitte-worktree)
    (define-key map ":" #'jitte-git-command)
    (define-key map "!" #'jitte-run-popup)
    (define-key map "$" #'jitte-process-buffer)
    (define-key map "?" #'jitte-help)
    (define-key map (kbd "RET") #'jitte-visit-thing)
    (define-key map "<" #'jitte-diff-smaller-hunks)
    (define-key map ">" #'jitte-diff-larger-hunks)
    (define-key map [remap previous-line] #'jitte-previous-line)
    (define-key map [remap next-line] #'jitte-next-line)
    (define-key map [remap backward-paragraph] #'jitte-previous-section)
    (define-key map [remap forward-paragraph] #'jitte-next-section)
    (define-key map "^" #'jitte-section-up)
    (define-key map "\M-p" #'jitte-section-backward-sibling)
    (define-key map "\M-n" #'jitte-section-forward-sibling)
    (define-key map "\t" #'jitte-section-toggle)
    (define-key map [backtab] #'jitte-section-cycle-global)
    (define-key map "\M-\t" #'jitte-section-cycle)
    (define-key map [M-tab] #'jitte-section-cycle)
    (define-key map (kbd "C-x 4 RET") #'jitte-visit-thing-other-window)
    (define-key map (kbd "C-x 5 RET") #'jitte-visit-thing-other-frame)
    map)
  "Keymap for Jitte major mode.")

(define-derived-mode jitte-mode magit-section-mode "Jitte"
  "Parent major mode from which Jitte major modes inherit.

Please see the manual for a complete description of Jitte.

\\{jitte-mode-map}"
  :group 'jitte-modes
  (hack-dir-local-variables-non-file-buffer)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t)
  (setq-local show-trailing-whitespace nil)
  (setq-local list-buffers-directory default-directory)
  (setq-local bookmark-make-record-function #'jitte-bookmark-make-record)
  (when (bound-and-true-p global-diff-hl-mode)
    (diff-hl-mode -1))
  (run-hooks 'jitte-mode-hook))

;;; Buffer Management

(defun jitte-setup-buffer (mode &optional refresh-buffer &rest refresh-args)
  "Setup and return a buffer for MODE.
If REFRESH-BUFFER is non-nil, refresh the buffer.
REFRESH-ARGS are passed to the refresh function."
  (let* ((directory (jitte-toplevel))
         (buffer (jitte-get-mode-buffer mode directory)))
    (unless buffer
      (setq buffer (jitte-generate-buffer mode directory))
      (with-current-buffer buffer
        (setq jitte-refresh-args refresh-args)
        (funcall mode)
        (jitte-setup-buffer-internal refresh-buffer)))
    (when refresh-buffer
      (with-current-buffer buffer
        (apply #'jitte-refresh-buffer refresh-args)))
    buffer))

(defun jitte-setup-buffer-internal (&optional refresh-buffer)
  "Setup the current buffer for Jitte.
If REFRESH-BUFFER is non-nil, refresh the buffer."
  (jitte-set-header-line-format)
  (run-hooks 'jitte-setup-buffer-hook)
  (when refresh-buffer
    (jitte-refresh-buffer)))

(defun jitte-get-mode-buffer (mode &optional directory)
  "Return existing buffer for MODE in DIRECTORY.
If DIRECTORY is nil, use the current repository."
  (let ((directory (or directory (jitte-toplevel))))
    (cl-find-if (lambda (buffer)
                  (with-current-buffer buffer
                    (and (eq major-mode mode)
                         (equal default-directory directory))))
                (buffer-list))))

(defun jitte-generate-buffer (mode &optional directory)
  "Generate and return a new buffer for MODE in DIRECTORY."
  (let* ((directory (or directory (jitte-toplevel)))
         (name (jitte-buffer-name mode directory)))
    (generate-new-buffer name)))

(defun jitte-buffer-name (mode &optional directory)
  "Return buffer name for MODE in DIRECTORY."
  (let ((directory (or directory default-directory)))
    (format "*jitte: %s*" (file-name-nondirectory
                           (directory-file-name directory)))))

;;; Display

(defun jitte-display-buffer (buffer)
  "Display BUFFER in some window and maybe select it."
  (run-hook-with-args 'jitte-pre-display-buffer-hook buffer)
  (funcall jitte-display-buffer-function buffer)
  (run-hook-with-args 'jitte-post-display-buffer-hook buffer))

(defun jitte-display-buffer-traditional (buffer)
  "Display BUFFER the way this has traditionally been done."
  (display-buffer buffer '(display-buffer-same-window)))

;;; Refresh

(defun jitte-refresh (&optional refresh-args)
  "Refresh current buffer if it's a Jitte buffer.
Optional REFRESH-ARGS are passed to the refresh function."
  (interactive)
  (when (derived-mode-p 'jitte-mode)
    (run-hooks 'jitte-pre-refresh-hook)
    (if refresh-args
        (apply #'jitte-refresh-buffer refresh-args)
      (jitte-refresh-buffer))
    (run-hooks 'jitte-post-refresh-hook)))

(defun jitte-refresh-buffer (&rest _args)
  "Refresh the current buffer.
This is a placeholder that should be overridden by derived modes."
  (message "Refreshing %s buffer..." (buffer-name))
  (run-hooks 'jitte-refresh-buffer-hook)
  (message "Refreshing %s buffer...done" (buffer-name)))

(defun jitte-refresh-all ()
  "Refresh all Jitte buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'jitte-mode)
        (jitte-refresh)))))

;;; Utilities

(defun jitte-mode-bury-buffer (&optional kill-buffer)
  "Bury or kill the current Jitte buffer.
With prefix argument KILL-BUFFER, kill the buffer instead."
  (interactive "P")
  (if kill-buffer
      (kill-buffer)
    (bury-buffer)))

(defun jitte-mode-quit-window (&optional kill-buffer)
  "Quit the current window and maybe kill the buffer.
With prefix argument KILL-BUFFER, kill the buffer instead."
  (interactive "P")
  (quit-window kill-buffer))

(defun jitte-set-header-line-format ()
  "Set the header line format for the current buffer."
  (setq header-line-format
        (substitute-command-keys
         "\\<jitte-mode-map>Type \\[jitte-dispatch] for help")))

;;; Section Navigation

(defun jitte-section-toggle (&optional section)
  "Toggle visibility of the body of the current section."
  (interactive)
  (magit-section-toggle (or section (magit-current-section))))

(defun jitte-next-section ()
  "Move to the next section."
  (interactive)
  (magit-section-forward))

(defun jitte-previous-section ()
  "Move to the previous section."
  (interactive)
  (magit-section-backward))

(defun jitte-section-up ()
  "Move to the parent section."
  (interactive)
  (when-let ((parent (magit-section-parent (magit-current-section))))
    (goto-char (magit-section-start parent))))

;;; Placeholder Functions

(declare-function jitte-dispatch "jitte-transient" ())

(defun jitte-dispatch-popup ()
  "Show the Jitte dispatch popup."
  (interactive)
  (require 'jitte-transient)
  (jitte-dispatch))

(defun jitte-help ()
  "Show help for Jitte."
  (interactive)
  (message "Jitte help not yet implemented"))

(defun jitte-visit-thing ()
  "Visit the thing at point."
  (interactive)
  (message "Visit thing not yet implemented"))

;;; Footer

(provide 'jitte-mode)

;;; jitte-mode.el ends here
