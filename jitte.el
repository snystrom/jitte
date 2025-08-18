;;; jitte.el --- A Jujutsu porcelain inside Emacs  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2025 Jitte Contributors

;; Author: Jitte Contributors
;; Maintainer: Jitte Contributors
;; Homepage: https://github.com/your-org/jitte
;; Keywords: jj jujutsu tools vc

;; Package-Version: 0.1.0
;; Package-Requires: (
;;     (emacs "28.1")
;;     (compat "30.1")
;;     (magit-section "4.0.0")
;;     (seq "2.24")
;;     (transient "0.9.0")
;;     (with-editor "3.4.0"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Jitte is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; Jitte is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Jitte.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Jitte is a text-based Jujutsu user interface that provides an
;; intuitive interface for working with jj repositories.  Commands are
;; invoked using short mnemonic key sequences that take the cursor's
;; position in the highly actionable interface into account to provide
;; context-sensitive behavior.

;; With Jitte you can perform most jj operations that you would do
;; when using jj on the command-line, but at greater speed and while
;; taking advantage of advanced features.

;;; Code:

(require 'jitte-core)
(require 'jitte-jj)
(require 'jitte-status)
(require 'jitte-log)
(require 'jitte-diff)
(require 'jitte-split)
(require 'jitte-transient)

(require 'format-spec)
(require 'package nil t)
(require 'with-editor)

;;; Autoloads

;;;###autoload
(defun jitte-version (&optional print-destination)
  "Return the version of Jitte.
If optional PRINT-DESTINATION is non-nil, also print the version."
  (interactive (list t))
  (let ((version-string (format "Jitte %s, jj %s"
                                jitte-version
                                (or (jitte-jj-version) "unknown"))))
    (when print-destination
      (message "%s" version-string))
    version-string))

;;;###autoload
(defun jitte-status (&optional directory)
  "Show the working tree status for the current jj repository.
If optional DIRECTORY is provided, use that directory instead."
  (interactive
   (list (and current-prefix-arg
              (jitte-read-repository))))
  (if (jitte-inside-repository-p directory)
      (jitte-status-setup-buffer directory)
    (if directory
        (user-error "%s is not a jj repository" directory)
      (user-error "Not in a jj repository"))))

;;;###autoload
(defun jitte-log (&optional revset args)
  "Show log for the current jj repository.
Optional REVSET specifies which revisions to show.
Optional ARGS specifies additional arguments to jj log."
  (interactive)
  (jitte-log-setup-buffer revset args))

;;;###autoload
(defun jitte-diff (&optional rev-or-range args)
  "Show differences for REV-OR-RANGE.
If REV-OR-RANGE is not specified, show differences for the working copy."
  (interactive)
  (jitte-diff-setup-buffer rev-or-range args))

;;;###autoload
(defun jitte-clone (repository directory &optional args)
  "Clone REPOSITORY to DIRECTORY using jj clone.
Optional ARGS specifies additional arguments to jj clone."
  (interactive
   (let ((repo (read-string "Repository URL: "))
         (dir (read-directory-name "Clone to: "
                                   (or jitte-clone-default-directory
                                       default-directory))))
     (list repo dir)))
  (apply #'jitte-run-jj-async "clone" repository directory (or args nil)))

;;;###autoload
(defun jitte-init (&optional directory)
  "Initialize a new jj repository in DIRECTORY.
If DIRECTORY is not specified, use the current directory."
  (interactive
   (list (and current-prefix-arg
              (read-directory-name "Initialize repository in: "))))
  (let ((dir (or directory default-directory)))
    (jitte-run-jj-async "init" dir)
    (when (called-interactively-p 'any)
      (jitte-status dir))))

;;; Utility Functions

(defun jitte-jj-version ()
  "Return the version of jj."
  (when-let ((version-output (jitte-run-jj-sync "version")))
    (when (string-match "jj \\([0-9]+\\.[0-9]+\\.[0-9]+\\)" version-output)
      (match-string 1 version-output))))

(defun jitte-read-repository ()
  "Read repository directory from user."
  (file-name-as-directory
   (expand-file-name
    (read-directory-name "Repository: " default-directory))))

;;; Entry Points

;;;###autoload
(defun jitte-project-status ()
  "Show the working tree status for the current project.
This function is suitable for binding to a global key."
  (interactive)
  (let ((default-directory (or (locate-dominating-file default-directory ".jj")
                               default-directory)))
    (call-interactively #'jitte-status)))

;;;###autoload
(define-obsolete-function-alias 'jitte 'jitte-status "0.1.0"
  "Use `jitte-status' instead.")

;;; Mode Line

(defvar jitte-mode-line-process nil
  "Mode line process indicator for Jitte buffers.")

;;; Global Variables

(defvar jitte-buffer-file-name nil
  "Name of the file that the current buffer is visiting.")

(defvar jitte-buffer-revision nil
  "Revision shown in the current buffer.")

;;; Keymaps and Hooks

(defvar jitte-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    (define-key map "g" #'jitte-refresh)
    map)
  "Parent keymap for all Jitte modes.")

(defvar jitte-mode-hook nil
  "Hook run when entering any Jitte mode.")

;;; Integration

;;;###autoload
(with-eval-after-load 'project
  (defun jitte-project-status ()
    "Show the working tree status for the current project."
    (interactive)
    (if-let ((root (locate-dominating-file default-directory ".jj")))
        (let ((default-directory root))
          (jitte-status))
      (user-error "Not in a jj repository"))))

;;; Footer

(provide 'jitte)

;;; jitte.el ends here