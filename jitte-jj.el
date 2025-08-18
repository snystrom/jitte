;;; jitte-jj.el --- Jj functionality for Jitte  -*- lexical-binding:t -*-

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

;; This library implements wrappers for various jj plumbing commands.

;;; Code:

(require 'jitte-base)

(require 'format-spec)

;;; Options

(defgroup jitte-process nil
  "Jj and other external processes used by Jitte."
  :group 'jitte)

(defvar jitte-jj-environment
  (list (format "INSIDE_EMACS=%s,jitte" emacs-version))
  "Prepended to `process-environment' while running jj.")

(defcustom jitte-jj-output-coding-system nil
  "Coding system for receiving output from jj.
When non-nil, this overrides the system default."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-process
  :type '(choice (coding-system :tag "Coding system to decode jj output")
                 (const :tag "Use system default" nil)))

(defcustom jitte-jj-global-arguments nil
  "Global jj arguments.
These arguments are passed to every jj command."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-commands
  :type '(repeat string))

;;; Variables

(defvar jitte-this-error nil)

(defvar jitte-process-error-message-regexps
  '(("^Error: " . jitte-process-ng-section)
    ("^error: " . jitte-process-ng-section)
    ("^hint: " . jitte-process-ok-section))
  "An alist of regexps used to identify error messages.")

;;; Utilities

(defun jitte-jj-executable-find (command)
  "Find the jj executable, considering COMMAND."
  (or (and (file-name-absolute-p jitte-jj-executable)
           (file-executable-p jitte-jj-executable)
           jitte-jj-executable)
      (executable-find jitte-jj-executable)
      (user-error "Cannot find jj executable")))

(defun jitte-process-environment ()
  "Return the environment for jj processes."
  (append jitte-jj-environment process-environment))

(defmacro jitte-with-environment (&rest body)
  "Execute BODY with the jj environment."
  (declare (indent 0) (debug t))
  `(let ((process-environment (jitte-process-environment)))
     ,@body))

;;; Running jj

(defun jitte-run-jj (&rest args)
  "Run jj with ARGS and return the exit code."
  (jitte-with-environment
    (apply #'jitte-call-process (jitte-jj-executable-find "jj") args)))

(defun jitte-run-jj-sync (&rest args)
  "Run jj with ARGS synchronously and return output."
  (jitte-with-environment
    (with-temp-buffer
      (let ((coding-system-for-read jitte-jj-output-coding-system)
            (exit-code (apply #'call-process
                              (jitte-jj-executable-find "jj")
                              nil t nil
                              (append jitte-jj-global-arguments args))))
        (if (zerop exit-code)
            (buffer-string)
          (error "jj command failed with exit code %d: %s"
                 exit-code (buffer-string)))))))

(defun jitte-run-jj-async (&rest args)
  "Run jj with ARGS asynchronously."
  (jitte-with-environment
    (let ((process (apply #'start-process
                          "jj" "*jitte-jj*"
                          (jitte-jj-executable-find "jj")
                          (append jitte-jj-global-arguments args))))
      (set-process-sentinel process #'jitte-process-sentinel)
      process)))

(defun jitte-call-process (program &rest args)
  "Call PROGRAM with ARGS and return exit code."
  (apply #'call-process program nil nil nil args))

(defun jitte-process-sentinel (process event)
  "Process sentinel for jj processes."
  (when (memq (process-status process) '(exit signal))
    (let ((exit-code (process-exit-status process)))
      (if (zerop exit-code)
          (message "jj process finished successfully")
        (message "jj process failed with exit code %d" exit-code)))))

;;; Status

(defun jitte-status-p ()
  "Return t if there are any changes in the working copy."
  (not (string-empty-p (string-trim (jitte-run-jj-sync "status")))))

(defun jitte-status-files ()
  "Return list of files with changes in the working copy."
  (let ((output (jitte-run-jj-sync "status" "--no-graph")))
    (when (not (string-empty-p output))
      (split-string output "\n" t))))

;;; Log

(defun jitte-log (&optional revset args)
  "Return log output for REVSET with ARGS."
  (let ((cmd-args (append (list "log")
                          (when revset (list "-r" revset))
                          args)))
    (jitte-run-jj-sync cmd-args)))

(defun jitte-log-oneline (&optional revset limit)
  "Return oneline log output for REVSET with LIMIT."
  (let ((args (list "log" "--no-graph" "-T" "oneline")))
    (when revset
      (setq args (append args (list "-r" revset))))
    (when limit
      (setq args (append args (list "-l" (number-to-string limit)))))
    (jitte-run-jj-sync args)))

;;; Diff

(defun jitte-diff (&optional revision args)
  "Return diff output for REVISION with ARGS."
  (let ((cmd-args (append (list "diff")
                          (when revision (list "-r" revision))
                          args)))
    (jitte-run-jj-sync cmd-args)))

(defun jitte-diff-files (&optional revision)
  "Return list of files changed in REVISION."
  (let ((output (jitte-run-jj-sync "diff" "--summary"
                                   (when revision (list "-r" revision)))))
    (when (not (string-empty-p output))
      (mapcar (lambda (line)
                (if (string-match "^[AMD] \\(.+\\)$" line)
                    (match-string 1 line)
                  line))
              (split-string output "\n" t)))))

;;; Revisions and Revsets

(defun jitte-rev-parse (revision)
  "Parse REVISION into a commit hash."
  (string-trim (jitte-run-jj-sync "log" "-r" revision "-T" "commit_id")))

(defun jitte-change-id (revision)
  "Return the change ID for REVISION."
  (string-trim (jitte-run-jj-sync "log" "-r" revision "-T" "change_id")))

(defun jitte-current-working-copy-change-id ()
  "Return the change ID of the current working copy."
  (jitte-change-id "@"))

(defun jitte-current-working-copy-commit-id ()
  "Return the commit ID of the current working copy."
  (jitte-rev-parse "@"))

;;; Bookmarks

(defun jitte-bookmark-list ()
  "Return list of bookmarks."
  (let ((output (jitte-run-jj-sync "bookmark" "list")))
    (when (not (string-empty-p output))
      (mapcar (lambda (line)
                (when (string-match "^\\([^:]+\\):" line)
                  (match-string 1 line)))
              (split-string output "\n" t)))))

(defun jitte-bookmark-create (name &optional revision)
  "Create bookmark NAME at REVISION."
  (jitte-run-jj "bookmark" "create" name
                (when revision (list "-r" revision))))

(defun jitte-bookmark-delete (name)
  "Delete bookmark NAME."
  (jitte-run-jj "bookmark" "delete" name))

;;; Working Copy Operations

(defun jitte-describe (message &optional revision)
  "Set description MESSAGE for REVISION (or working copy)."
  (jitte-run-jj "describe" "-m" message
                (when revision (list "-r" revision))))

(defun jitte-new (&optional message)
  "Create a new working copy change with optional MESSAGE."
  (if message
      (jitte-run-jj "new" "-m" message)
    (jitte-run-jj "new")))

(defun jitte-edit (revision)
  "Start editing REVISION."
  (jitte-run-jj "edit" revision))

(defun jitte-squash (&optional revision message)
  "Squash changes into REVISION with optional MESSAGE."
  (let ((args (list "squash")))
    (when revision
      (setq args (append args (list "-r" revision))))
    (when message
      (setq args (append args (list "-m" message))))
    (apply #'jitte-run-jj args)))

;;; Repository Information

(defun jitte-inside-repository-p (&optional directory)
  "Return t if DIRECTORY is inside a jj repository."
  (let ((default-directory (or directory default-directory)))
    (jitte-toplevel)))

(defun jitte-get-top-dir (&optional directory)
  "Return the top-level directory of the repository."
  (jitte-toplevel directory))

;;; File Operations

(defun jitte-tracked-files ()
  "Return list of tracked files."
  (let ((output (jitte-run-jj-sync "files")))
    (when (not (string-empty-p output))
      (split-string output "\n" t))))

;;; Utilities

(defun jitte-wash-args (args)
  "Remove nil and empty string arguments from ARGS."
  (delq nil (delete "" (flatten-list args))))

(defun jitte-wash-sequence (sequence)
  "Remove empty strings and nil values from SEQUENCE."
  (delq nil (delete "" sequence)))

;;; Commands

(defun jitte-init (&optional directory)
  "Initialize a new jj repository in DIRECTORY."
  (let ((dir (or directory default-directory)))
    (jitte-run-jj "init" (expand-file-name dir))))

(defun jitte-clone (url directory &optional args)
  "Clone repository from URL to DIRECTORY with optional ARGS."
  (apply #'jitte-run-jj "clone" url (expand-file-name directory) args))

(provide 'jitte-jj)

;;; jitte-jj.el ends here