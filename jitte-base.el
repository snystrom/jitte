;;; jitte-base.el --- Base functionality for Jitte  -*- lexical-binding:t -*-

;; Copyright (C) 2025 Jitte Contributors

;; Author: Jitte Contributors
;; Maintainer: Jitte Contributors
;; Keywords: tools vc jj jujutsu
;; Package-Version: 0.1.0
;; Package-Requires: (
;;     (emacs "28.1")
;;     (compat "30.1")
;;     (magit-section "4.0.0")
;;     (transient "0.9.0")
;;     (with-editor "3.4.0"))

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

;; This library provides base functionality for Jitte, including
;; fundamental utilities, constants, and compatibility helpers.

;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'eieio)
(require 'subr-x)

;;; Constants

(defconst jitte-version "0.1.0"
  "Version of Jitte.")

(defconst jitte-jj-minimum-version "0.20.0"
  "Minimum version of jj required by Jitte.")

;;; Variables

(defvar jitte-jj-executable
  (or (executable-find "jj")
      "jj")
  "The jj executable used by Jitte.")

(defvar jitte-repository-directories nil
  "List of directories that are or contain jj repositories.
This is most useful when used as a dir-local variable.")

;;; Utilities

(defun jitte-toplevel (&optional directory)
  "Return the absolute path to the top-level of the current jj repository.
When optional DIRECTORY is provided, return the top-level of that directory."
  (let ((default-directory (file-name-as-directory
                            (expand-file-name (or directory ".")))))
    (when-let ((toplevel (locate-dominating-file default-directory ".jj")))
      (file-name-as-directory (expand-file-name toplevel)))))

(defun jitte-inside-repository-p (&optional directory)
  "Return t if DIRECTORY is inside a jj repository.
When DIRECTORY is nil, use the current directory."
  (not (null (jitte-toplevel directory))))

(defun jitte-inside-working-tree-p (&optional directory)
  "Return t if DIRECTORY is inside the working tree of a jj repository.
When DIRECTORY is nil, use the current directory."
  (and (jitte-inside-repository-p directory)
       t))

(defun jitte-file-relative-name (&optional file tracked-only)
  "Return the path of FILE relative to the repository root.
If optional FILE is nil, return the relative path of the current buffer's file.
If optional TRACKED-ONLY is non-nil and FILE is not tracked, return nil."
  (when-let ((file (or file (buffer-file-name)))
             (toplevel (jitte-toplevel)))
    (let ((relname (file-relative-name file toplevel)))
      (if (and tracked-only
               (string-prefix-p "../" relname))
          nil
        relname))))

(defun jitte-get-jj-dir (&optional directory)
  "Return the absolute path to the .jj directory.
When optional DIRECTORY is provided, return the .jj directory for that
directory."
  (when-let ((toplevel (jitte-toplevel directory)))
    (expand-file-name ".jj" toplevel)))

(defun jitte-rev-parse (revision)
  "Parse REVISION into a change ID or commit hash.
Return nil if REVISION cannot be parsed."
  (let ((revision (string-trim revision)))
    (cond
     ((string-empty-p revision) nil)
     ((string-match-p "^[0-9a-f]\\{40\\}$" revision) revision)
     ((string-match-p "^[a-z0-9]\\{12\\}$" revision) revision)
     (t revision))))

(defun jitte-wash-sequence (sequence)
  "Remove empty strings and nil values from SEQUENCE."
  (delq nil (delete "" sequence)))

;;; User Interface Utilities

(defun jitte-completing-read (prompt choices &optional predicate require-match
                                     initial-input history default)
  "Read from CHOICES in the minibuffer, with PROMPT.
This is a wrapper around `completing-read' with Jitte-specific defaults."
  (completing-read prompt choices predicate require-match
                   initial-input history default))

(defun jitte-confirm (prompt &optional default)
  "Ask user to confirm action with PROMPT.
Return t if user confirms, nil otherwise.
Optional DEFAULT determines the default answer."
  (y-or-n-p (concat prompt (if default " (y)" " (n)") " ")))

;;; Repository Detection

(defun jitte-repository-local-exists-p ()
  "Return t if a local repository exists."
  (file-exists-p (jitte-get-jj-dir)))

(defun jitte-repository-local-repository-p (directory)
  "Return t if DIRECTORY contains a jj repository."
  (file-directory-p (expand-file-name ".jj" directory)))

;;; Working Directory

(defmacro jitte-with-toplevel (&rest body)
  "Execute BODY with `default-directory' set to the repository toplevel."
  (declare (indent 0) (debug t))
  `(when-let ((toplevel (jitte-toplevel)))
     (let ((default-directory toplevel))
       ,@body)))

;;; Error Handling

(define-error 'jitte-error "Jitte error")
(define-error 'jitte-jj-error "Jj error" 'jitte-error)

(defun jitte-user-error (format &rest args)
  "Signal a `user-error' with FORMAT and ARGS."
  (user-error "Jitte: %s" (apply #'format format args)))

;;; Version Information

(defun jitte-version (&optional print-dest)
  "Return the version of Jitte.
If optional PRINT-DEST is non-nil, also print the version."
  (let ((version-string (format "Jitte %s" jitte-version)))
    (when print-dest
      (message "%s" version-string))
    version-string))

(provide 'jitte-base)

;;; jitte-base.el ends here