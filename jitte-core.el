;;; jitte-core.el --- Core functionality for Jitte  -*- lexical-binding:t -*-

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

;; This library requires several other libraries and defines custom groups
;; for Jitte configuration options.

;;; Code:

(require 'jitte-base)

;;; Custom Groups

(defgroup jitte nil
  "Controlling Jujutsu from Emacs."
  :link '(url-link "https://github.com/your-org/jitte")
  :group 'tools)

(defgroup jitte-essentials nil
  "Options that every Jitte user should briefly think about.

Each of these options falls into one or more of these categories:

* Options that affect Jitte's behavior in fundamental ways.
* Options that affect safety.
* Options that affect performance.
* Options that are of a personal nature."
  :group 'jitte)

(defgroup jitte-miscellaneous nil
  "Miscellaneous Jitte options."
  :group 'jitte)

(defgroup jitte-commands nil
  "Options controlling behavior of certain commands."
  :group 'jitte)

(defgroup jitte-modes nil
  "Modes used or provided by Jitte."
  :group 'jitte)

(defgroup jitte-buffers nil
  "Options concerning Jitte buffers."
  :group 'jitte)

(defgroup jitte-refresh nil
  "Options controlling how Jitte buffers are refreshed."
  :group 'jitte
  :group 'jitte-buffers)

(defgroup jitte-faces nil
  "Faces used by Jitte."
  :group 'jitte
  :group 'faces)

(defgroup jitte-extensions nil
  "Extensions to Jitte."
  :group 'jitte)

;;; Options

(defcustom jitte-repository-directories nil
  "List of directories that are Jitte repositories.
Each element has the form (DIRECTORY . DEPTH), where DIRECTORY
is a directory or a directory file-name, a string.  DEPTH, an
integer, specifies the maximum depth to look for jj repositories.
If DEPTH is 0, then only add DIRECTORY itself."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-essentials
  :type '(repeat (cons directory (integer :tag "Depth"))))

(defcustom jitte-clone-default-directory nil
  "Default directory to use for `jitte-clone'."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-commands
  :type '(choice (const :tag "Default to `default-directory'" nil)
                 (directory :tag "Directory")))

(defcustom jitte-jj-executable "jj"
  "The jj executable used by Jitte.
Either the name of the executable assuming it's on `exec-path',
or the absolute path to the executable."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-essentials
  :type 'string)

(defcustom jitte-prefer-remote-upstream nil
  "Whether to prefer remote upstream when setting upstream branch."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-commands
  :type 'boolean)

;;; Faces

(defface jitte-header-line
  '((t :inherit magit-section-heading))
  "Face for the `header-line' in some Jitte modes."
  :group 'jitte-faces)

(defface jitte-dimmed
  '((((class color) (background light)) :foreground "grey50")
    (((class color) (background  dark)) :foreground "grey50"))
  "Face for text that should be less prominent."
  :group 'jitte-faces)

(defface jitte-hash
  '((((class color) (background light)) :foreground "grey60")
    (((class color) (background  dark)) :foreground "grey40"))
  "Face for commit and change hashes."
  :group 'jitte-faces)

(defface jitte-keyword
  '((t :inherit font-lock-string-face))
  "Face for keywords."
  :group 'jitte-faces)

(defface jitte-branch-local
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background  dark)) :foreground "LightSkyBlue1"))
  "Face for local bookmarks."
  :group 'jitte-faces)

(defface jitte-branch-remote
  '((((class color) (background light)) :foreground "DarkOliveGreen4")
    (((class color) (background  dark)) :foreground "DarkSeaGreen2"))
  "Face for remote bookmarks."
  :group 'jitte-faces)

(defface jitte-working-copy
  '((t :inherit jitte-branch-local :weight bold))
  "Face for the working copy indicator."
  :group 'jitte-faces)

(defface jitte-tag
  '((((class color) (background light)) :foreground "Goldenrod4")
    (((class color) (background  dark)) :foreground "LightGoldenrod2"))
  "Face for tags."
  :group 'jitte-faces)

;;; Variables

(defvar jitte-this-error nil)

(defvar jitte-process-error-message-regexps
  '(("^\\*\\*Error: " . jitte-process-ng-section)
    ("^error: " . jitte-process-ng-section)
    ("^fatal: " . jitte-process-ng-section))
  "An alist of regexps used to identify error messages.")

;;; Utilities

(defun jitte-get-top-dir (&optional directory)
  "Return the absolute path to the top-level of the current jj repository."
  (jitte-toplevel directory))

(defmacro jitte-with-repository (directory &rest body)
  "Execute BODY with DIRECTORY as the repository root."
  (declare (indent 1) (debug t))
  `(let ((default-directory ,(or directory '(jitte-toplevel))))
     (unless (jitte-inside-repository-p)
       (user-error "Not inside a jj repository"))
     ,@body))

(provide 'jitte-core)

;;; jitte-core.el ends here