;;; jitte-transient.el --- Support for transients  -*- lexical-binding:t -*-

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

;; This library implements Jitte-specific transient commands and menus.

;;; Code:

(require 'jitte-jj)
(require 'jitte-mode)
(require 'jitte-process)

(require 'transient)

;;; Main Dispatch

;;;###autoload (autoload 'jitte-dispatch "jitte-transient" nil t)
(transient-define-prefix jitte-dispatch ()
  "Invoke a Jitte command from a list of available commands."
  :info-manual "(jitte)Top"
  ["Transient and dwim commands"
   [("A" "Apply"          jitte-apply)
    ("a" "Apply"          jitte-cherry-pick)]
   [("b" "Branch"         jitte-bookmark)
    ("B" "Bisect"         jitte-bisect)]
   [("c" "Commit"         jitte-commit)
    ("C" "Clone"          jitte-clone)]
   [("d" "Diff"           jitte-diff)
    ("D" "Diff refresh"   jitte-diff-refresh)]
   [("e" "Edit"           jitte-edit)
    ("E" "Ediff"          jitte-ediff)]
   [("f" "Fetch"          jitte-fetch)
    ("F" "Pull"           jitte-pull)]
   [("g" "Refresh"        jitte-refresh)
    ("G" "Refresh all"    jitte-refresh-all)]
   [("i" "Init"           jitte-init)
    ("I" "Ignore"         jitte-ignore)]
   [("j" "Jump"           jitte-status-jump)
    ("J" "Display repository" jitte-display-repository)]
   [("l" "Log"            jitte-log)
    ("L" "Log refresh"    jitte-log-refresh)]
   [("m" "Merge"          jitte-merge)
    ("M" "Remote"         jitte-remote)]
   [("n" "New"            jitte-new)
    ("N" "Notes"          jitte-notes)]
   [("o" "Submodule"      jitte-submodule)
    ("O" "Subtree"        jitte-subtree)]
   [("p" "Push"           jitte-push)
    ("P" "Patch"          jitte-patch)]
   [("r" "Rebase"         jitte-rebase)
    ("R" "File rename"    jitte-file-rename)]
   [("s" "Split"          jitte-split)
    ("S" "Squash"         jitte-squash)]
   [("t" "Tag"            jitte-tag)
    ("T" "Worktree"       jitte-worktree)]
   [("u" "Undo"           jitte-undo)
    ("U" "Unstage all"    jitte-unstage-all)]
   [("v" "Reverse"        jitte-reverse)
    ("V" "Revert"         jitte-revert)]
   [("w" "Apply patches"  jitte-am)
    ("W" "Patch apply"    jitte-patch-apply)]
   [("x" "Reset"          jitte-reset)
    ("X" "Reset keep"     jitte-reset-keep)]
   [("y" "Show refs"      jitte-show-refs)
    ("Y" "Cherry"         jitte-cherry)]
   [("z" "Stash"          jitte-stash)
    ("Z" "Worktree"       jitte-worktree)]]
  ["Applying changes"
   :if-derived jitte-mode
   [("a" "Apply"          jitte-apply)
    ("v" "Reverse"        jitte-reverse)
    ("k" "Discard"        jitte-discard)]
   [("s" "Stage"          jitte-stage)
    ("u" "Unstage"        jitte-unstage)]]
  ["Essential commands"
   [("$" "Show process buffer" jitte-process-buffer)
    ("q" "Quit"               jitte-mode-bury-buffer)]])

;;; Change Management

;;;###autoload (autoload 'jitte-commit "jitte-transient" nil t)
(transient-define-prefix jitte-commit ()
  "Create or edit commits."
  :info-manual "(jitte)Committing"
  :incompatible '(("--author=" "--reset-author"))
  ["Arguments"
   ("-m" "Message" "--message=" :prompt "Commit message: ")
   ("-r" "Revision" "--revision=" :prompt "Revision: ")]
  ["Actions"
   [("c" "Commit (describe)" jitte-commit-create)
    ("a" "Amend" jitte-commit-amend)]
   [("e" "Edit" jitte-commit-edit)
    ("w" "Reword" jitte-commit-reword)]
   [("f" "Fixup" jitte-commit-fixup)
    ("s" "Squash" jitte-commit-squash)]
   [("A" "Augment" jitte-commit-augment)
    ("x" "Absorb changes" jitte-commit-absorb)]])

;;;###autoload (autoload 'jitte-bookmark "jitte-transient" nil t)
(transient-define-prefix jitte-bookmark ()
  "Create, checkout and delete bookmarks."
  :man-page "jj-bookmark"
  ["Actions"
   [("b" "Checkout"      jitte-bookmark-checkout)
    ("l" "Checkout local" jitte-bookmark-checkout-local)
    ("c" "Create"        jitte-bookmark-create)
    ("n" "Create new"    jitte-bookmark-and-checkout)]
   [("m" "Move"          jitte-bookmark-move)
    ("d" "Delete"        jitte-bookmark-delete)
    ("r" "Rename"        jitte-bookmark-rename)]])

;;;###autoload (autoload 'jitte-split "jitte-transient" nil t)
(transient-define-prefix jitte-split ()
  "Split current change into multiple changes."
  :man-page "jj-split"
  ["Arguments"
   ("-r" "Revision" "--revision=" :prompt "Revision: ")
   ("-i" "Interactive" "--interactive")]
  ["Actions"
   [("s" "Split current change" jitte-split-current)
    ("p" "Split by paths" jitte-split-paths)]
   [("h" "Split hunks" jitte-split-hunks)
    ("l" "Split lines" jitte-split-lines)]])

;;; Log and History

;;;###autoload (autoload 'jitte-log "jitte-transient" nil t)
(transient-define-prefix jitte-log ()
  "Show commit logs."
  :man-page "jj-log"
  :value '("--graph")
  ["Arguments"
   ("-r" "Revset" "--revset=" :prompt "Revset: ")
   ("-l" "Limit" "--limit=" :prompt "Limit: ")
   ("-T" "Template" "--template=" :prompt "Template: ")
   ("-g" "Graph" "--graph")
   ("-p" "Show patch" "--patch")]
  ["Actions"
   [("l" "Log current" jitte-log-current)
    ("a" "Log all" jitte-log-all)]
   [("b" "Log bookmark" jitte-log-bookmark)
    ("f" "Log file" jitte-log-file)]
   [("o" "Log oneline" jitte-log-oneline)
    ("r" "Log revset" jitte-log-revset)]])

;;;###autoload (autoload 'jitte-diff "jitte-transient" nil t)
(transient-define-prefix jitte-diff ()
  "Show changes using jj diff."
  :man-page "jj-diff"
  ["Arguments"
   ("-r" "Revision" "--revision=" :prompt "Revision: ")
   ("-s" "Summary" "--summary")
   ("-g" "Git format" "--git")
   ("--" "Paths" :prompt "Paths: ")]
  ["Actions"
   [("d" "Diff working copy" jitte-diff-working-copy)
    ("c" "Diff revision" jitte-diff-revision)]
   [("r" "Diff range" jitte-diff-range)
    ("p" "Diff paths" jitte-diff-paths)]
   [("w" "Diff worktree" jitte-diff-worktree)
    ("s" "Show" jitte-show)]])

;;; Repository Operations

;;;###autoload (autoload 'jitte-remote "jitte-transient" nil t)
(transient-define-prefix jitte-remote ()
  "Manage remotes."
  :man-page "jj-git"
  ["Actions"
   [("a" "Add remote" jitte-remote-add)
    ("r" "Remove remote" jitte-remote-remove)
    ("p" "Prune remote" jitte-remote-prune)]
   [("f" "Fetch" jitte-fetch)
    ("F" "Fetch all" jitte-fetch-all)]
   [("P" "Push" jitte-push)
    ("u" "Push upstream" jitte-push-upstream)]])

;;;###autoload (autoload 'jitte-rebase "jitte-transient" nil t)
(transient-define-prefix jitte-rebase ()
  "Rebase changes."
  :man-page "jj-rebase"
  ["Arguments"
   ("-s" "Source" "--source=" :prompt "Source: ")
   ("-d" "Destination" "--destination=" :prompt "Destination: ")
   ("-r" "Revision" "--revision=" :prompt "Revision: ")]
  ["Actions"
   [("r" "Rebase" jitte-rebase-interactively)
    ("s" "Rebase source" jitte-rebase-source)]
   [("d" "Rebase destination" jitte-rebase-destination)
    ("a" "Rebase all" jitte-rebase-all)]])

;;; File Operations

;;;###autoload (autoload 'jitte-file "jitte-transient" nil t)
(transient-define-prefix jitte-file ()
  "File-level operations."
  ["Actions"
   [("s" "Stage" jitte-stage-file)
    ("u" "Unstage" jitte-unstage-file)
    ("d" "Discard" jitte-discard-file)]
   [("r" "Rename" jitte-file-rename)
    ("D" "Delete" jitte-file-delete)
    ("U" "Untrack" jitte-file-untrack)]
   [("c" "Checkout" jitte-file-checkout)
    ("b" "Blame" jitte-file-blame)]])

;;; Utilities

(defun jitte-read-revset (prompt &optional default)
  "Read a revset from the user with PROMPT.
Optional DEFAULT provides a default value."
  (read-string prompt default))

(defun jitte-read-bookmark (prompt &optional default)
  "Read a bookmark name from the user with PROMPT.
Optional DEFAULT provides a default value."
  (let ((bookmarks (jitte-bookmark-list)))
    (completing-read prompt bookmarks nil nil default)))

;;; Placeholder Actions

(defun jitte-apply ()
  "Apply changes."
  (interactive)
  (message "Apply not yet implemented"))

(defun jitte-cherry-pick ()
  "Cherry-pick changes."
  (interactive)
  (message "Cherry-pick not yet implemented"))

(defun jitte-bisect ()
  "Start bisecting."
  (interactive)
  (message "Bisect not yet implemented"))

(defun jitte-clone ()
  "Clone repository."
  (interactive)
  (call-interactively #'jitte-clone))

(defun jitte-diff-refresh ()
  "Refresh diff buffer."
  (interactive)
  (message "Diff refresh not yet implemented"))

(defun jitte-ediff ()
  "Show ediff."
  (interactive)
  (message "Ediff not yet implemented"))

(defun jitte-fetch ()
  "Fetch from remote."
  (interactive)
  (message "Fetch not yet implemented"))

(defun jitte-pull ()
  "Pull from remote."
  (interactive)
  (message "Pull not yet implemented"))

(defun jitte-ignore ()
  "Edit ignore file."
  (interactive)
  (message "Ignore not yet implemented"))

(defun jitte-display-repository ()
  "Display repository."
  (interactive)
  (message "Display repository not yet implemented"))

(defun jitte-log-refresh ()
  "Refresh log buffer."
  (interactive)
  (message "Log refresh not yet implemented"))

(defun jitte-merge ()
  "Merge changes."
  (interactive)
  (message "Merge not yet implemented"))

(defun jitte-remote ()
  "Manage remotes."
  (interactive)
  (jitte-remote))

(defun jitte-notes ()
  "Manage notes."
  (interactive)
  (message "Notes not yet implemented"))

(defun jitte-submodule ()
  "Manage submodules."
  (interactive)
  (message "Submodule not yet implemented"))

(defun jitte-subtree ()
  "Manage subtrees."
  (interactive)
  (message "Subtree not yet implemented"))

(defun jitte-push ()
  "Push to remote."
  (interactive)
  (message "Push not yet implemented"))

(defun jitte-patch ()
  "Manage patches."
  (interactive)
  (message "Patch not yet implemented"))

(defun jitte-file-rename ()
  "Rename file."
  (interactive)
  (message "File rename not yet implemented"))

(defun jitte-tag ()
  "Manage tags."
  (interactive)
  (message "Tag not yet implemented"))

(defun jitte-worktree ()
  "Manage worktrees."
  (interactive)
  (message "Worktree not yet implemented"))

(defun jitte-undo ()
  "Undo operation."
  (interactive)
  (when (y-or-n-p "Undo last operation? ")
    (jitte-run-jj-async "undo")
    (jitte-refresh)))

(defun jitte-unstage-all ()
  "Unstage all changes."
  (interactive)
  (message "Unstage all not yet implemented"))

(defun jitte-reverse ()
  "Reverse changes."
  (interactive)
  (message "Reverse not yet implemented"))

(defun jitte-revert ()
  "Revert changes."
  (interactive)
  (message "Revert not yet implemented"))

(defun jitte-am ()
  "Apply patches."
  (interactive)
  (message "Apply patches not yet implemented"))

(defun jitte-patch-apply ()
  "Apply patch."
  (interactive)
  (message "Patch apply not yet implemented"))

(defun jitte-reset ()
  "Reset changes."
  (interactive)
  (message "Reset not yet implemented"))

(defun jitte-reset-keep ()
  "Reset but keep changes."
  (interactive)
  (message "Reset keep not yet implemented"))

(defun jitte-show-refs ()
  "Show references."
  (interactive)
  (message "Show refs not yet implemented"))

(defun jitte-cherry ()
  "Cherry operations."
  (interactive)
  (message "Cherry not yet implemented"))

(defun jitte-stash ()
  "Manage stashes."
  (interactive)
  (message "Stash not yet implemented"))

;;; Commit Actions

(defun jitte-commit-create ()
  "Create a new commit."
  (interactive)
  (call-interactively #'jitte-describe))

(defun jitte-commit-amend ()
  "Amend current commit."
  (interactive)
  (call-interactively #'jitte-describe))

(defun jitte-commit-edit ()
  "Edit a commit."
  (interactive)
  (call-interactively #'jitte-edit))

(defun jitte-commit-reword ()
  "Reword commit message."
  (interactive)
  (call-interactively #'jitte-describe))

(defun jitte-commit-fixup ()
  "Create fixup commit."
  (interactive)
  (message "Fixup not yet implemented"))

(defun jitte-commit-squash ()
  "Squash commit."
  (interactive)
  (call-interactively #'jitte-squash))

(defun jitte-commit-augment ()
  "Augment commit."
  (interactive)
  (message "Augment not yet implemented"))

(defun jitte-commit-absorb ()
  "Absorb changes into commit."
  (interactive)
  (message "Absorb not yet implemented"))

(provide 'jitte-transient)

;;; jitte-transient.el ends here