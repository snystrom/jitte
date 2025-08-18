;;; jitte-process.el --- Process management for Jitte  -*- lexical-binding:t -*-

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

;; This library implements process management for Jitte, including
;; handling of jj processes and process output formatting.

;;; Code:

(require 'jitte-base)
(require 'magit-section)

(require 'with-editor)

;;; Options

(defcustom jitte-process-connection-type (not (eq system-type 'cygwin))
  "Connection type used for the jj process.

If nil, use pipes: this is usually more efficient, and works on Cygwin.
If t, use ptys: this enables Jitte to prompt for passphrases when needed."
  :group 'jitte-process
  :type '(choice (const :tag "Use ptys" t)
                 (const :tag "Use pipes" nil)))

(defcustom jitte-process-popup-time -1
  "Popup the process buffer if a command takes longer than this many seconds."
  :group 'jitte-process
  :type '(choice (const :tag "Never" -1)
                 (const :tag "Immediately" 0)
                 (integer :tag "After this many seconds")))

(defcustom jitte-process-log-max 32
  "Maximum number of sections to keep in a process log buffer.
When adding a new section would go beyond the limit, then the
oldest section is first removed."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-process
  :type 'integer)

(defcustom jitte-process-yes-or-no-prompt-regexp
  "\\(\\(Are you sure\\|Delete \\|Overwrite \\|Update \\|Restart \\).*? \\)?\\[\\([Yy]\\(?:es\\)?\\(?:/[Nn]o?\\)?\\|[Nn]o?\\(?:/[Yy]\\(?:es\\)?\\)?\\)\\]\\s-*[?:]?\\s-*$"
  "Regexp matching Yes-or-No prompts of jj and its subprocesses."
  :package-version '(jitte . "0.1.0")
  :group 'jitte-process
  :type 'regexp)

;;; Variables

(defvar jitte-process-buffer-name-format "*jitte: %s*"
  "Format string for process buffer names.")

(defvar-local jitte-process-command-line nil
  "Command line of the current process.")

(defvar jitte-hidden-commands nil
  "List of jj commands for which output should not be shown by default.")

;;; Process Buffer

(defun jitte-process-buffer (&optional nodisplay)
  "Display the current repository's process buffer.
If NODISPLAY is non-nil, just return the buffer."
  (interactive)
  (let ((toplevel (jitte-toplevel)))
    (unless toplevel
      (user-error "Not in a jj repository"))
    (let ((buffer (jitte-process-get-buffer)))
      (if nodisplay
          buffer
        (jitte-display-buffer buffer)))))

(defun jitte-process-get-buffer ()
  "Return the process buffer for the current repository."
  (let ((topdir (jitte-toplevel)))
    (unless topdir
      (error "Not in a jj repository"))
    (or (jitte-process-find-buffer topdir)
        (jitte-process-create-buffer topdir))))

(defun jitte-process-find-buffer (topdir)
  "Find existing process buffer for TOPDIR."
  (let ((name (format jitte-process-buffer-name-format
                      (file-name-nondirectory
                       (directory-file-name topdir)))))
    (get-buffer name)))

(defun jitte-process-create-buffer (topdir)
  "Create new process buffer for TOPDIR."
  (let* ((name (format jitte-process-buffer-name-format
                       (file-name-nondirectory
                        (directory-file-name topdir))))
         (buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (setq default-directory topdir)
      (jitte-process-mode)
      buffer)))

;;; Process Mode

(defvar jitte-process-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "k" #'jitte-process-kill)
    (define-key map "q" #'quit-window)
    (define-key map "g" #'jitte-refresh)
    map)
  "Keymap for `jitte-process-mode'.")

(define-derived-mode jitte-process-mode magit-section-mode "Jitte Process"
  "Mode for looking at jj process output."
  :group 'jitte-modes
  (hack-dir-local-variables-non-file-buffer)
  (setq truncate-lines t))

;;; Running Processes

(defun jitte-run-jj-with-input (input &rest args)
  "Run jj with ARGS, providing INPUT on stdin."
  (jitte-with-environment
    (with-temp-buffer
      (insert input)
      (let ((coding-system-for-read jitte-jj-output-coding-system)
            (exit-code (apply #'call-process-region
                              (point-min) (point-max)
                              (jitte-jj-executable-find "jj")
                              t t nil
                              (append jitte-jj-global-arguments args))))
        (if (zerop exit-code)
            (buffer-string)
          (error "jj command failed with exit code %d: %s"
                 exit-code (buffer-string)))))))

(defun jitte-run-jj-with-editor (&rest args)
  "Run jj with ARGS, using with-editor for editor commands."
  (with-editor "JJ_EDITOR"
    (apply #'jitte-run-jj-async args)))

(defun jitte-run-jj-with-logfile (file &rest args)
  "Run jj with ARGS, logging output to FILE."
  (let ((process (apply #'jitte-run-jj-async args)))
    (set-process-filter process
                        (lambda (proc output)
                          (with-temp-buffer
                            (insert output)
                            (append-to-file (point-min) (point-max) file))))
    process))

;;; Process Management

(defun jitte-process-kill ()
  "Kill the process at point."
  (interactive)
  (when-let ((section (magit-current-section))
             (process (magit-section-value section)))
    (when (process-live-p process)
      (kill-process process)
      (message "Killed process"))))

(defun jitte-process-insert-section (pwd program args &optional errcode errlog)
  "Insert a new process section.
PWD is the working directory, PROGRAM is the program name,
ARGS are the arguments, ERRCODE is the exit code, and ERRLOG
is error output."
  (magit-insert-section (process)
    (magit-insert-heading
      (format "%s %s" program (mapconcat #'identity args " ")))
    (when errlog
      (insert errlog))
    (when errcode
      (insert (format "Process exited with code %d\n" errcode)))))

;;; Process Filtering

(defun jitte-process-filter (process output)
  "Filter function for jj processes."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t))
        (goto-char (process-mark process))
        (insert output)
        (set-marker (process-mark process) (point))))))

(defun jitte-process-sentinel (process event)
  "Sentinel function for jj processes."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t))
        (goto-char (process-mark process))
        (insert (format "Process %s %s"
                         (process-name process)
                         (substring event 0 -1)))
        (when (memq (process-status process) '(exit signal))
          (insert (format " with exit code %d" (process-exit-status process))))
        (insert "\n")
        (set-marker (process-mark process) (point))))))

;;; Async Process Utilities

(defun jitte-start-process (name buffer program &rest args)
  "Start a process with NAME in BUFFER running PROGRAM with ARGS."
  (let ((process (apply #'start-process name buffer program args)))
    (set-process-filter process #'jitte-process-filter)
    (set-process-sentinel process #'jitte-process-sentinel)
    process))

(defun jitte-process-file (process &optional infile buffer display &rest args)
  "Wrapper around `process-file' with Jitte environment."
  (jitte-with-environment
    (apply #'process-file process infile buffer display args)))

;;; Process Output Washing

(defun jitte-wash-sequence (sequence)
  "Remove empty strings and nil values from SEQUENCE."
  (delq nil (delete "" sequence)))

(defun jitte-process-wash-status (status)
  "Wash jj status output."
  (when (looking-at "^\\(.*\\)$")
    (let ((file (match-string 1)))
      (delete-region (point) (1+ (line-end-position)))
      (magit-insert-section (file file)
        (insert file "\n")))))

;;; Error Handling

(define-error 'jitte-process-error "Jj process error")

(defun jitte-process-error (process &rest args)
  "Signal a process error for PROCESS with ARGS."
  (signal 'jitte-process-error
          (cons (format "Process %s failed" (process-name process)) args)))

;;; Integration

(defun jitte-process-setup ()
  "Setup process handling for Jitte."
  (add-hook 'jitte-mode-hook #'jitte-process-mode-setup))

(defun jitte-process-mode-setup ()
  "Setup process mode for current buffer."
  (when (derived-mode-p 'jitte-process-mode)
    (setq buffer-read-only t)))

(provide 'jitte-process)

;;; jitte-process.el ends here