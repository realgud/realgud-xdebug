;; Copyright (C) 2016-2017, 2019 Free Software Foundation, Inc

;; Author: Sean Farley <sean@farley.io>, Rocky Bernstein (rocky@gnu.org)

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.or/licenses/>.


(require 'comint)
(require 'realgud)
(require 'load-relative)

(declare-function realgud-lang-mode? 'realgud-lang)
(declare-function realgud-parse-command-arg 'realgud-core)
(declare-function realgud-query-cmdline 'realgud-core)
(declare-function realgud-suggest-invocation 'realgud-core)
(declare-function realgud-get-cmdbuf   'realgud-buffer-helper)

;; FIXME: I think the following could be generalized and moved to
;; realgud-... probably via a macro.
(defvar realgud--xdebug-minibuffer-history nil
  "minibuffer history list for the command `xdebug'.")

(defvar realgud--xdebug-remote-minibuffer-history nil
  "minibuffer history list for the command `xdebug-remote'.")

(easy-mmode-defmap xdebug-minibuffer-local-map
  '(("\C-i" . comint-dynamic-complete-filename))
  "Keymap for minibuffer prompting of debugger startup command."
  :inherit minibuffer-local-map)

;; FIXME: I think this code and the keymaps and history
;; variable chould be generalized, perhaps via a macro.
(defun xdebug-query-cmdline (&optional opt-debugger)
  "xdebug"
  )

(defun xdebug-parse-cmd-args (orig-args)
  "Parse command line ORIG-ARGS for the annotate level and name of script to debug.

ORIG-ARGS should contain a tokenized list of the command line to run.

We return the a list containing:
* the command processor (e.g. python) and it's arguments if any - a list of strings
* the name of the debugger given (e.g. xdebug) and its arguments - a list of strings
* the script name and its arguments - list of strings
* whether the annotate or emacs option was given ('-A', '--annotate' or '--emacs) - a boolean

For example for the following input:
  (map 'list 'symbol-name
   '(python2.6 -O -Qold ./gcd.py a b))

we might return:
   ((\"python2.6\" \"-O\" \"-Qold\") (\"xdebug\") (\"/tmp/gcd.py\" \"a\" \"b\") nil)

Note that the script name path has been expanded via `expand-file-name'.
"

  ;; Parse the following kind of pattern:
  ;;  [python python-options] xdebug xdebug-options script-name script-options
  (list "/home/fermin/Programming/drupal-9.0.6/dbgpClient" nil nil))

;; To silence Warning: reference to free variable
(defvar realgud--xdebug-command-name)

(defun xdebug-suggest-invocation (debugger-name)
  "Suggest a xdebug command invocation via `realgud-suggest-invocaton'"
  (realgud-suggest-invocation (or realgud--xdebug-command-name debugger-name)
			      realgud--xdebug-minibuffer-history
			      "/home/fermin/Programming/drupal-9.0.6/dbgpClient"))

(defun xdebug-reset ()
  "Xdebug cleanup - remove debugger's internal buffers (frame,
breakpoints, etc.)."
  (interactive)
  ;; (xdebug-breakpoint-remove-all-icons)
  (dolist (buffer (buffer-list))
    (when (string-match "\\*xdebug-[a-z]+\\*" (buffer-name buffer))
      (let ((w (get-buffer-window buffer)))
        (when w
          (delete-window w)))
      (kill-buffer buffer))))

(defun realgud--xdebug-customize ()
  "Use `customize' to edit the settings of the `xdebug' debugger."
  (interactive)
  (customize-group 'realgud--xdebug))

(provide-me "realgud--xdebug-")
