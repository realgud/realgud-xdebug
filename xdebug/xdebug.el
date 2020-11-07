;; Copyright (C) 2016, 2019 Free Software Foundation, Inc

;; Author: Fermin Munoz <fmfs@posteo.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;  `xdebug' Main interface to xdebug via Emacs
(require 'load-relative)

(require 'realgud)
(require-relative-list '("core" "track-mode") "realgud:xdebug-")

;; This is needed, or at least the docstring part of it is needed to
;; get the customization menu to work in Emacs 24.
(defgroup realgud:xdebug nil
  "The realgud interface to the PHP xdebug debugger"
  :group 'realgud
  :version "25.1")

(declare-function make-realgud-loc "realgud-loc" (a b c d e f))

;; -------------------------------------------------------------------
;; User-definable variables
;;

(defcustom realgud:xdebug-command-name
  "/home/fermin/Programming/drupal-9.0.6/dbgpClient"
  "File name for executing xdebug and command options.
This should be an executable on your path, or an absolute file name."
  :type 'string
  :group 'realgud:xdebug)
;; -------------------------------------------------------------------
;; The end.
;;

(declare-function realgud:xdebug-track-mode       'realgud:xdebug-track-mode)
(declare-function xdebug-query-cmdline    'realgud:xdebug-core)
(declare-function xdebug-parse-cmd-args   'realgud:xdebug-core)
(declare-function realgud:run-process        'realgud-core)
(declare-function realgud:run-debugger 'realgud:run)


(defun realgud:xdebug-break-line ()
  "Add a breakpoint in the current line.
This is a hack, pretty unstable"
  (interactive)
  (let* ((debug-proc (get-process "xdebug"))
	 (command (replace-regexp-in-string "%l" "%d"
					    (replace-regexp-in-string "%X" "%s" (gethash "break" realgud:xdebug-command-hash))))
	 (current-line (line-number-at-pos)))
    (if debug-proc
	(comint-send-string debug-proc (concat (format command
						       (buffer-file-name) current-line) "\n") )
      (message "xdebug process not found."))))

;;;###autoload
(defun realgud:xdebug (&optional opt-cmd-line no-reset)
  "Invoke the xdebug Python debugger and start the Emacs user interface.

String OPT-CMD-LINE specifies how to run xdebug. You will be prompted
for a command line is one isn't supplied.

OPT-COMMAND-LINE is treated like a shell string; arguments are
tokenized by `split-string-and-unquote'. The tokenized string is
parsed by `xdebug-parse-cmd-args' and path elements found by that
are expanded using `realgud:expand-file-name-if-exists'.

Normally, command buffers are reused when the same debugger is
reinvoked inside a command buffer with a similar command. If we
discover that the buffer has prior command-buffer information and
NO-RESET is nil, then that information which may point into other
buffers and source buffers which may contain marks and fringe or
marginal icons is reset. See `loc-changes-clear-buffer' to clear
fringe and marginal icons.
"
  (interactive)
  (let ((cmd-buf (realgud:run-debugger "xdebug" 'xdebug-query-cmdline
                                       'xdebug-parse-cmd-args
                                       'realgud--xdebug-minibuffer-history
                                       nil))
        )
    cmd-buf)
  )



;;;###autoload
(defalias 'xdebug 'realgud:xdebug)

(provide-me "realgud-")
