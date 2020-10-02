;; Copyright (C) 2016, 2018-2019 Free Software Foundation, Inc

;; Author: Rocky Bernstein <rocky@gnu.org>
;; Author: Sean Farley <sean@farley.io>

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
;; ipdb: "interactive" debugger extension to Python debugger pdb

(eval-when-compile (require 'cl-lib))   ;For setf.

(require 'load-relative)
(require 'realgud)

(defvar realgud-pat-hash)
(declare-function make-realgud-loc-pat 'realgud-regexp)

(defvar realgud:xdebug-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

(declare-function make-realgud-loc 'realgud-loc)

;; -------------------------------------------------------------------
;; User-definable variables
;;

;; realgud-loc-pat that describes a xdebug location generally shown
;; before a command prompt.
;;
;; Program-location lines look like this:
;;   > /usr/bin/zonetab2pot.py(15)<module>()
;; or MS Windows:
;;   > c:\\mydirectory\\gcd.py(10)<module>
(setf (gethash "loc" realgud:xdebug-pat-hash)
      (make-realgud-loc-pat
       :regexp "Debugging file://\\(?:/\\|[[:alpha:]]\\|[[:punct:]]\\|[[:digit:]]\\)*[[:space:]]\\s(ID:[[:space:]][[:digit:]]*/\\(?:[[:alpha:]]\\|[[:punct:]]\\)*\\s)"
       :file-group 1
       :line-group 2))

(setf (gethash "prompt" realgud:xdebug-pat-hash)
      (make-realgud-loc-pat
       :regexp   "^(cmd) "
       ))

;;  realgud-loc-pat that describes a Python backtrace line.
;; (setf (gethash "lang-backtrace" realgud:xdebug-pat-hash)
;;       realgud-python-backtrace-loc-pat)

;; (setf (gethash "debugger-backtrace" realgud:xdebug-pat-hash)
;;       realgud:python-trepan-backtrace-pat)

;;  realgud-loc-pat that describes a line a Python "info break" line.
;; For example:
;; 1   breakpoint    keep y   at /usr/local/bin/trepan3k:7
;; (setf (gethash "debugger-breakpoint" realgud:xdebug-pat-hash)
;;       (make-realgud-loc-pat
;;        :regexp (format "^breakpoint_set -t line -f file://+\\(.+\\) -n %s"
;; 		       realgud:regexp-captured-num)
;;        :num 1
;;        :string 1
;;        :line-group 2
;;        :file-group 1))


;;  realgud-loc-pat that describes a "breakpoint set" line. For example:
;;     Breakpoint 1 at /usr/bin/xdebug:7
(setf (gethash "brkpt-set" realgud:xdebug-pat-hash)
      (make-realgud-loc-pat
       :regexp "^breakpoint_set -t line -f file://+\\(.+\\) -n \\([0-9]+\\)"
       :num 1
       :file-group 1
       :line-group 2))

;; realgud-loc-pat that describes a "delete breakpoint" line
;; Python 3 includes a file name and line number; Python 2 doesn't
(setf (gethash "brkpt-del" realgud:xdebug-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Deleted breakpoint \\([0-9]+\\)"
       :num 1))

(setf (gethash "font-lock-keywords" realgud:xdebug-pat-hash)
      '(
	;; The frame number and first type name, if present.
	("^\\(->\\|##\\)\\([0-9]+\\) \\(<module>\\)? *\\([a-zA-Z_][a-zA-Z0-9_]*\\)(\\(.+\\))?"
	 (2 realgud-backtrace-number-face)
	 (4 font-lock-function-name-face nil t))     ; t means optional.

	;; Parameter sequence, E.g. gcd(a=3, b=5)
	;;                             ^^^^^^^^^
	("(\\(.+\\))"
	 (1 font-lock-variable-name-face))

	;; File name. E.g  file '/test/gcd.py'
	;;                 ------^^^^^^^^^^^^-

	;; (cmd) step_out 
	;; 3 | step_out > break/ok
	;; 3 | file:///usr/Data_2/Programming/drupal-9.0.6/index.php:14

	("file:/// \\([^ ]+*\\)'"
	 (1 realgud-file-name-face))

	;; Line number. E.g. at line 28
        ;;                  ---------^^
	(":\\([0-9]+\\)$"
	 (1 realgud-line-number-face))

	;; Function name.
	("{\\([a-zA-Z_][a-zA-Z0-9_]*\\)}"
	 (1 font-lock-type-face)
	 (2 font-lock-function-name-face))
	;; (xdebug-frames-match-current-line
	;;  (0 xdebug-frames-current-frame-face append))
	))

(setf (gethash "xdebug" realgud-pat-hash) realgud:xdebug-pat-hash)

(defvar realgud:xdebug-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'finish' and the value is
the xdebug command to use, like 'return'")

;; Mappings between xdebug-specific names and GUD names
(setf (gethash "kill"             realgud:xdebug-command-hash) "stop")
(setf (gethash "debugger-backtrace" realgud:xdebug-command-hash) "stack_get")
(setf (gethash "continue"        realgud:xdebug-command-hash) "run")
(setf (gethash "next"        realgud:xdebug-command-hash) "step_over")
(setf (gethash "step"        realgud:xdebug-command-hash) "step_into")
(setf (gethash "finish"           realgud:xdebug-command-hash) "step_out")

;; Clear in Python does both the usual “delete” and “clear”
;; (setf (gethash "delete"           realgud:xdebug-command-hash) "clear %p")
;; (setf (gethash "clear"            realgud:xdebug-command-hash) "clear %X:%l")
;; Use ‘!’ instead of ‘p’, since ‘p’ only works for expressions, not statements
(setf (gethash "eval"             realgud:xdebug-command-hash) "eval")
(setf (gethash "info-breakpoints" realgud:xdebug-command-hash) "break_list")

;; Unsupported features:
(setf (gethash "shell" realgud:xdebug-command-hash) "*not-implemented*")
(setf (gethash "frame" realgud:xdebug-command-hash) "*not-implemented*")

(setf (gethash "xdebug" realgud-command-hash) realgud:xdebug-command-hash)

(provide-me "realgud--xdebug-")
