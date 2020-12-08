;;; init.el --- init file for xdebug Emacs interface  -*- lexical-binding: t; -*-

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

;;; Commentary:
;; xdebug: "interactive" debugger extension to PHP debugger xdebug

;;; Code:

;;;; The requires

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
;; 1 | step_into > break/ok
;; 1 | file:///usr/Data_2/Programming/drupal-9.0.6/.ht.router.php:27
;; 
;; (cmd)

(setf (gethash "loc" realgud:xdebug-pat-hash)
      (make-realgud-loc-pat
       :regexp"file://\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\):\\([0-9]+\\)"
       :file-group 1
       :line-group 2))

(setf (gethash "prompt" realgud:xdebug-pat-hash)
      (make-realgud-loc-pat
       :regexp   "^(cmd) "))

;;  realgud-loc-pat that describes a xdebug backtrace line.
(setf (gethash "debugger-backtrace" realgud:xdebug-pat-hash)
      (make-realgud-loc-pat
       :regexp "[^z-a][[:digit:]]+ | \\([0-9]+\\):[[:space:]]file://\\(\\(?:[a-zA-Z]\\)?[-a-zA-Z0-9_/.\\\ ]+\\):\\([0-9]+\\)"
       :num 1
       :file-group 2
       :line-group 3))

;;  realgud-loc-pat that describes a line a xdebug "info break" line.
;; For example:
;; 1   breakpoint    keep y   at /usr/local/bin/trepan3k:7
;; (rx (* anything) (literal "|") space (literal "file://") (* (or (eval (f-path-separator)) alpha punctuation num))
;;     (literal ":") (+ num)  )
(setf (gethash "debugger-breakpoint" realgud:xdebug-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^[[:digit:]]+[[:space:]]|[[:space:]]file://\\(?:/\\|[[:alpha:]]\\|[[:punct:]]\\|[[:digit:]]\\)*:%s"
		       realgud:regexp-captured-num)
       :file-group 1
       :line-group 2
       ))


;;  realgud-loc-pat that describes a "breakpoint set" line. For example:
;;     Breakpoint 1 at /usr/bin/xdebug:7
;; (rx anything (literal "breakpoint_set") space (literal "-t line -f") space
;;  ;this line it's not the same (literal "file://") (* (or (eval (f-path-separator)) alpha punctuation num))
;;     space (literal "-n") space (+ num) (* anything)
;;     (+ num) space (literal "|") space (literal "breakpoint_set") anything
;;     (+ num) space (literal "|") space (literal "Breakpoint") space (literal "set") space
;;     (literal "with") space (literal "ID") space (+ num) eol)
;; 
(setf (gethash "brkpt-set" realgud:xdebug-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "[^z-a]breakpoint_set[[:space:]]-t line -f[[:space:]]file://\\(\\(?:[a-zA-Z]:\\)?[-a-zA-Z0-9_/.\\\\ ]+\\)*[[:space:]]-n[[:space:]]%s[^z-a]*[[:digit:]]+[[:space:]]|[[:space:]]breakpoint_set[^z-a][[:digit:]]+[[:space:]]|[[:space:]]Breakpoint[[:space:]]set[[:space:]]with[[:space:]]ID[[:space:]]%s$"
		       realgud:regexp-captured-num realgud:regexp-captured-num)
       :file-group 1
       :line-group 2
       :num 3))

;; realgud-loc-pat that describes a "delete breakpoint" line
;; Python 3 includes a file name and line number; Python 2 doesn't

;; (rx bol (+ num ) space (literal "|") space (literal "breakpoint_remove")
;;     eol anything bol (+ num) space (literal "|") space anything space (syntax open-parenthesis)
;;     (+ num) (syntax close-parenthesis) space (+ num) space (literal "line:") space
;;     (literal "file://") (* (or (eval (f-path-separator)) alpha punctuation num)) (literal ":") (+ num))
(setf (gethash "brkpt-del" realgud:xdebug-pat-hash)
      (make-realgud-loc-pat
       :regexp "[^z-a]breakpoint_remove[[:space:]]-d \\(\\([0-9]+ *\\)+\\)$"
       :num 1))

(setf (gethash "font-lock-keywords" realgud:xdebug-pat-hash)
      '(
	;; The frame number and first type name, if present.
	("^\\(->\\|##\\)\\([0-9]+\\) \\(<module>\\)? *\\([a-zA-Z_][a-zA-Z0-9_]*\\)(\\(.+\\))?"
	 (2 realgud-backtrace-number-face)
	 (4 font-lock-function-name-face nil t))     ; t means optional.

	;; Parameter sequence, E.g. gcd(a=3, b=5)
	;;       (rx (literal "$") (+ alnum) )                      ^^^^^^^^^
	("\\$[[:alnum:]]+"
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
	(":\\([0-9]+\\)?:"
	 (1 realgud-line-number-face))

	;; Function name.
	("{\\([a-zA-Z_][a-zA-Z0-9_]*\\)}"
	 (1 font-lock-function-name-face))
	;; (xdebug-frames-match-current-line
	;;  (0 xdebug-frames-current-frame-face append))
	))

(setf (gethash "xdebug" realgud-pat-hash) realgud:xdebug-pat-hash)

(defvar realgud:xdebug-command-hash (make-hash-table :test 'equal)
  "Pair value-xdebug command.")

;; Supported features
(setf (gethash "kill"             realgud:xdebug-command-hash) "stop"
      (gethash "quit"      realgud:xdebug-command-hash) "detach"
      (gethash "backtrace" realgud:xdebug-command-hash) "stack_get"
      (gethash "continue"        realgud:xdebug-command-hash) "run"
      (gethash "next"        realgud:xdebug-command-hash) "step_over"
      (gethash "step"        realgud:xdebug-command-hash) "step_into"
      (gethash "finish"           realgud:xdebug-command-hash) "step_out"
      (gethash "break"           realgud:xdebug-command-hash) "breakpoint_set -t line -f file://%X -n %l"
      (gethash "delete"   realgud:xdebug-command-hash) "breakpoint_remove -d %p"
      (gethash "eval"             realgud:xdebug-command-hash) "property_get -n %s"
      (gethash "info-breakpoints" realgud:xdebug-command-hash) "breakpoint_list")

;; Unsupported features:
(setf (gethash "shell" realgud:xdebug-command-hash) "*not-implemented*"
      (gethash "frame" realgud:xdebug-command-hash) "*not-implemented*"
      (gethash "clear"    realgud:xdebug-command-hash) "*not-implemented*"
      (gethash "jump"    realgud:xdebug-command-hash) "*not-implemented*"
      (gethash "up"       realgud:xdebug-command-hash) "*not-implemented*"
      (gethash "down"     realgud:xdebug-command-hash) "*not-implemented*")


(setf (gethash "xdebug" realgud:variable-basename-hash) "realgud:xdebug")
(setf (gethash "xdebug" realgud-command-hash) realgud:xdebug-command-hash)

(provide-me "realgud:xdebug-")
