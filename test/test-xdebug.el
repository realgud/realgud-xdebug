;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(load-file "../xdebug/xdebug.el")

(eval-when-compile (defvar test:run-process-save))

(declare-function xdebug-parse-cmd-args 'realgud:xdebug-core)
(declare-function realgud:xdebug        'realgud:xdebug)
(declare-function __FILE__            'load-relative)

(test-simple-start)

;; Save value realgud:run-process and change it to something we want
(setq test:run-process-save (symbol-function 'realgud:run-process))
(defun realgud:run-process(debugger-name script-filename cmd-args
					 minibuffer-histroy &optional no-reset)
  "Fake realgud:run-process used in testing"
  (note
   (format "%s %s %s" debugger-name script-filename cmd-args))
  (assert-equal "xdebug" debugger-name "debugger name gets passed")
  (generate-new-buffer "*cmdbuf-test*")
  )

(note "xdebug-parse-cmd-args")
(assert-equal (list nil '("xdebug") (list (expand-file-name "foo")) nil)
	      (xdebug-parse-cmd-args '("xdebug" "foo")))

;; Restore the old value of realgud:run-process
(fset 'realgud:run-process test:run-process-save)

(end-tests)
