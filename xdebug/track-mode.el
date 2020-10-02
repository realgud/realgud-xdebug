;; Copyright (C) 2016, 2018, 2019 Free Software Foundation, Inc

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
;; Python "xdebug" Debugger tracking a comint buffer.

(eval-when-compile (require 'cl-lib))

(require 'realgud)
(require 'load-relative)

(require-relative-list '("core" "init") "realgud--xdebug-")

(realgud-track-mode-vars "xdebug")

(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud-track-mode-setup 'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-python-populate-command-keys 'realgud-lang-python)
(declare-function realgud:xdebug-completion-at-point 'realgud:xdebug-core)

(realgud-python-populate-command-keys xdebug-track-mode-map)

(defun xdebug-track-mode-hook()
  (if xdebug-track-mode
      (progn
        (use-local-map xdebug-track-mode-map)
	(realgud:remove-ansi-schmutz)
        (message "using xdebug mode map"))
    (message "xdebug track-mode-hook disable called")
    )
  )

(define-minor-mode xdebug-track-mode
  "Minor mode for tracking xdebug source locations inside a process shell via realgud. xdebug is a Python debugger based on ipython.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

a process shell.

\\{xdebug-track-mode-map}
"
  :init-value nil
  ;; :lighter " xdebug"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:xdebug
  :keymap xdebug-track-mode-map
  (realgud:track-set-debugger "xdebug")
  (if xdebug-track-mode
      (progn
	(realgud-track-mode-setup 't)
        (xdebug-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(provide-me "realgud--xdebug-")
