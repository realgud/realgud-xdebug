;;; track-mode.el --- tack-mode for realgud-xdebug  -*- lexical-binding: t; -*-

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
;; PHP "xdebug" Debugger tracking a comint buffer.

;;; Code:

;;;; The requires
(eval-when-compile (require 'cl-lib))

(require 'realgud)
(require 'load-relative)

(require-relative-list '("core" "init") "realgud:xdebug-")

(realgud-track-mode-vars "realgud:xdebug")

(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud-track-mode-setup 'realgud-track-mode)
(declare-function realgud:track-mode-hook 'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)


(defun realgud:xdebug-track-mode-hook()
  "Hook for `xdebug-track-mode'."
  (if realgud:xdebug-track-mode
      (progn
	(use-local-map realgud:xdebug-track-mode-map)
	(realgud-track-mode-setup 't)
	(message "using xdebug mode map"))
    (message "xdebug track-mode-hook disable called")))

(define-minor-mode realgud:xdebug-track-mode
  "Minor mode for tracking xdebug source locations inside a process
shell via `realgud.  If called interactively with no prefix argument,
the mode is toggled. A prefix argument, captured as ARG, enables the
mode if the argument is positive, and disables it otherwise."
  :init-value nil
  :global nil
  :group 'realgud:xdebug
  :keymap realgud:xdebug-track-mode-map
  (realgud:track-set-debugger "realgud:xdebug")
  (if realgud:xdebug-track-mode
      (progn
	(realgud-track-mode-setup 't)
	(realgud:xdebug-track-mode-hook))
    (progn
      (setq realgud-track-mode nil))))

(provide-me "realgud:xdebug-")
