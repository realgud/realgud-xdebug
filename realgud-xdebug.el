;;; realgud-xdebug.el --- Realgud front-end to xdebug  -*- lexical-binding: t -*-

;; Author: Fermin Munoz <fmfs@posteo.net>
;; Version: 0.0.1
;; Package-Type: multi
;; Package-Requires: ((realgud "1.5.0") (load-relative "1.3.1") (emacs "25"))
;; URL: https://github.com/realgud/realgud-xdebug
;; Compatibility: GNU Emacs 25.x

;; Copyright (C) 2019, 2020 Free Software Foundation, Inc

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

;; realgud support for the PHP xdebug
;; See https://xdebug.org/docs/dbgpClient

;;; Code:

;; Press C-x C-e at the end of the next line configure the program in
;; for building via "make" to get set up.
;; (compile (format "EMACSLOADPATH=:%s:%s:%s:%s ./autogen.sh" (file-name-directory (locate-library "loc-changes.elc")) (file-name-directory (locate-library "test-simple.elc")) (file-name-directory (locate-library "load-relative.elc")) (file-name-directory (locate-library "realgud.elc"))))

(require 'load-relative)

(defgroup realgud-xdebug  nil
  "Realgud interface to xdebug"
  :group 'realgud
  :version "25.1")

(require-relative-list '( "./xdebug/xdebug" ) "realgud-")

(provide-me)
;;; realgud-xdebug.el ends here
