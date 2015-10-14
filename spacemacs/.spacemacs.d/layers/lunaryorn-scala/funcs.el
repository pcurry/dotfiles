;;; funcs.el --- Personal scala functions            -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sebastian Wiesner

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>

;; This file is not part of GNU Emacs.

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

;; Personal functions for scala programming

;;; Code:

(defun lunaryorn-scala/pop-to-sbt-frame ()
  "Open SBT REPL for this project in a new frame."
  (interactive)
  ;; Start SBT when no running, taken from `sbt:command'
  (when (not (comint-check-proc (sbt:buffer-name)))
    (sbt:run-sbt))

  (let ((display-buffer-overriding-action '(display-buffer-pop-up-frame)))
    (pop-to-buffer (sbt:buffer-name))
    ;; Dedicate the new window to the SBT REPL
    (set-window-dedicated-p (selected-window) t)))

;;; funcs.el ends here
