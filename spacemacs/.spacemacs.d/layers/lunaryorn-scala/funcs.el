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

(require 'dash)

(defun lunaryorn-scala/find-config-file-in-sbt-project (file &optional _checker)
  "Find a config FILE in sbt project/ directories."
  (-when-let* ((file-name (buffer-file-name))
               (root-dir (locate-dominating-file file-name "build.sbt"))
               (project-dir (expand-file-name "project/" root-dir))
               (config-file (expand-file-name file project-dir)))
    (when (file-exists-p config-file)
      config-file)))

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
