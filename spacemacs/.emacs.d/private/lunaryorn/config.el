;;; config.el --- Personal configuration layer for spacemacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sebastian Wiesner

;; Author: Sebastian Wiesner <swiesner@lunaryorn-air>
;; Keywords: abbrev

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

;; My personal Spacemacs configuration layer.

;;; Code:

;; Basic configuration
(setq line-spacing 0.1
      ;; Keep right option modifier free for accented input
      mac-right-option-modifier 'none)

;; Use jk to leave insert mode
(setq evil-escape-key-sequence "jk")

;; Don't nag me magit
(setq magit-push-always-verify nil)

(provide 'config)
;;; config.el ends here
