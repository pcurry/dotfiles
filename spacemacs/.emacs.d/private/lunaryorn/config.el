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

;; Additional fonts for special characters and fallbacks
;; Test range: 🐷 ⊄ ∫ 𝛼 α 🜚
(when (eq system-type 'darwin)
  ;; Colored Emoji on OS X
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji")
                    nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'append))
(set-fontset-font t 'mathematical (font-spec :family "XITS Math") nil 'append)
;; Fallback for Greek characters which Source Code Pro doesn't provide.
(set-fontset-font t 'greek (pcase system-type
                             (`darwin (font-spec :family "Menlo"))
                             (_ (font-spec :family "DejaVu Sans Mono")))
                  nil 'append)

;; A general fallback for all kinds of unknown symbols
(set-fontset-font t nil (font-spec :family "Apple Symbols") nil 'append)

;; Basic configuration
(setq line-spacing 0.1
      ;; Keep right option modifier free for accented input
      mac-right-option-modifier 'none)

;; Use jk to leave insert mode
(setq evil-escape-key-sequence "jk")

;; Version control settings
(setq magit-push-always-verify nil      ; Don't nag me magit
      vc-follow-symlinks t              ; Follow symlinks to VC dirs
      )

;; Shut up, Dired!
(setq dired-omit-verbose nil
      dired-recursive-copies 'always)

(provide 'config)
;;; config.el ends here
