;;; keybindings.el --- Personal keybindings          -*- lexical-binding: t; -*-

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

;;

;;; Code:

(spacemacs/declare-prefix "oe" "personal/eval")
(spacemacs/declare-prefix "of" "personal/files")
(spacemacs/declare-prefix "oi" "personal/insert")
(spacemacs/declare-prefix "ot" "personal/toggles")

;; File bindings
(spacemacs/set-leader-keys
  ;; Bindings in private key spacpacemacs
  "oR"  'lunaryorn/recompile-packages
  "oee" 'eval-expression
  "ofg" 'lunaryorn/browse-feature-url
  "ofi" 'lunaryorn/open-in-intellij
  "oid" 'lunaryorn/insert-current-date
  "oip" 'lunaryorn/insert-page-break-line
  )

;;; keybindings.el ends here
