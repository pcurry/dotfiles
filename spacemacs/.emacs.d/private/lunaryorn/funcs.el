;;; funcs.el --- Personal functions                  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sebastian Wiesner

;; Author: Sebastian Wiesner <swiesner@MB-Pro-Sebastian-Wiesner.local>

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

(defun lunaryorn-insert-current-date (iso)
  "Insert the current date at point.

When ISO is non-nil, insert the date in ISO 8601 format.
Otherwise insert the date as Mar 04, 2014."
  (interactive "P")
  (insert (format-time-string (if iso "%F" "%b %d, %Y"))))

(define-skeleton lunaryorn-insert-mit/x11
  "Insert the MIT/X11 license into the current buffer"
  nil
  @ "Copyright (c) " (format-time-string "%Y")
  "  " user-full-name " <" user-mail-address ">

"
  @
  "Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
"
  @
  '(pcase-let ((`(,end ,mid ,start . _) skeleton-positions))
     ;; We use markers to keep the positions updated after commenting
     (let ((mid (copy-marker mid t))
           (end (copy-marker end t)))
       (comment-region start end)
       (fill-region mid end)))
  "\n"-)

;;; funcs.el ends here
