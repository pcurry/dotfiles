;;; funcs.el --- Personal functions                  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sebastian Wiesner

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>

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

(require 'lisp-mnt)
(require 'find-func)

(defun lunaryorn/insert-page-break-line ()
  "Insert a new line with a page break at point."
  (interactive)
  (insert "\n\n"))

(defun lunaryorn/browse-feature-url (feature)
  "Browse the URL of the given FEATURE.

Interactively, use the symbol at point, or prompt, if there is
none."
  (interactive
   (let ((symbol (or (symbol-at-point)
                     (completing-read "Feature: " features nil
                                      'require-match))))
     (list symbol)))
  (let* ((library (if (symbolp feature) (symbol-name feature) feature))
         (library-file (find-library-name library)))
    (when library-file
      (with-temp-buffer
        (insert-file-contents library-file)
        (let ((url (lm-header "URL")))
          (if url
              (browse-url url)
            (user-error "Library %s has no URL header" library)))))))

(defun lunaryorn/recompile-packages ()
  "Recompile all packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

(defun lunaryorn/open-in-intellij ()
  "Open the current file in IntelliJ IDEA."
  (interactive)
  (let ((idea (executable-find "idea")))
    (unless idea
      (user-error "IntelliJ launcher does not exist.
Create with Tools -> Create Command-line launcher in IntelliJ"))
    (unless (= 0 (call-process idea nil nil nil
                               "--line" (number-to-string (line-number-at-pos))
                               (expand-file-name (buffer-file-name))))
      (error "IntelliJ failed"))))

;;; funcs.el ends here
