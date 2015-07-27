;;; packages.ex --- lunaryorn Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 Sebastian Wiesner <swiesner@lunaryorn.com>
;;
;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/lunaryorn/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq lunaryorn-packages
    '(
      osx-trash
      whitespace
      whitespace-cleanup-mode
      helm-unicode
      bug-reference
      auto-insert
      ))

;; List of packages to exclude.
(setq lunaryorn-excluded-packages '())

(defun lunaryorn/init-osx-trash ()
  "Initialize osx-trash."
  (use-package osx-trash
    :if (eq system-type 'darwin)
    :init (osx-trash-setup)))

(defun lunaryorn/init-whitespace-cleanup-mode ()
  "Initialize whitespace-cleanup-mode."
  (use-package whitespace-cleanup-mode
    :diminish (whitespace-cleanup-mode . " Ⓧ")
    :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
            (add-hook hook #'whitespace-cleanup-mode))))

(defun lunaryorn/init-helm-unicode ()
  (use-package helm-unicode
    :defer t
    :init (evil-leader/set-key "iu" #'helm-unicode)))

(defun lunaryorn/init-auto-insert ()
  (use-package auto-insert
    :defer t
    :init (evil-leader/set-key "ia" #'auto-insert)))

(defun lunaryorn/init-bug-reference ()
  (use-package bug-reference
    :defer t
    :init (progn (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
                 (add-hook 'text-mode-hook #'bug-reference-mode))))

(defun lunaryorn/post-init-flycheck ()
  (global-flycheck-mode 1))

(defun lunaryorn/post-init-whitespace ()
  (global-whitespace-mode 1)
  (use-package whitespace
    :defer t
    :init (evil-leader/set-key "dw" #'whitespace-cleanup)
    :config (setq whitespace-style '(face
                                     indentation
                                     space-after-tab
                                     space-before-tab
                                     tab-mark
                                     empty
                                     trailing
                                     lines-tail)
                  whitespace-line-column nil)))
