;;; packages.el --- lunaryorn-spacemacs-fixes Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq lunaryorn-spacemacs-fixes-packages
      '(
        company-flx
        ensime
        ruby-tools
        bundler
        robe
        ruby-test-mode
        markdown-mode
        ))

(setq lunaryorn-spacemacs-fixes-excluded-packages '())

(defun lunaryorn-spacemacs-fixes/init-company-flx ()
  ;; See https://github.com/syl20bnr/spacemacs/pull/3468
  (use-package company-flx
    :defer t)
  (spacemacs|use-package-add-hook company
    :pre-config
    (company-flx-mode)))

(defun lunaryorn-spacemacs-fixes/pre-init-ensime ()
  ;; See https://github.com/syl20bnr/spacemacs/pull/3482
  (dolist (prefix '(("mb" . "scala/build")
                    ("mc" . "scala/check")
                    ("md" . "scala/debug")
                    ("me" . "scala/errors")
                    ("mg" . "scala/goto")
                    ("mh" . "scala/docs")
                    ("mi" . "scala/inspect")
                    ("mn" . "scala/ensime")
                    ("mr" . "scala/refactor")
                    ("mt" . "scala/test")
                    ("ms" . "scala/repl")))
    (spacemacs/declare-prefix-for-mode 'scala-mode (car prefix) (cdr prefix))))

;; See https://github.com/syl20bnr/spacemacs/pull/3483
(defun lunaryorn-spacemacs-fixes/pre-init-ruby-tools ()
  (dolist (mode '(ruby-mode enh-ruby-mode))
    (spacemacs/declare-prefix-for-mode mode "mx" "ruby/text")))

(defun lunaryorn-spacemacs-fixes/pre-init-bundler ()
  (dolist (mode '(ruby-mode enh-ruby-mode))
    (spacemacs/declare-prefix-for-mode mode "mb" "ruby/bundle")))

(defun lunaryorn-spacemacs-fixes/pre-init-robe ()
  (dolist (mode '(ruby-mode enh-ruby-mode))
    (spacemacs/declare-prefix-for-mode mode "mg" "ruby/goto")
    (spacemacs/declare-prefix-for-mode mode "mh" "ruby/docs")
    (spacemacs/declare-prefix-for-mode mode "ms" "ruby/repl")))

(defun lunaryorn-spacemacs-fixes/pre-init-ruby-test-mode ()
  (dolist (mode '(ruby-mode enh-ruby-mode))
    (spacemacs/declare-prefix-for-mode mode "mt" "ruby/test")))

(defun lunaryorn-spacemacs-fixes/post-init-markdown-mode ()
  ;; https://github.com/syl20bnr/spacemacs/pull/3494
  (evil-leader/set-key-for-mode 'markdown-mode
    "mxC"  'markdown-insert-gfm-code-block))
