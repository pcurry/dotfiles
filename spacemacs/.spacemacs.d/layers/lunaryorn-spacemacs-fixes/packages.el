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
        ))

(setq lunaryorn-spacemacs-fixes-excluded-packages '())

(defun lunaryorn-spacemacs-fixes/init-company-flx ()
  ;; See https://github.com/syl20bnr/spacemacs/pull/3468
  (use-package company-flx
    :defer t)
  (spacemacs|use-package-add-hook company
    :pre-config
    (company-flx-mode)))
