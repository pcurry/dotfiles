;;; packages.el --- lunaryorn-scala Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq lunaryorn-scala-packages
    '(
      sbt-mode
      (flycheck-auto-scalastyle :location local)
      ))

(setq lunaryorn-scala-excluded-packages '())

(defun lunaryorn-scala/post-init-sbt-mode ()
  (evil-leader/set-key-for-mode 'scala-mode
    "moi" #'lunaryorn-scala/pop-to-sbt-frame))

(defun lunaryorn-scala/init-flycheck-auto-scalastyle ()
  (use-package flycheck-auto-scalastyle
    :init (add-hook 'scala-mode-hook #'flycheck-auto-scalastyle-setup)
    :config (setq flycheck-auto-scalastyle-jar-dir
                  spacemacs-cache-directory)))
