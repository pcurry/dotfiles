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
      (ensime-expand-region :location local)
      flycheck
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

(defun lunaryorn-scala/init-ensime-expand-region ()
  ;; Pending upstream, see https://github.com/ensime/ensime-emacs/pull/263
  (spacemacs|use-package-add-hook ensime
    :post-config (require 'ensime-expand-region)))

(defun lunaryorn-scala/post-init-flycheck ()
  (add-hook 'flycheck-locate-config-file-functions
            #'lunaryorn-scala/find-config-file-in-sbt-project))
