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
      thrift
      sbt-mode
      flycheck
      ))

(setq lunaryorn-scala-excluded-packages '())

(defun lunaryorn-scala/init-thrift ()
  (use-package thrift
    :defer t
    :init (put 'thrift-indent-level 'safe-local-variable #'integerp)
    ;; Fake inheritance from prog mode
    :config (add-hook 'thrift-mode-hook (lambda () (run-hooks 'prog-mode-hook)))))

(defun lunaryorn-scala/post-init-sbt-mode ()
  (with-eval-after-load 'scala-mode2
    (evil-leader/set-key-for-mode 'scala-mode
      "moi" #'lunaryorn-scala/pop-to-sbt-frame)))

(defun lunaryorn-scala/post-init-flycheck ()
  (add-hook 'flycheck-locate-config-file-functions
            #'lunaryorn-scala/find-config-file-in-sbt-project))
