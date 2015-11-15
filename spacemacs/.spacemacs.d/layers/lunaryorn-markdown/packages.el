;;; packages.el --- lunaryorn-markdown Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; license: gplv3

(setq lunaryorn-markdown-packages
      '(
        markdown-mode
        ))

(setq lunaryorn-markdown-excluded-packages '())

(defun lunaryorn-markdown/post-init-markdown-mode ()
  (evil-leader/set-key-for-mode 'markdown-mode
    "moh" 'lunaryorn-markdown/post-header
    "mop" 'lunaryorn-markdown/publish-jekyll-draft)

  (let* ((layer-dir (configuration-layer/get-layer-local-dir 'lunaryorn-markdown))
         (stylesheet (expand-file-name "pandoc.css" layer-dir)))
    (setq markdown-command
          (mapconcat #'shell-quote-argument
                     `("pandoc" "--toc" "--section-divs"
                       "--css" ,(concat "file://" stylesheet)
                       "--standalone" "-f" "markdown" "-t" "html5")
                     " "))))
