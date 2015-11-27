;;; packages.el --- lunaryorn-latex Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2015 Sebastian Wiesner <swiesner@lunaryorn.com>
;;
;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq lunaryorn-latex-packages
      '(
        auctex
        typo
        ))

(setq lunaryorn-latex-excluded-packages '())

(defun lunaryorn-latex/post-init-auctex ()
  (use-package tex
    :defer t
    :config
    (progn
      (setq
       ;; Automatically insert braces after sub- and superscripts
       TeX-electric-sub-and-superscript t
       ;; Don't insert magic quotes immediately
       TeX-quote-after-quote t
       ;; Don't confirm cleaning
       TeX-clean-confirm nil)
      ;; Use a modern TeX engine
      (setq-default TeX-engine 'luatex)))

  (use-package tex-buf
    :defer t
    ;; Don't ask for confirmation when saving
    :config (setq TeX-save-query nil))

  (use-package tex-style
    :defer t
    :config
    ;; Enable support for csquotes
    (setq LaTeX-csquotes-close-quote "}"
          LaTeX-csquotes-open-quote "\\enquote{"))

  (use-package latex
    :defer t
    :config
    (setq TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
                              (,(rx (0+ space) "\\subsection*{") 3)
                              (,(rx (0+ space) "\\subsubsection*{") 4)
                              (,(rx (0+ space) "\\minisec{") 5))
          ;; No language-specific hyphens please
          LaTeX-babel-hyphen nil)))

(defun lunaryorn-latex/post-init-typo ()
  ;; Keep typo mode enabled in LaTeX
  (remove-hook 'LaTeX-mode-hook 'spacemacs//disable-typo-mode))
