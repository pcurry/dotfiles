;;; packages.el --- typography Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 Sylvain Benner & Contributors
;;
;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq typography-packages
      '(
        typo
        tildify
        ))

(setq typography-excluded-packages '())

(defun typography/init-typo ()
  (use-package typo
    :defer t
    :init
    (progn
      (when typography-enable-typographic-editing
        (dolist (hook '(text-mode-hook org-mode-hook))
          (add-hook hook 'typo-mode)))

      (spacemacs|add-toggle typographic-substitutions
        :status typo-mode
        :on (typo-mode)
        :off (typo-mode -1)
        :documentation "Enable typographic substitutions"
        :evil-leader "tt")
      (spacemacs|diminish typo-mode " 𝔗" " ty"))))

(defun typography/init-tildify ()
  (use-package tildify
    :defer t
    :init
    (progn
      (when typography-enable-typographic-editing
        (add-hook 'text-mode-hook 'tildify-mode))

      (evil-leader/set-key
        "xt" 'tildify-region)

      ;; Use the symbolic non-breaking space for LaTeX
      (defun typography/tildify-latex-space ()
        "Set tildify space for LaTeX"
        (setq-local tildify-space-string "~"))
      (add-hook 'LaTeX-mode-hook 'typography/tildify-latex-space)

      (spacemacs|add-toggle tildify-space
        :status tildify-mode
        :on (tildify-mode)
        :off (tildify-mode -1)
        :documentation "Enable electric non-breaking space"
        :evil-leader "tT")
      (spacemacs|diminish tildify-mode " 𝔇" " td"))))
