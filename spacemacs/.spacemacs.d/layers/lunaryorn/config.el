;;; config.el --- Lunaryorn layer configuration      -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sebastian Wiesner

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>

;;; Code:

;; More refined font setup, providing math and emoji support.  Needs:
;;
;; - XITS Math (https://github.com/khaledhosny/xits-math) as fallback for math
;;
;; Currently this setup only works for OS X, as we rely on Apple's Emoji and
;; Symbol fonts.

;; Font setup
(defun lunaryorn-configure-fonts (frame)
  "Set up fonts for FRAME.

Set the default font, and configure various overrides for
symbols, emojis, greek letters, as well as fall backs for."
  ;; Additional fonts for special characters and fallbacks
  ;; Test range: 🐷 ❤ ⊄ ∫ 𝛼 α 🜚 Ⓚ

  (dolist (script '(symbol mathematical))
    (set-fontset-font t script (font-spec :family "XITS Math")
                      frame 'prepend))

  ;; Define a font set stack for symbols, greek and math characters
  (dolist (script '(symbol greek mathematical))
    (set-fontset-font t script (font-spec :family "Arial Unicode MS")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "Menlo")
                      frame 'prepend)
    (set-fontset-font t script (font-spec :family "DejaVu Sans Mono")
                      frame 'prepend))

  (when (eq system-type 'darwin)
    ;; Colored Emoji on OS X, prefer over everything else!
    (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji")
                      frame 'prepend))

  ;; Fallbacks for math and generic symbols
  (set-fontset-font t nil (font-spec :family "Apple Symbols")
                    frame 'append))

(when-let (frame (selected-frame))
  (lunaryorn-configure-fonts frame))
(add-hook 'after-make-frame-functions #'lunaryorn-configure-fonts)

;; Resize frames pixelwise
(setq frame-resize-pixelwise t)

;; More space between lines, makes text easier to read
(setq-default line-spacing 0.2)

;; Jump to bug references from code
(add-hook 'text-mode-hook #'bug-reference-mode)
(add-hook 'prog-mode-hook #'bug-reference-prog-mode)

;;; config.el ends here
