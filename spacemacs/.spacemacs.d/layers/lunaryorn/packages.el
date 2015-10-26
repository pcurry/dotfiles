;;; packages.el --- lunaryorn Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 Sebastian Wiesner <swiesner@lunaryorn.com>
;;
;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(eval-when-compile
  (require 'use-package))
(require 'subr-x)

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq lunaryorn-packages
      '(exec-path-from-shell
        focus-autosave-mode
        spaceline
        ;; Editing
        whitespace
        whitespace-cleanup-mode
        hungry-delete
        beacon
        writeroom-mode
        company
        company-emoji
        ;; Tools
        ispell
        flycheck
        projectile
        ;; Applications
        paradox
        ))

;; List of packages to exclude.
(setq lunaryorn-excluded-packages '())

(defun lunaryorn/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :if (and (eq system-type 'darwin) (display-graphic-p))
    :config
    (progn
      (when (string-match-p "/zsh$" (getenv "SHELL"))
        ;; Use a non-interactive login shell.  A login shell, because my
        ;; environment variables are mostly set in `.zprofile'.
        (setq exec-path-from-shell-arguments '("-l")))

      (dolist (var '("EMAIL" "PYTHONPATH" "INFOPATH" "JAVA_OPTS"))
        (add-to-list 'exec-path-from-shell-variables var))

      (exec-path-from-shell-initialize)

      (setq user-mail-address (getenv "EMAIL"))

      ;; Re-initialize the `Info-directory-list' from $INFOPATH.  Since package.el
      ;; already initializes info, we need to explicitly add the $INFOPATH
      ;; directories to `Info-directory-list'.  We reverse the list of info paths
      ;; to prepend them in proper order subsequently
      (with-eval-after-load 'info
        (dolist (dir (nreverse (parse-colon-path (getenv "INFOPATH"))))
          (when dir
            (add-to-list 'Info-directory-list dir)))))))

(defun lunaryorn/init-focus-autosave-mode ()
  (use-package focus-autosave-mode
    :init (focus-autosave-mode)
    :config (spacemacs|hide-lighter focus-autosave-mode)))

(defun lunaryorn/post-init-spaceline ()
  ;; Disabled, broken again.  Find some time to fix it...
  (spaceline-define-segment lunaryorn-branding
    "My personal branding."
    "🐷"
    :skip-alternate t
    :enabled nil)
  ;; (add-to-list 'spaceline-left 'lunaryorn-branding)
  )

(defun lunaryprn/post-init-company ()
  ;; Auto-complete less aggressively
  (setq company-idle-delay 0.5))

(defun lunaryorn/post-init-company-emoji ()
  ;; Re-enable unicode emoji.  It's 2015, dammit
  (setq company-emoji-insert-unicode t))

;; Editing
(defun lunaryorn-whitespace-mode-local ()
  "Enable `whitespace-mode' after local variables where set up."
  (add-hook 'hack-local-variables-hook #'whitespace-mode nil 'local))

(defun lunaryorn/pre-init-whitespace ()
  ;; Cleanup all whitespace
  (evil-leader/set-key "xdw" #'whitespace-cleanup)
  ;; Use less aggressive whitespace highlighting, and disable Spacemacs own
  ;; whitespace highlighting
  (setq spacemacs-show-trailing-whitespace nil
        whitespace-style '(face indentation space-after-tab space-before-tab
                                tab-mark empty trailing lines-tail)
        whitespace-line-column nil))

(defun lunaryorn/post-init-whitespace ()
  ;; Enable whitespace mode after local variables were setup because whitespace
  ;; mode doesn't handle local variables well :(
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'lunaryorn-whitespace-mode-local)))

(defun lunaryorn/init-whitespace-cleanup-mode ()
  (use-package whitespace-cleanup-mode
    :init
    (progn
      (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
        (add-hook hook #'whitespace-cleanup-mode))

      (spacemacs|add-toggle whitespace-cleanup
        :status whitespace-cleanup-mode
        :on (whitespace-cleanup-mode)
        :off (whitespace-cleanup-mode -1)
        :documentation "Cleanup whitespace."
        :evil-leader "tW"))
    :config (spacemacs|diminish whitespace-cleanup-mode " ⓧ" " x")))

(defun lunaryorn/post-init-hungry-delete ()
  (global-hungry-delete-mode))

(defun lunaryorn/init-beacon ()
  (use-package beacon
    :init
    (progn
      (spacemacs|add-toggle beacon
        :status beacon-mode
        :on (beacon-mode)
        :off (beacon-mode -1)
        :documentation "Enable point highlighting after scrolling"
        :evil-leader "otb")

      (spacemacs|hide-lighter beacon-mode)
      (spacemacs/toggle-beacon-on))))

(defun lunaryorn/init-writeroom-mode ()
  (use-package writeroom-mode
    :init (spacemacs|add-toggle writeroom
            :status writeroom-mode
            :on (writeroom-mode)
            :off (writeroom-mode -1)
            :documentation "Enable distraction-free editing"
            :evil-leader "otw")))

(defun lunaryorn/init-ispell ()
  (use-package ispell
    :defer t
    :config
    (progn
      (setq ispell-program-name (executable-find "aspell")
            ispell-dictionary "en_GB"     ; Default dictionnary
            ispell-silently-savep t       ; Don't ask when saving the private dict
            ;; Increase the height of the choices window to take our header line
            ;; into account.
            ispell-choices-win-default-height 5)

      (unless ispell-program-name
        (warn "No spell checker available, install ASpell.")))))

(defun lunaryorn/post-init-flycheck ()
  ;; Enable Flycheck _everywhere
  (global-flycheck-mode))

(defun lunaryorn/post-init-projectile ()
  ;; Disable caching, my systems are fast enough
  (setq projectile-enable-caching nil)
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects))

(defun lunaryorn/post-init-paradox ()
  ;; Make the spinner fancy and don't star packages automatically
  (setq paradox-spinner-type 'moon
        paradox-automatically-star nil))
