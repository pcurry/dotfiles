;;; packages.el --- lunaryorn Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 Sebastian Wiesner <swiesner@lunaryorn.com>
;;
;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq lunaryorn-packages
      '(exec-path-from-shell
        ;; Basic editing
        whitespace
        whitespace-cleanup-mode
        writeroom-mode
        typo
        ;; Tools
        flycheck
        projectile))

;; List of packages to exclude.
(setq lunaryorn-excluded-packages '())

(defun lunaryorn/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :ensure t
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
    :config
    (progn (spacemacs|diminish whitespace-cleanup-mode " ⓧ" " x"))))

(defun lunaryorn/init-writeroom-mode ()
  (use-package writeroom-mode
    :init (spacemacs|add-toggle writeroom
            :status writerooom-mode
            :on (writeroom-mode)
            :off (writeroom-mode -1)
            :documentation "Enable distraction-free editing"
            :evil-leader "otw")))

(defun lunaryorn/init-typo ()
  (use-package typo
    :init
    (progn
      (spacemacs|add-toggle typo
        :status typo-mode
        :on (typo-mode)
        :off (typo-mode -1)
        :documentation "Enable typographic editing"
        :evil-leader "tt")

      (dolist (hook '(markdown-mode-hook
                      rst-mode-hook))
        (add-hook hook 'typo-mode)))))

(defun lunaryorn/post-init-flycheck ()
  ;; Enable Flycheck _everywhere
  (global-flycheck-mode))

(defun lunaryorn/post-init-projectile ()
  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects))
