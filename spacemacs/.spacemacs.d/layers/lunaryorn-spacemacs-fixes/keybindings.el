;;; keybindings.el --- Keybindings fixing Spacemacs issues  -*- lexical-binding: t; -*-

;; Copyright (c) 2015 Sebastian Wiesner
;;
;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; Adds keybindings that are pending an upstream change in Spacemacs.

;;; Code:

;; Pending https://github.com/syl20bnr/spacemacs/pull/3851
(spacemacs||set-helm-key "ohn"  view-emacs-news)

;;; keybindings.el ends here
