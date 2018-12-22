;;; -*- coding: utf-8 ; lexical-binding: t -*-
(setq inhibit-startup-message t)

;; backspace
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key global-map (kbd "C-x ?") 'help-command)

;; Don't make backup files
(setq make-backup-files nil)
(setq auto-save-default nil)
