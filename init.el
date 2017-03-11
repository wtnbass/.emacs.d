;; (package-initialize)
(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode t)

;; UI
(set-frame-font (font-spec :family "ricty diminished" :size 14))
(load-theme 'monokai t)
(nyan-mode t)

;; setting key bind
(global-unset-key (kbd "C-z"))
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

(require 'cl)
(prefer-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)

;; hide tool bar
(tool-bar-mode -1)

;; hide scroll bar
(set-scroll-bar-mode nil)

;; set mouse scroll step
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)

;; don't make backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

(setq transient-mark-mode t)
(setq ring-bell-function 'ignore)

;; show paren pair
(show-paren-mode t)

;; show line number
(global-linum-mode t)

;; show whitespaces
(global-whitespace-mode t)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; tab
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)))
