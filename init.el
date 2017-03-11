(package-initialize)
(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(set-frame-font (font-spec :family "ricty diminished" :size 14))
(load-theme 'monokai t)
(nyan-mode t)

(global-unset-key (kbd "C-z"))
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

(require 'cl)
(prefer-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq transient-mark-mode t)
(setq ring-bell-function 'ignore)

(global-whitespace-mode t)
(add-hook 'before-save-hook 'whitespace-cleanup)
