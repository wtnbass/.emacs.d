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
(define-key global-map [?¥] [?\\])
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

;; auto close
(electric-pair-mode t)

;; show line number
(global-linum-mode t)

;; show whitespaces
(global-whitespace-mode t)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; tab
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)

;; anzu-mode
(global-anzu-mode t)

;; smart-newline-mode
(smart-newline-mode t)

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)))

(use-package magit
  :bind (("C-c g" . magit-status)))

(use-package company
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (setq company-dabbrev-downcase nil)
  (setq company-tooltip-align-annotations t))
(global-company-mode t)

(use-package emmet-mode
  :config
  (setq emmet-self-closing-tag-style " /"))

(use-package web-mode
  :mode "\\.html\\'"
  :mode "\\.css\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package js2-mode
  :mode ("\\.jsx?\\'" . js2-jsx-mode)
  :config
  (add-to-list 'company-backends '(company-tern :with company-dabbrev-code))
  (setq js2-include-browser-externs nil)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-strict-trailing-comma-warning nil)
  (setq js2-highlight-external-variables nil)
  (setq js2-include-jslint-globals nil)
  (setq js2-basic-offset 2)
  (setq js-switch-indent-offset 2)
  (setq emmet-expand-jsx-className? t)
  (add-hook 'js2-jsx-mode-hook 'emmet-mode))
