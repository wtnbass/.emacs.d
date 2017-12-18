;; (package-initialize)
(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode t)

;; UI
(load-theme 'monokai t)
(set-frame-font (font-spec :family "Monaco" :size 12))
(defun set-frame-background-color (frame)
  (select-frame frame)
  ;; terminal
  (unless (display-graphic-p)
    (set-background-color "black")))
(add-hook 'after-make-frame-functions 'set-frame-background-color)
(set-frame-background-color (selected-frame))

;; setting key bind
(define-key global-map [?¥] [?\\])
(global-unset-key (kbd "C-z"))
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

(require 'cl)
(prefer-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-message t)

;; exec path
(exec-path-from-shell-initialize)

;; hide tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)

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

;; show line and column
(column-number-mode t)

;; show whitespaces
(global-whitespace-mode t)
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq whitespace-line-column 300)

;; tab
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

;; smart-newline-mode
(smart-newline-mode t)

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files))
  :config
  (helm-mode t))

(use-package company
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (abbrev-mode t)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (setq company-dabbrev-downcase nil)
  (setq company-tooltip-align-annotations t))
(global-company-mode t)

(use-package expand-region
  :bind (("M-@" . er/expand-region)
         ("C-M-@" . er/contract-region)))

(use-package smartrep
  :config
  ;; multiple-curcors key bind
  (smartrep-define-key global-map "C-z"
    '(("p" . 'mc/mark-previous-like-this)
      ("n" . 'mc/mark-next-like-this)
      ("u" . mc/unmark-next-like-this)
      ("U" . mc/unmark-previous-like-this)
      ("s" . mc/skip-to-next-like-this)
      ("S" . mc/skip-to-previous-like-this)
      ("*" . 'mc/mark-all-like-this))))

(use-package json-mode
  :mode "\\.json\\'"
  :mode "\\.babelrc")

(use-package tide
  :init
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (company-mode +1)
    (local-set-key [f1] 'tide-documentation-at-point))

  (setq company-tooltip-align-annotations t))

(use-package web-mode
  :mode "\\.html"
  :mode "\\.tera\\'"
  :mode "\\.css\\'"
  :mode "\\.jsx?\\'"
  :mode "\\.tsx\\'"
  :config
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq emmet-self-closing-tag-style " /")
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook (lambda ()
                             (if (equal web-mode-content-type "javascript")
                                 (web-mode-set-content-type "jsx"))))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode)))))

(use-package go-mode
  :config
  (defun my/go-hook()
    (add-hook 'before-save-hook' 'gofmt-before-save)
    (local-set-key (kbd "M-.") 'godef-jump)
    (set (make-local-variable 'company-backends) '(company-go))
    (setq company-go-insert-arguments nil))
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook 'company-mode)
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook 'my/go-hook))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
