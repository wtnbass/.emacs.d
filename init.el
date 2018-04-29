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
(setq fancy-splash-image (expand-file-name "~/.emacs.d/yotsuboshi_logo.png"))

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

(tool-bar-mode -1)
(menu-bar-mode -1)
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

(show-paren-mode t)
(electric-pair-mode t)

(column-number-mode t)

;; show whitespaces
(global-whitespace-mode t)
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq whitespace-line-column 300)

;; tab
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

;; down/up case region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

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

(use-package multiple-cursors
  :config
  (use-package smartrep
    :config
    (smartrep-define-key global-map "C-z"
      '(("p" . 'mc/mark-previous-like-this)
        ("n" . 'mc/mark-next-like-this)
        ("u" . mc/unmark-next-like-this)
        ("U" . mc/unmark-previous-like-this)
        ("s" . mc/skip-to-next-like-this)
        ("S" . mc/skip-to-previous-like-this)
        ("*" . 'mc/mark-all-like-this)))))

(use-package magit
  :bind (("C-c C-g" . magit-status)))

(use-package json-mode
  :mode "\\.json\\'"
  :mode "\\.babelrc"
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (company-mode +1)
  (local-set-key [f1] 'tide-documentation-at-point))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :mode "\\.tsx\\'"
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'typescript-mode-hook 'emmet-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  (setq emmet-self-closing-tag-style " /")
  (setq company-tooltip-align-annotations t))

(defun enable-miner-mode (my-pair)
  "Enabel minor mode if filename match the regexp. MY-PAIR is a cons cell"
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(defun setup-prettier-js-mode-for-web ()
  (enable-miner-mode '("\\.jsx?\\'" . prettier-js-mode))
  (enable-miner-mode '("\\.css\\'" . prettier-js-mode)))

(use-package web-mode
  :mode "\\.html\\'"
  :mode "\\.tera\\'"
  :mode "\\.css\\'"
  :mode "\\.jsx?\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq emmet-self-closing-tag-style " /")
  (setq emmet-expand-jsx-className? t)
  (setq emmet-self-closing-tag-style " /")
  (add-to-list 'company-backends '(company-tern :with company-dabbrev-code))
  (add-hook 'web-mode-hook 'tern-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook #'setup-prettier-js-mode-for-web))

(use-package scss-mode
  :mode "\\.scss\\'"
  :config
  (add-hook 'scss-mode-hook 'prettier-js-mode))

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

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package python-mode
  :mode "\\.py\\'"
  :config
  (setq python-indent 4))

(use-package elm-mode
  :mode "\\.elm\\'"
  :config
  (setq elm-format-on-save t)
  (setq elm-sort-imports-on-save t)
  (add-hook 'elm-mode-hook 'company-mode)
  (add-to-list 'company-backends 'company-elm))
