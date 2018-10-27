;; Package
;; =====
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") )
(package-initialize)

(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)
;; Key Binding
;; =====
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key global-map (kbd "C-x ?") 'help-command)

;; macOS
(setq mac-command-modifier 'super)
(setq mac-right-command-modifier ' super)
(setq mac-option-modifier 'meta)

;; Fix for JIS keyboard
(when (eq system-type 'darwin)
  (define-key global-map [?¥] [?\\]))

;; macOS like
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-S") 'write-file)
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-w") 'delete-window)
(global-set-key (kbd "s-z") 'undo)


;; Basic setting
;; =====
(require 'cl)
(prefer-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)

;; image of startup buffer
(setq fancy-splash-image (expand-file-name "~/.emacs.d/yotsuboshi_logo.png"))
(setq inhibit-startup-message t)
;; Do M-x display-about-screen if open startup buffer

;; exec path
(use-package exec-path-from-shell
  :ensure t)
(exec-path-from-shell-initialize)

(tool-bar-mode -1)
(menu-bar-mode -1)
(set-scroll-bar-mode nil)

;; set mouse scroll step
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)

;; Don't make backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

(setq large-file-warning-threshold 10000000)
(global-auto-revert-mode t)

;; TAB
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

;; down/up case region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Show whitespaces
(global-whitespace-mode t)
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq whitespace-line-column 300)

;; show current region
(setq transient-mark-mode t)

;; show paren
(show-paren-mode t)
(electric-pair-mode t)

(global-visual-line-mode t)

;; UI
(use-package material-theme)
(load-theme 'material)
;;(load-theme 'tsdh-light)

(global-display-line-numbers-mode t)
(column-number-mode t)

;; Font
(set-frame-font (font-spec :family "Monaco" :size 12))
(setq-default line-spacing 2)

(defun set-frame-background-color (frame)
  (select-frame frame)
  ;; terminal
  (unless (display-graphic-p)
    (set-background-color "black")))
(add-hook 'after-make-frame-functions 'set-frame-background-color)
(set-frame-background-color (selected-frame))

;; window
(unless (eq window-system nil)
  (set-frame-parameter (selected-frame) 'alpha 95)
  (set-frame-parameter nil 'fullscreen 'maximized))

(use-package all-the-icons)
;; MUST DO M-x all-the-icons-install-fonts after

;; Hide minor modes from modeline
(use-package rich-minority
  :config
  (rich-minority-mode 1)
  (setf rm-blacklist ""))

(use-package window-number
  :bind(("C-x o" . 'window-number-switch))
  :config
  (window-number-mode t))

;; Completion
;; =====
(use-package ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode t)
(setq ido-enable-flex-matching t)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x". execute-extended-command)))

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

(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper)
  (global-set-key "\C-r" 'swiper)
  (global-set-key (kbd "s-f") 'swiper))


;; File tree
;; =====
(use-package neotree
  :bind (("s-B" . neotree-toggle))
  :config
  (setq neo-window-width 32
        neo-create-file-auto-open t
        neo-banner-message nil
        neo-show-updir-line t
        neo-window-fixed-size nil
        neo-vc-integration nil
        neo-mode-line-type 'neotree
        neo-smart-open t
        neo-show-hidden-files t
        neo-mode-line-type 'none
        neo-auto-indent-point t)
  (setq neo-theme (if (display-graphic-p) 'nerd 'arrow))
  (setq neo-hidden-regexp-list '("venv" "\\.pyc$" "~$" "\\.git" "__pycache__" ".DS_Store")))

(use-package undo-tree
  :config
  (global-undo-tree-mode t))


;; Git
;; =====
(use-package magit
  :bind (("C-c C-g" . magit-status)))
(use-package git-gutter
  :config
  (global-git-gutter-mode t))

;; Terminal
;; =====
(use-package shell-pop
  :config
  (custom-set-variables
   '(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
   '(shell-pop-term-shell "/bin/bash")
   '(shell-pop-universal-key "C-`"))) ;; Control + Shift + @


;; Text
;; =====
(use-package expand-region
  :bind (("M-@" . er/expand-region)
         ("C-M-@" . er/contract-region)))

(use-package multiple-cursors
  :bind(("M-S-<up>" . 'mc/mark-previous-like-this)
        ("M-S-<down>" . 'mc/mark-next-like-this)
        ("M-P" . 'mc/mark-previous-like-this)
        ("M-N" . 'mc/mark-next-like-this)
        ("M-L" . 'mc/mark-all-like-this)
        ("M-*" . 'mc/mark-all-like-this)))

;; Org-mode
;; =====
;; Some basic Org defaults
(use-package org
  :config
  (setq org-startup-indented t)         ;; Visually indent sections. This looks better for smaller files.
  (setq org-src-tab-acts-natively t)    ;; Tab in source blocks should act like in major mode
  (setq org-src-preserve-indentation t)
  (setq org-log-into-drawer t)          ;; State changes for todos and also notes should go into a Logbook drawer
  (setq org-src-fontify-natively t)     ;; Code highlighting in code blocks
  (setq org-log-done 'time)             ;; Add closed date when todo goes to DONE state
  (setq org-support-shift-select t))    ;; Allow shift selection with arrows.

;; Store all my org files in ~/org.
(setq org-directory "~/org")

;; And all of those files should be in included agenda.
(setq org-agenda-files '("~/org"))


;; Program
;; =====
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package json-mode
  :mode "\\.json\\'"
  :mode "\\.babelrc"
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))

(use-package yaml-mode)

(use-package toml-mode)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :mode "\\.tsx\\'"
  :config
  (setq typescript-indent-level 2)
  (setq emmet-self-closing-tag-style " /")
  (setq company-tooltip-align-annotations t))

(use-package tide
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (company-mode +1)
    (local-set-key [f1] 'tide-documentation-at-point))
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package web-mode
  :mode "\\.html\\'"
  :mode ".erb\\'"
  :mode "\\.[agj]sp\\'"
  :mode "\\.tera\\'"
  :mode "\\.css\\'"
  :mode "\\.jsx?\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-attr-value-indent-offset 2)
  (setq emmet-self-closing-tag-style " /")
  (setq emmet-expand-jsx-className? t)
  (setq emmet-self-closing-tag-style " /")
  (use-package company-tern)
  (add-to-list 'company-backends '(company-tern :with company-dabbrev-code))
  (add-hook 'web-mode-hook 'tern-mode))

(use-package scss-mode
  :mode "\\.scss\\'"
  :config
  (add-hook 'scss-mode-hook 'prettier-js-mode))

;; Emmet
(use-package emmet-mode
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'typescript-mode-hook 'emmet-mode))

;; Formatter for Web
(use-package prettier-js
  :config
  (defun enable-minor-mode (my-pair)
    "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
    (if (buffer-file-name)
        (if (string-match (car my-pair) buffer-file-name)
            (funcall (cdr my-pair)))))
  (add-hook 'web-mode-hook #'(lambda ()
                               (enable-minor-mode
                                '("\\.jsx?\\'" . prettier-js-mode))))
  (add-hook 'typescript-mode-hook 'prettier-js-mode))

;; Go
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

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (use-package flycheck-rust)
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook 'lsp-rust-enable)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package lsp-mode
  :config
  (use-package lsp-rust)
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls")))

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(use-package python-mode
  :mode "\\.py\\'"
  :config
  (setq py-python-command "python3")
  (setq python-indent 4)
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "")
  (setq python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
  (setq python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
  (setq python-shell-completion-setup-code "from IPython.core.completerlib import module_completion")
  (setq python-shell-completion-module-string-code  "';'.join(module_completion('''%s'''))\n")
  (setq python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'my/python-mode-hook))

(use-package elm-mode
  :mode "\\.elm\\'"
  :config
  (setq elm-format-on-save t)
  (setq elm-sort-imports-on-save t)
  (add-hook 'elm-mode-hook 'company-mode)
  (add-to-list 'company-backends 'company-elm))

(use-package haskell-mode
  :config
  (use-package intero)
  (add-hook 'haskell-mode 'intero-mode))
