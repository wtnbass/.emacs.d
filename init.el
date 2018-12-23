;;; -*- coding: utf-8 ; lexical-binding: t -*-
;; Package
;; ------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") )

(package-initialize)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

(load-file (expand-file-name "minit.el" user-emacs-directory))

;; Key Binding
;; ------------
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
;; ------------
(require 'cl)
(prefer-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)

;; image of startup buffer
(setq fancy-splash-image (expand-file-name "images/yotsuboshi_logo.png" user-emacs-directory))

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

(global-display-line-numbers-mode t)
(column-number-mode t)

;; Font
;;(set-frame-font (font-spec :family "Monaco" :size 12))
(set-frame-font (font-spec :family "Fira Code" :size 12))

;; Copy-Paste from: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-composition-char-table
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

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


;; Completion
;; ------------
(use-package ido-vertical-mode)
(ido-mode t)
(ido-everywhere t)
(ido-vertical-mode t)
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)
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
;; ------------
(use-package neotree
  :bind (("<f8>" . neotree-toggle))
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
;; ------------
(use-package magit
  :bind (("C-c C-g" . magit-status)))
(use-package git-gutter
  :config
  (global-git-gutter-mode t))


;; Terminal
;; ------------
(use-package shell-pop
  :config
  (custom-set-variables
   '(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
   '(shell-pop-term-shell "/bin/bash")
   '(shell-pop-universal-key "C-`"))) ;; Control + Shift + @


;; Text
;; ------------
(use-package expand-region
  :bind (("M-@" . er/expand-region)
         ("C-M-@" . er/contract-region)))

(use-package multiple-cursors
  :bind(("M-S-<up>" . 'mc/mark-previous-like-this)
        ("M-S-<down>" . 'mc/mark-next-like-this)
        ("M-P" . 'mc/mark-previous-like-this)
        ("M-N" . 'mc/mark-next-like-this)
        ("M-L" . 'mc/mark-all-like-this)
        ("C-M-P" . 'mc/skip-to-previous-like-this)
        ("C-M-N" . 'mc/skip-to-next-like-this)
        ("M-*" . 'mc/mark-all-like-this)))

;; Org-mode
;; ------------
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
;; ------------
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

;; Language Server Protocol
(use-package lsp-mode
  :init
  (require 'lsp-clients)
  (when (equal system-type 'darwin)
    (setq lsp-clangd-executable "/usr/local/opt/llvm/bin/clangd"))
  :config
  ;; C/C++
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'objc-mode-hook 'lsp)
  ;; html
  (add-hook 'html-mode-hook 'lsp)
  ;; CSS
  (add-hook 'css-mode-hook 'lsp)
  (add-hook 'sass-mode-hook 'lsp)
  (add-hook 'scss-mode-hook 'lsp)
  ;; Javascript/Typescript
  (add-hook 'js-mode-hook 'lsp)
  (add-hook 'typescript-mode-hook 'lsp)
  (add-hook 'vue-mode-hook 'lsp)
  ;; Python
  (add-hook 'python-mode-hook 'lsp)
  ;; Rust
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
  (add-hook 'rust-mode-hook 'lsp)
  ;; Go
  (add-hook 'go-mode-hook 'lsp))

(use-package web-mode
  :mode "\\.html\\'"
  :mode ".erb\\'"
  :mode "\\.[agj]sp\\'"
  :mode "\\.tera\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-attr-value-indent-offset 2))

;; Typescript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :mode "\\.tsx\\'"
  :config
  (setq typescript-indent-level 2)
  (setq emmet-self-closing-tag-style " /")
  (setq company-tooltip-align-annotations t)
  (add-hook 'typescript-mode 'flycheck-mode))

;; Vue.js
(use-package vue-mode)

;; Emmet
(use-package emmet-mode
  :config
  (setq emmet-self-closing-tag-style " /")
  (setq emmet-expand-jsx-className? t)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'typescript-mode-hook 'emmet-mode)
  (add-hook 'vue-mode-hook 'emmet-mode))

;; Prettier
(use-package prettier-js
  :config
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  (add-hook 'vue-mode-hook 'prettier-js-mode))

;; Go
(use-package go-mode
  :config
  (add-hook 'go-mode-hook
            '(lambda()
               (add-hook 'before-save-hook' 'gofmt-before-save))))

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook 'flycheck-mode))

;; Python
(use-package python-mode
  :mode "\\.py\\'"
  :config
  (setq py-python-command "python3")
  (setq python-indent 4)
  (add-hook 'python-mode-hook 'flycheck-mode))

;; Elm
(use-package elm-mode
  :mode "\\.elm\\'"
  :config
  (setq elm-format-on-save t)
  (setq elm-sort-imports-on-save t)
  (add-hook 'elm-mode-hook 'company-mode)
  (add-to-list 'company-backends 'company-elm))

;; Haskell
(use-package haskell-mode
  :config
  (use-package intero)
  (add-hook 'haskell-mode 'intero-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default))
 '(package-selected-packages
   '(intero haskell-mode elm-mode python-mode rust-mode go-mode prettier-js emmet-mode vue-mode typescript-mode web-mode lsp-mode toml-mode yaml-mode json-mode markdown-mode multiple-cursors expand-region shell-pop git-gutter magit undo-tree neotree swiper company smex ido-vertical-mode window-number material-theme exec-path-from-shell use-package))
 '(shell-pop-shell-type
   '("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell))))
 '(shell-pop-term-shell "/bin/bash")
 '(shell-pop-universal-key "C-`"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
