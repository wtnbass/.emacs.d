;;; -*- coding: utf-8 ; lexical-binding: t -*-
;; Package
;; ------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") )
(package-initialize)

(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

;; Key Binding
;; ------------
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key global-map (kbd "C-x ?") 'help-command)

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

(use-package hydra :ensure t)

;; Basic setting
;; ------------
(prefer-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)

;; image of startup buffer
(setq inhibit-startup-message t)
(setq fancy-splash-image (expand-file-name "images/yotsuboshi_logo.png" user-emacs-directory))

;; Don't make backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; exec path
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(tool-bar-mode -1)
(menu-bar-mode -1)

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
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai))

(global-display-line-numbers-mode t)
(column-number-mode t)
(hl-line-mode t)

;; Font
;;(set-frame-font (font-spec :family "Monaco" :size 12))
(set-frame-font (font-spec :family "Consolas" :size 12))

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
  (set-frame-parameter nil 'fullscreen 'maximized)
  (setq alpha 95)
  (defhydra frame-alpha-hydra (:hint nil)
    "
[_p_] plus 5    [_n_] minus 5
"
    ("p" plus-frame-alpha)
    ("n" minus-frame-alpha))
  (global-set-key (kbd "C-c q") 'frame-alpha-hydra/body)
  (defun plus-frame-alpha ()
    (interactive)
    (setq alpha (+ alpha 5))
    (set-frame-parameter (selected-frame) 'alpha alpha))
  (defun minus-frame-alpha ()
    (interactive)
    (setq alpha (- alpha 5))
    (set-frame-parameter (selected-frame) 'alpha alpha))
  (set-frame-parameter (selected-frame) 'alpha alpha))


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
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package company
  :config
  (global-company-mode t)
  (abbrev-mode t)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (setq company-dabbrev-downcase nil)
  (setq company-tooltip-align-annotations t))

(use-package swiper
  :config
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-r") 'swiper)
  (global-set-key (kbd "s-f") 'swiper))

(use-package avy
  :config
  (global-set-key (kbd "M-SPC") 'avy-goto-char-timer))

;; File tree
;; ------------
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (treemacs--find-python3) 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (treemacs--find-python3))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package undo-tree
  :config
  (global-undo-tree-mode t))

;; Git
;; ------------
(use-package magit
  :config
  (global-set-key (kbd "C-c C-g") 'magit-status))

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
  :config
  (global-set-key (kbd "M-@") 'er/expand-region)
  (global-set-key (kbd "C-M-@") 'er/contract-region))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-c m") 'multiple-cursors-hydra/body)
  (defhydra multiple-cursors-hydra (:hint nil)
    "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil)))

;; Text Modes
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


(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;; Program
;; ------------
(use-package yasnippet)
(use-package yasnippet-snippets)
(setq yas-snippet-dirs '((yasnippet-snippets-dir)))

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
  :hook ((web-mode . lsp)
         (html-mode . lsp)
         (css-mode . lsp)
         (sass-mode . lsp)
         (scss-mode . lsp)
         (js-mode . lsp)
         (typescript-mode . lsp)
         (vue-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (objc-mode . lsp)
         (go-mode . lsp)
         (rust-mode . lsp)
         (python-mode . lsp))
  :commands lsp
  :config
  (setq lsp-auto-guess-root t)
  (when (equal system-type 'darwin)
    (setq lsp-clangd-executable "/usr/local/opt/llvm/bin/clangd")))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; Emmet
(use-package emmet-mode
  :hook ((html-mode . emmet-mode)
         (css-mode . emmet-mode)
         (js-mode . emmet-mode)
         (web-mode . emmet-mode)
         (typescript-mode . emmet-mode)
         (vue-mode . emmet-mode))
  :config
  (setq emmet-self-closing-tag-style " /")
  (setq emmet-expand-jsx-className? t))

;; Prettier
(use-package prettier-js
  :hook ((html-mode . prettier-js-mode)
         (css-mode . prettier-js-mode)
         (js-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (vue-mode . prettier-js-mode)
         (markdown-mode . prettier-js-mode)))

(use-package web-mode
  :mode "\\.html\\'"
  :mode "\\.erb\\'"
  :mode "\\.[agj]sp\\'"
  :mode "\\.tera\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-attr-value-indent-offset 2))

;; css
(use-package sass-mode)

;; JavaScript
(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))

;; TypeScript
(use-package typescript-mode
  :mode "\\.ts[x]?\\'"
  :config
  (setq typescript-indent-level 2)
  (setq company-tooltip-align-annotations t))

;; Vue.js
(use-package vue-mode)

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
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls")))

;; Python
(use-package python-mode
  :mode "\\.py\\'"
  :config
  (setq py-python-command "python3")
  (setq python-indent 4))

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
