(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

; custom
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

; appearance basics
(add-to-list 'default-frame-alist '(font . "Inconsolata-15" ))
(set-face-attribute 'default t :font "Inconsolata-15" )
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(server-mode t)
(setq visible-bell t)
(blink-cursor-mode -1)
; maximize by default
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq show-paren-delay  0)
(show-paren-mode t)

; backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t)
(setq kept-old-versions 1000)
(setq vc-make-backup-files t)
(setq version-control t)

; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq tramp-default-method "ssh")

(setq bookmark-save-flag 1)

;; use (likely newer) sqlite3 shell from homebrew for sql-mode
;; if it exists
(if-let ((sqlite3-path "/usr/local/opt/sqlite3/bin/sqlite3")
	 ((file-exists-p sqlite3-path)))
    (setq sql-sqlite-program sqlite3-path))

; package init
(require 'package)

(setq package-archives '(;("org"       . "https://orgmode.org/elpa/")
                         ("gnu"       . "https://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)
; this is too slow to do every startup
; would be nice if there was a global use-package "before install" hook
; where this could be done once
; (package-refresh-contents)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

(use-package acme-theme
  :config
  (load-theme 'acme t))

(use-package diminish
  :demand t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(use-package ivy
  :diminish
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package ivy-xref
  :init
  (setq xref-show-definitions-function #'ivy-xref-show-defs)
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package lsp-ivy)

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode))

(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))

(use-package go-mode)

(setq-default indent-tabs-mode nil)

(use-package elm-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(use-package lsp-mode
  :config (lsp-register-custom-settings
	   '(("gopls.staticcheck" t t)))
  (setq lsp-disabled-clients '(semgrep-ls))
  ;(setq lsp-semantic-tokens-enable t)
  ;(setq lsp-semantic-tokens-honor-refresh-requests t)
  (setq lsp-go-gopls-server-args '("-debug=localhost:31337"))
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.minio\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.log\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.elastic\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.devdb\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.bin\\'")
  :hook
  (go-mode . lsp)
  (elm-mode . lsp)
  (rust-mode . lsp)
  (tsx-ts-mode . lsp)
  (typescript-ts-mode . lsp)
  (html-mode . lsp)
  (elixir-mode . lsp)
  :commands lsp)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(add-hook 'prog-mode-hook 'copilot-mode)

(defun lsp-rust-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'rust-mode-hook #'lsp-rust-install-save-hooks)

(use-package company
  :diminish
  :hook (go-mode . company-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :hook
  (go-mode . yas-minor-mode)
  (rust-mode . yas-minor-mode))

(use-package color-identifiers-mode
  :diminish
  :hook
  (go-mode . color-identifiers-mode))

(use-package find-file-in-project
  :config
  (add-to-list 'ffip-project-file '"go.mod")
  (add-to-list 'ffip-project-file '"Rakefile")
  (add-to-list 'ffip-project-file '"Gemfile")
  (add-to-list 'ffip-prune-patterns '"*/.bin")
  (add-to-list 'ffip-prune-patterns '"*/.devdb")
  (add-to-list 'ffip-prune-patterns '"*/.elastic")
  (add-to-list 'ffip-prune-patterns '"*/.log")
  (add-to-list 'ffip-prune-patterns '"*/.minio")
  :bind
  ("s-t" . 'find-file-in-project))

(use-package enh-ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")

(use-package rust-mode)

(use-package yaml-mode)

(use-package protobuf-mode)

(use-package flycheck)

(use-package caddyfile-mode)

(use-package ag)

(use-package git-link
  :config
  (setq git-link-use-commit t))

(use-package fish-mode)

(use-package lsp-tailwindcss)

(use-package editorconfig)

(use-package elixir-mode)

(add-to-list 'load-path (expand-file-name "copilot/copilot.el" user-emacs-directory))
(require 'copilot)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

; https://github.com/emacs-lsp/lsp-mode/issues/3516 and https://github.com/emacs-lsp/lsp-mode/issues/4112
(setq backup-by-copying t)

(use-package gptel
  :config
  (defun get-anthropic-api-key ()
    (require 'auth-source)
    (let ((auth-info (auth-source-search :host "api.anthropic.com"
                                         :user "apikey"
                                         :require '(:secret))))
      (if auth-info
          (funcall (plist-get (car auth-info) :secret))
        (error "Could not find Anthropic API key in authinfo"))))

  (gptel-make-anthropic "Claude"
    :stream t
    :key #'get-anthropic-api-key))
