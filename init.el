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
(server-mode)
(setq visible-bell t)
(blink-cursor-mode -1)
; maximize by default
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq show-paren-delay  0)
(show-paren-mode)

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
(defalias 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; package init
(require 'package)

(setq package-archives '(;("org"       . "https://orgmode.org/elpa/")
                         ;("gnu"       . "https://elpa.gnu.org/packages/")
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
  (ivy-mode))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package ivy-xref
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package markdown-mode)

(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package go-mode)

(use-package eglot
  :config
  (add-to-list 'eglot-stay-out-of 'completion-styles) ; uses gopls completion ordering https://github.com/joaotavora/eglot/issues/576
  (add-to-list 'eglot-server-programs '(enh-ruby-mode . ("solargraph" "socket" "--port" :autoport)))
  :hook
  (go-mode . eglot-ensure)
  (go-mode . go-install-save-hooks)
  (enh-ruby-mode . eglot-ensure))

;; from https://github.com/joaotavora/eglot/issues/574, thx bcmills
;;
;; eglot-organize-imports is hopefully a temporary stopgap until
;; https://github.com/joaotavora/eglot/issues/574 is addressed.
(defun eglot-organize-imports ()
  "Offer to execute the source.organizeImports code action."
  (interactive)
  (unless (eglot--server-capable :codeActionProvider)
    (eglot--error "Server can't execute code actions!"))
  (let* ((server (eglot--current-server-or-lose))
         (actions (jsonrpc-request
                   server
                   :textDocument/codeAction
                   (list :textDocument (eglot--TextDocumentIdentifier))))
         (action (cl-find-if
                  (jsonrpc-lambda (&key kind &allow-other-keys)
                    (string-equal kind "source.organizeImports" ))
                  actions)))
    (when action
      (eglot--dcase action
        (((Command) command arguments)
          (eglot-execute-command server (intern command) arguments))
        (((CodeAction) edit command)
          (when edit (eglot--apply-workspace-edit edit))
          (when command
            (eglot--dbind ((Command) command arguments) command
              (eglot-execute-command server (intern command) arguments))))))))

(defun eglot-organize-imports-nosignal ()
  "Run eglot-organize-imports, but demote errors to messages."
  ;; Demote errors to work around
  ;; https://github.com/joaotavora/eglot/issues/411#issuecomment-749305401
  ;; so that we do not prevent subsequent save hooks from running
  ;; if we encounter a spurious error.
  (with-demoted-errors "Error: %s" (eglot-organize-imports)))

(defun go-install-save-hooks ()
  (add-hook 'before-save-hook #'eglot-organize-imports-nosignal -10 t)
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

(use-package company
  :diminish
  :hook (go-mode . company-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(use-package find-file-in-project
  :config
  (add-to-list 'ffip-project-file '"go.mod")
  (add-to-list 'ffip-project-file '"Rakefile")
  (add-to-list 'ffip-project-file '"Gemfile")
  :bind
  ("s-t" . 'find-file-in-project))

(use-package enh-ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")

(use-package rust-mode)

(use-package yaml-mode)

(use-package protobuf-mode)

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
  (add-hook 'sh-mode-hook 'flymake-mode))

(require 'project)
(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))
(cl-defmethod project-root ((project (head go-module)))
  (cdr project))
(add-hook 'project-find-functions #'project-find-go-module)

(setq-default eglot-workspace-configuration
    '((:gopls .
        ((staticcheck . t)))))

(setq tramp-default-method "ssh")

(setq bookmark-save-flag 1)
