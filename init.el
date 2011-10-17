(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
  starter-kit
  starter-kit-bindings
  starter-kit-ruby
  markdown-mode
  yaml-mode
  coffee-mode
  color-theme-ir-black
  gist
  go-mode
  haml-mode
  textmate
  auto-indent-mode
  maxframe
  marmalade))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

; needed to run things installed via homebrew, such as aspell
(when (equal system-type 'darwin)
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (push "/usr/local/bin" exec-path))

(require 'color-theme-ir-black)
(color-theme-ir-black)

(server-mode 1)

(require 'textmate)
(textmate-mode 1)
; reset the list of ignored things; previously included at least 'log'
(setq *textmate-gf-exclude*
   "/\\.|vendor|tmp|\\.xcodeproj|\\.nib|\\.framework|\\.app|\\.pbproj|\\.pbxproj|\\.xcode|\\.xcodeproj|\\.bundle|\\.pyc|\\.rbc")
; reset this to include Gemfile
(setq *textmate-project-roots*
   '(".git" ".hg" "Rakefile" "Makefile" "Gemfile" "README" "build.xml"))

(set-default-font "Menlo-12")

(require 'maxframe)
(setq mf-max-width 1600)
(add-hook 'window-setup-hook 'maximize-frame t)

; autoindent on yank-pop
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

; brew install apsell --lang=en
(setq ispell-program-name "aspell")

(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

(setq-default fill-column 120)

(setq ruby-deep-indent-paren nil)
(setq ruby-deep-arglist nil)
(add-hook 'ruby-mode-hook
          '(lambda ()
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)))

(require 'linum)
(global-linum-mode 1)
(setq linum-format "%d ")
