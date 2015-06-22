(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(defvar my-packages '(paredit
                      cider
                      haskell-mode
                      tuareg
                      ghc
                      rainbow-delimiters
                      rainbow-mode
                      smooth-scrolling
                      lua-mode
                      yaml-mode
                      ack
                      company
                      php-mode
                      flymake-php
                      company-jedi
                      web-mode
                      jinja2-mode
                      virtualenvwrapper
                      markdown-mode
                      feature-mode
                      ggtags
                      projectile
                      js2-mode
                      slime
                      solarized-theme
                      color-theme-sanityinc-tomorrow))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; no backup files
(setq make-backup-files nil)

;; user information
(if (file-exists-p "~/.userinfo.el")
    (load "~/.userinfo.el")
  (progn
    (setq user-full-name "Steven Zhang")
    (setq user-mail-address "zhzhxtrrk@gmail.com")))

;; lisp load path
(add-to-list 'load-path "~/.emacs.d/lisp")

(cond (window-system (progn
                       (global-hl-line-mode t)

                       (add-to-list 'default-frame-alist '(width . 160))
                       (add-to-list 'default-frame-alist '(height . 45))

                       (blink-cursor-mode -1)
                       ;; (load-theme 'solarized-light t)
                       (load-theme 'sanityinc-tomorrow-bright t)
                       (set-fringe-style -1)
                       ;; (add-to-list 'default-frame-alist '(font . "Source Code Pro Light 13"))
                       (set-fontset-font t 'han (font-spec :family "STHeiti"))
                       (setq face-font-rescale-alist '(("STHeiti" . 1.1)))
                       (scroll-bar-mode -1)
                       (tool-bar-mode -1)))
      (t (progn
           ;; (load-theme 'solarized-dark t)
           (load-theme 'sanityinc-tomorrow-bright t))))

;; common key bindings
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key [(f4)] 'delete-window)
(global-set-key [(f6)] 'projectile-compile-project)
(global-set-key [(f11)] 'delete-other-windows)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(windmove-default-keybindings)

;; no startup screen
(setq inhibit-startup-message t)

;; use 4 spaces for indention
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; highlight (){}
(show-paren-mode t)

;; smooth-scrolling
(setq smooth-scroll-margin 3)

;; clojure
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

(let ((hook (lambda ()
              (paredit-mode t)
              (rainbow-delimiters-mode t))))
  (add-hook 'lisp-mode-hook hook)
  (add-hook 'emacs-lisp-mode-hook hook)
  (add-hook 'clojure-mode-hook hook))

;; objc
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))

;; ruby-mode
(add-to-list 'auto-mode-alist '("\\Gemfile$" . ruby-mode))
(defvar ruby-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m") 'reindent-then-newline-and-indent)
    map)
  "keymap used in ruby mode")

;; cmake-mode
(load-library "cmake-mode")
(add-to-list 'auto-mode-alist '("CMakeLists.txt$" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode))

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; yaml-mode
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; ido
(ido-mode t)

;; company
(global-company-mode t)

;; projectile
(projectile-global-mode t)

;; python
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;; lisp
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))

(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'php-mode)
                (flymake-mode)
              )))

;; haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; ocaml
(dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
  (setenv (car var) (cadr var)))

(add-to-list 'load-path (expand-file-name "../../share/emacs/site-lisp"
                                          (getenv "OCAML_TOPLEVEL_PATH")))

(autoload 'utop "utop" "Toplevel for OCaml" t)

(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'typerex-mode-hook 'utop-setup-ocaml-buffer)

;; path environment variable
(setq exec-path (append (parse-colon-path (getenv "PATH"))
                        (list exec-directory)))

;; eshell
(setq eshell-cmpl-ignore-case t)

;; speedbar 
(setq speedbar-show-unknown-files t)

;; server
(require 'server)
(unless (server-running-p)
  (server-start))
