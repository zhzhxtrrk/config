;; no backup files
(setq make-backup-files nil)

;; user information
(setq user-full-name "Steven Zhang")
(setq user-mail-address "zhzhxtrrk@gmail.com")

;; lisp load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;; look and feel
(setq-default cursor-type 'bar)
(load-theme 'tango-dark)
(require 'rainbow-delimiters)
(add-hook 'lisp-mode-hook (lambda ()
			    (rainbow-delimiters-mode t)))
(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (rainbow-delimiters-mode t)))
(add-hook 'clojure-mode-hook (lambda ()
			       (rainbow-delimiters-mode t)))

(cond (window-system (progn
		       (scroll-bar-mode -1)
		       (tool-bar-mode -1)
		       (setq default-frame-alist '((width . 100) (height . 45)))
		       (set-default-font "Menlo-13")
		       (add-to-list 'default-frame-alist '(font . "Menlo-13")))))

;; smooth scrolling
(require 'smooth-scrolling)

;; common key bindings
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-x C-c") (lambda ()
				  (interactive)
				  (message "Want to quit? try M-x save-buffers-kill-terminal RET")))
(global-set-key [(f2)] 'shell-command)
(global-set-key [(f3)] 'isearch-forward)
(global-set-key [(f4)] 'delete-window)
(global-set-key [(f5)] 'revert-buffer)
(global-set-key [(f6)] 'my-compile-command)
(global-set-key [(f10)] 'dirtree-textmate-project)
(global-set-key [(f11)] 'delete-other-windows)

;; no startup screen
(setq inhibit-startup-message t)

;; use 4 spaces for indention
(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset 4)

;; highlight (){}
(show-paren-mode t)

;; objc
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))

;; ruby-mode
(defvar ruby-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m") 'reindent-then-newline-and-indent)
    map)
  "keymap used in ruby mode")

;; php-mode
(require 'php-mode)
(add-hook 'php-mode-hook
	  '(lambda ()
	     (flymake-mode t)))

;; haskell-mode
(load "~/.emacs.d/haskell-mode/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; clojure-mode
(add-to-list 'load-path "~/.emacs.d/clojure-mode")
(require 'clojure-mode)
(add-to-list 'load-path "~/.emacs.d/nrepl")
(require 'nrepl)
(add-hook 'nrepl-interaction-mode-hook
          (lambda ()
	    (nrepl-turn-on-eldoc-mode t)))

;; multi-web-mode
(add-to-list 'load-path "~/.emacs.d/multi-web-mode")
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
		  (ruby-mode "<%" "%>")
		  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
		  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("htm" "html" "phtml" "erb"))
(multi-web-global-mode t)

;; textmate
(require 'textmate)
(textmate-mode t)

;; compile
(setq compilation-scroll-output t)
(defun my-compile-command ()
  (interactive)
  (let ((*textmate-project-roots* '(".emacs-project" "Makefile")))
    (setq compile-command ((lambda()
			     (let ((root (textmate-project-root)))
			       (cond (root (concat "make -k " "-C " root))
				     (t "make -k "))))))
    (call-interactively 'compile)))

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/popup-el")
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-auto-start nil)
(setq ac-trigger-key "M-/")

;; dirtree
(require 'dirtree)
(defun dirtree-textmate-project ()
  (interactive)
  (let ((project-root (textmate-project-root)))
    (cond (project-root (dirtree-in-buffer project-root t))
	  (t (message "no project found")))))

;; git
(add-to-list 'load-path (expand-file-name "~/.emacs.d/magit-1.1.1"))
(require 'magit)

;; yasnippet
(add-to-list 'load-path (expand-file-name "~/.emacs.d/yasnippet"))
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/yasnippet/snippets"))
(yas-global-mode t)

;; common lisp
(setq inferior-lisp-program "/usr/local/bin/ccl64") ; your Lisp system
(add-to-list 'load-path "~/.emacs.d/slime-2012-09-03")  ; your SLIME directory
(require 'slime)
(slime-setup)

;; geben debuging support
(add-to-list 'load-path (expand-file-name "~/.emacs.d/geben"))
(autoload 'geben "geben" "DBGp protocol frontend, a script debugger" t)

;; emacs server
(unless (server-running-p)
  (server-start))

(setq speedbar-show-unknown-files t)
