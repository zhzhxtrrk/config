;; no backup files
(setq make-backup-files nil)

;; user information
(setq user-full-name "Steven Zhang")
(setq user-mail-address "zhzhxtrrk@gmail.com")

;; common key bindings
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key [(f2)] 'shell-command)
(global-set-key [(f3)] 'isearch-forward)
(global-set-key [(f4)] 'delete-window)
(global-set-key [(f6)] 'compile)
(global-set-key [(f10)] 'dirtree-textmate-project)
(global-set-key [(f11)] 'delete-other-windows)

;; no startup screen
(setq inhibit-startup-message t)

;; use 4 spaces for indention
(setq-default indent-tabs-mode -1)
(setq default-tab-width 4)

;; highlight (){}
(show-paren-mode t)

;; ruby-mode
(defvar ruby-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m") 'reindent-then-newline-and-indent)
    map)
  "keymap used in ruby mode")

;; lisp load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;; php-mode
(require 'php-mode)
(add-hook 'php-mode-hook
		  '(lambda ()
			 (flymake-mode)))

;; lua-mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; cmake-mode
(autoload 'cmake-mode "cmake-mode" "CMake editing mode." t)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;; haskell-mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/haskell-mode-2.8.0"))
(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;; go mode
(require 'go-mode-load)

;; js2-mode, may need to install from elpa first, not managed in git
(add-hook 'js2-mode-hook
		  '(lambda()
			 (define-key js2-mode-map (kbd "C-m") 'newline-and-indent)))

;; textmate
(require 'textmate)
(textmate-mode t)


;; dirtree
(require 'dirtree)
(setq dirtree-windata '(frame left 0.2 delete))
(defun dirtree-textmate-project ()
  (interactive)
  (let ((project-root (textmate-project-root)))
	   (cond (project-root (dirtree-in-buffer project-root t))
			 (t (message "no project found")))))


;; git
(add-to-list 'load-path (expand-file-name "~/.emacs.d/magit-1.1.1"))
(require 'magit)

;; shell
(setq eshell-login-script "profile")
(add-hook 'term-exec-hook
		  #'(lambda ()
			  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))

;; common lisp
(setq inferior-lisp-program "/usr/local/bin/sbcl") ; your Lisp system
(add-to-list 'load-path "~/.emacs.d/slime-2012-09-03")  ; your SLIME directory
(require 'slime)
(slime-setup)

;; smooth scrolling
(require 'smooth-scrolling)

;; look and feel
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
	(color-theme-initialize)
	(cond (window-system (color-theme-cyberpunk)))))

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(cond (window-system (progn
		       (scroll-bar-mode -1)
		       (tool-bar-mode -1)
		       (setq default-frame-alist '((width . 100) (height . 45)))
		       (set-default-font "Consolas-15")
		       (add-to-list 'default-frame-alist '(font . "Consolas-15")))))


;; geben debuging support
(add-to-list 'load-path (expand-file-name "~/.emacs.d/geben"))
(autoload 'geben "geben" "DBGp protocol frontend, a script debugger" t)

;; emacs server
(unless (server-running-p)
  (server-start))

