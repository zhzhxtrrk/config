;; no backup files
(setq make-backup-files nil)

;; user information
(setq user-full-name "Steven Zhang")
(setq user-mail-address "zhzhxtrrk@gmail.com")

;; common key bindings
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key [(f2)] 'shell-command)
(global-set-key [(f4)] 'delete-other-windows)
(global-set-key [(f12)] 'compile)

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
    (define-key map (kbd "C-j") 'newline)
    (define-key map (kbd "C-m") 'reindent-then-newline-and-indent)
    map)
  "keymap used in ruby mode")

;; lisp load path
(setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))

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

;; go mode
(require 'go-mode-load)

;; eshell
(setq eshell-login-script "profile")

;; look and feel
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
	(color-theme-initialize)
	(color-theme-blue-mood)))

(scroll-bar-mode -1)
