;; no backup files
(setq make-backup-files nil)

;; user information
(if (file-exists-p "~/.userinfo.el")
    (load "~/.userinfo.el")
  (progn
    (setq user-full-name "Mockup user")
    (setq user-mail-address "mOckUp@UsEr.cOm")))

;; lisp load path
(add-to-list 'load-path "~/.emacs.d")

;; safe themes
(custom-set-variables
 '(custom-safe-themes (quote ("6bc195f4f8f9cf1a4b1946a12e33de2b156ce44e384fbc54f1bae5b81e3f5496" default))))

;; look and feel
(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))

;; add cyberpunk-theme load-path
(add-to-list 'custom-theme-load-path "~/.emacs.d/cyberpunk-theme")

;; load color theme
(load-theme 'misterioso)

;; cursor
(add-to-list 'default-frame-alist
             '(cursor-color . "yellow"))
(blink-cursor-mode -1)

;; paredit
(require 'paredit)

(require 'rainbow-delimiters)
(let ((hook (lambda ()
              (paredit-mode t)
              (rainbow-delimiters-mode t))))
  (add-hook 'lisp-mode-hook hook)
  (add-hook 'emacs-lisp-mode-hook hook)
  (add-hook 'clojure-mode-hook hook))

(cond (window-system (progn
                       (set-fringe-mode '(1 . 1))
                       (scroll-bar-mode -1)
                       (tool-bar-mode -1)
                       ;; (global-hl-line-mode t)
                       )))

 ;; smooth scrolling
(require 'smooth-scrolling)

;; common key bindings
(global-set-key (kbd "C-m") 'newline-and-indent)
(when window-system
  (global-set-key (kbd "C-x C-c") (lambda ()
                                    (interactive)
                                    (message "Want to quit? try M-x save-buffers-kill-terminal RET"))))
(global-set-key [(f2)] 'shell-command)
(global-set-key [(f3)] 'isearch-forward)
(global-set-key [(f4)] 'delete-window)
(global-set-key [(f6)] 'my-compile-command)
(global-set-key [(f10)] 'dirtree-textmate-project)
(global-set-key [(f11)] 'delete-other-windows)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "s-b") 'my-quick-switch)

;; no startup screen
(setq inhibit-startup-message t)

;; use 4 spaces for indention
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

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

;; quick switch
(defvar *my-switch-table*
  '((("m" "mm" "c" "cpp" "cxx" "cc") ("h" "hpp" "hh"))))

(defun my-possible-exts (ext)
  (when *my-switch-table*
    (let ((swith-item (car *my-switch-table*)))
      (let ((c1 (car swith-item))
            (c2 (cadr swith-item)))
        (cond ((find ext c1 :test #'equal) c2)
              ((find ext c2 :test #'equal) c1)
              (t (let ((*my-switch-table* (cdr *my-switch-table*)))
                   (my-possible-exts ext))))))))

(defun my-try-switch (file-path-without-ext possible-exts)
  (when possible-exts
    (let ((file-path (concat file-path-without-ext "." (car possible-exts))))
      (if (and (file-exists-p file-path) (file-regular-p file-path))
          (find-file file-path)
        (my-try-switch file-path-without-ext (cdr possible-exts))))))

(defun my-quick-switch ()
  (interactive)
  (let ((file-path (buffer-file-name)))
    (when file-path
      (my-try-switch (file-name-sans-extension file-path)
                     (my-possible-exts (file-name-extension file-path))))))

;; compile
(setq compilation-scroll-output t)
(defun my-compile-command ()
  (interactive)
  (let ((*textmate-project-roots* '(".emacs-project" "Makefile")))
    (setq compile-command (let (root (textmate-project-root))
                            (cond (root (concat "make -k " "-C " root " "))
                                  (t "make -k "))))
    (call-interactively 'compile)))

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/popup-el")
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-modes 'objc-mode)
(ac-config-default)
(global-auto-complete-mode t)
;; (setq ac-auto-start nil)
;; (setq ac-trigger-key "M-/")

;; gtags
(autoload 'gtags-mode "gtags" "" t)
(defun my-enable-gtags ()
  (interactive)
  (setq gtags-suggested-key-mapping t)
  (setq gtags-auto-update t)
  (gtags-mode t)
  (gtags-visit-rootdir))

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

;; emacs server
(unless (server-running-p)
  (server-start))

;; speedbar 
(setq speedbar-show-unknown-files t)
