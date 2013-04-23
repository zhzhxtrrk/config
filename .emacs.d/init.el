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
(add-to-list 'custom-theme-load-path "~/.emacs.d")

(custom-set-variables
 '(custom-safe-themes
   '("608eb09bdd67de505df53ea96d2b46e5a9ac16241a238dd3ab8001e7852d9659"
     "a7e47993e8887d433c83ac082c954bfe566bcfb1fcf0165c3e52fc9ccd37cf9b" default)))
(load-theme 'molokai)
; (load-theme 'emacslive-cyberpunk)

;; hl-line-mode
(global-hl-line-mode t)

;; cursor
(blink-cursor-mode -1)
; (set-default 'cursor-type 'bar)

;; paredit
(require 'paredit)

(require 'rainbow-delimiters)
(let ((hook (lambda ()
              (paredit-mode t)
              (rainbow-delimiters-mode t))))
  (add-hook 'lisp-mode-hook hook)
  (add-hook 'emacs-lisp-mode-hook hook))

(cond (window-system (progn
                       (scroll-bar-mode -1)
                       (tool-bar-mode -1))))

;; smooth scrolling
(require 'smooth-scrolling)

;; common key bindings
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key [(f4)] 'delete-window)
(global-set-key [(f6)] 'my-compile-command)
(global-set-key [(f11)] 'delete-other-windows)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "M-`") 'my-quick-switch)

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
  (setq compile-command (let (root (textmate-project-root))
                          (cond (root (concat "make -k " "-C " root " "))
                                (t "make -k "))))
  (call-interactively 'compile))

;; auto-complete
(add-to-list 'load-path "~/.emacs.d/popup-el")
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-modes 'objc-mode)
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-auto-start nil)
(setq ac-trigger-key "M-/")

;; gtags
(autoload 'gtags-mode "gtags" "" t)
(defun my-enable-gtags ()
  (interactive)
  (setq gtags-suggested-key-mapping t)
  (setq gtags-auto-update t)
  (gtags-mode t)
  (gtags-visit-rootdir))

;; yasnippet
(add-to-list 'load-path (expand-file-name "~/.emacs.d/yasnippet"))
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/yasnippet/snippets"
        "~/.emacs.d/my_snippets/"))
(yas-global-mode t)

;; lisp
(setq inferior-lisp-program "/usr/local/bin/ccl64") ; your Lisp system
(add-to-list 'load-path "~/.emacs.d/slime-2012-09-03")  ; your SLIME directory
(require 'slime)
(slime-setup)

;; php-mode
(require 'php-mode)
(add-hook 'php-mode-hook (lambda ()
                      (flymake-mode t)))

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
; switch between html-mode and web-mode
(let ((web-hook (lambda ()
                  (local-set-key (kbd "M-`")
                                 (lambda ()
                                   (interactive)
                                   (if (eq 'web-mode major-mode)
                                       (html-mode)
                                     (web-mode)))))))
  (add-hook 'web-mode-hook web-hook)
  (add-hook 'html-mode-hook web-hook))


;; speedbar 
(setq speedbar-show-unknown-files t)



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
