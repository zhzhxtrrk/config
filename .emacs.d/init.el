(if (string-equal "darwin" (symbol-name system-type))
      (setenv "PATH" (concat "/usr/local/bin:~/.cabal/bin:" (getenv "PATH"))))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(defvar my-packages '(paredit
                      rainbow-delimiters
                      rainbow-mode
                      smooth-scrolling
                      projectile
                      company

                      lua-mode
                      
                      php-mode
                      web-mode

                      less-css-mode
                      sass-mode
                      js2-mode
                      tide

                      cider

                      haskell-mode
                      flycheck-haskell
                      company-ghc
                      hi2
                      
                      cmake-mode

                      badwolf-theme
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
                       (load-theme 'badwolf t)
                       ;; (load-theme 'solarized-dark t)
                       ;; (load-theme 'leuven)
                       ;; (set-fringe-style -1)
                       (add-to-list 'default-frame-alist '(font . "Monaco 12"))
                       (set-fontset-font t 'han (font-spec :family "PingFang SC"))
                       (setq face-font-rescale-alist '(("PingFang SC" . 0.95)))
                       (scroll-bar-mode -1)
                       (tool-bar-mode -1)))
      (t (progn
           (load-theme 'badwolf t))))

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

;; gbk first
(prefer-coding-system 'gbk)
(prefer-coding-system 'utf-8)

;; objc
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; ido
(ido-mode t)

;; company
(global-company-mode t)

;; projectile
(projectile-global-mode t)

;; php
(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'php-mode)
                (flycheck-mode))))

;; path environment variable
(setq exec-path (append (parse-colon-path (getenv "PATH"))
                        (list exec-directory)))

;; eshell
(setq eshell-cmpl-ignore-case t)

;; speedbar 
(setq speedbar-show-unknown-files t)

;; typescript
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
            ;; company is an optional dependency. You have to
            ;; install it separately via package-install
            (company-mode-on)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; Tide can be used along with web-mode to edit tsx files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (tide-setup)
              (flycheck-mode +1)
              (setq flycheck-check-syntax-automatically '(save mode-enabled))
              (eldoc-mode +1)
              (company-mode-on))))

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; haskell
(add-hook 'haskell-mode-hook
          (lambda ()
            (hi2-mode)
            (flycheck-mode)
            (ghc-init)
            (interactive-haskell-mode)))

(add-to-list 'company-backends 'company-ghc)
(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)


;; server
(require 'server)

(unless (server-running-p)
  (server-start))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
