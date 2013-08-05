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

;; hl-line-mode
(global-hl-line-mode t)

;; linum-mode
;; (global-linum-mode t)

;; paredit
(require 'paredit)

(require 'rainbow-delimiters)
(let ((hook (lambda ()
              (paredit-mode t)
              (rainbow-delimiters-mode t))))
  (add-hook 'lisp-mode-hook hook)
  (add-hook 'emacs-lisp-mode-hook hook))

(cond (window-system (progn
                       (load-theme 'leuven)

                       ;; cursor
                       (blink-cursor-mode -1)
                       (set-cursor-color 'orange)

                       (add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))
                       (set-fontset-font t 'han (font-spec :family "STHeiti"))
                       (setq face-font-rescale-alist '(("STHeiti" . 1.2)))
                       (scroll-bar-mode -1)
                       (tool-bar-mode -1))))

;; smooth scrolling
(require 'smooth-scrolling)

;; common key bindings
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key [(f4)] 'delete-window)
(global-set-key [(f6)] 'projectile-compile-project)
(global-set-key [(f11)] 'delete-other-windows)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "M-`") 'my-quick-switch)
(global-set-key (kbd "s-T") 'my-goto-symbol)
(global-set-key (kbd "M-T") 'my-goto-symbol)
(windmove-default-keybindings)

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
(add-to-list 'auto-mode-alist '("\\Gemfile$" . ruby-mode))

(defvar ruby-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m") 'reindent-then-newline-and-indent)
    map)
  "keymap used in ruby mode")

;; lua-mode
(add-to-list 'load-path "~/.emacs.d/lua-mode")
(require 'lua-mode)

;; slim-mode
(require 'slim-mode)

;; coffee-mode
(add-to-list 'load-path "~/.emacs.d/coffee-mode/")
(require 'coffee-mode)

;; scss-mode
(add-to-list 'load-path "~/.emacs.d/scss-mode/")
(require 'scss-mode)
(setq scss-compile-at-save nil)

;; yaml-mode
(add-to-list 'load-path "~/.emacs.d/yaml-mode/")
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; ido
(ido-mode t)

;; projectile
(add-to-list 'load-path "~/.emacs.d/s.el")
(add-to-list 'load-path "~/.emacs.d/dash.el")
(add-to-list 'load-path "~/.emacs.d/projectile")
(require 'projectile)
(projectile-global-mode)

;; ack
(add-to-list 'load-path "~/.emacs.d/ack-el")
(require 'ack)

;; rvm.el
(add-to-list 'load-path "~/.emacs.d/rvm.el/")
(require 'rvm)
(rvm-use-default)

;; goto-symbol
(require 'imenu)
(defun my-goto-symbol ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position
                                     (get-text-property 1 'org-imenu-marker
                                                        symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning
    ;; of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil
                                       (mapcar
                                        (lambda (symbol)
                                          (if (string-match regexp symbol)
                                              symbol))
                                        symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol)
                    (setq symbol-names (cons symbol
                                             (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " (reverse symbol-names)))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char (if (overlayp position) (overlay-start position) position)))))

;; quick switch
(defvar *my-switch-table*
  '((("m" "mm" "c" "cpp" "cxx" "cc") ("h" "hpp" "hh"))))

(defun my-possible-exts (ext)
  (when *my-switch-table*
    (let* ((swith-item (car *my-switch-table*))
           (c1 (car swith-item))
           (c2 (cadr swith-item)))
      (cond ((find ext c1 :test #'equal) c2)
            ((find ext c2 :test #'equal) c1)
            (t (let ((*my-switch-table* (cdr *my-switch-table*)))
                 (my-possible-exts ext)))))))

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

;; eshell
(setq eshell-cmpl-ignore-case t)
;; (add-hook 'eshell-mode-hook (lambda ()
;;                               (linum-mode 0)))

;; speedbar 
(setq speedbar-show-unknown-files t)

;; server
(require 'server)
(unless (server-running-p)
  (server-start))
