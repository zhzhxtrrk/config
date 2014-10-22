(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/")
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
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
                      coffee-mode
                      scss-mode
                      yaml-mode
                      ack
                      rvm
                      company
                      company-cmake
                      ggtags
                      projectile
                      js2-mode
                      solarized-theme
                      color-theme-sanityinc-tomorrow
                      cpputils-cmake))
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
; (add-to-list 'load-path "~/.emacs.d")

(cond (window-system (progn
                       (global-hl-line-mode t)

                       ;; (add-to-list 'default-frame-alist '(width . 120))
                       ;; (add-to-list 'default-frame-alist '(height . 45))

                       (blink-cursor-mode -1)
                       (load-theme 'solarized-dark t)
                       ;; (load-theme 'sanityinc-tomorrow-night t)
                       (set-fringe-style -1)
                       (add-to-list 'default-frame-alist '(font . "Source Code Pro 15"))
                       (set-fontset-font t 'han (font-spec :family "STHeiti"))
                       (setq face-font-rescale-alist '(("STHeiti" . 1.1)))
                       (scroll-bar-mode -1)
                       (tool-bar-mode -1)))
      (t (progn
           ;; (load-theme 'solarized-dark t)
           (load-theme 'sanityinc-tomorrow-night t))))

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

;; slim-mode
(require 'slim-mode)

;; scss
(setq scss-compile-at-save nil)

;; yaml-mode
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; ido
(ido-mode t)

;; rvm
(rvm-use-default)

;; projectile
(projectile-global-mode t)

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

;; lisp
(add-to-list 'load-path "~/.emacs.d/slime-2.6")
(require 'slime)
(slime-setup)
(setq inferior-lisp-program "/usr/local/bin/sbcl") ; your Lisp system

(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'c-mode 'c++-mode)
                (cppcm-reload-all)
              )))

;; haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; ocaml
(dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
  (setenv (car var) (cadr var)))

(setq exec-path (append (parse-colon-path (getenv "PATH"))
                        (list exec-directory)))

(add-to-list 'load-path (expand-file-name "../../share/emacs/site-lisp"
                                          (getenv "OCAML_TOPLEVEL_PATH")))

(autoload 'utop "utop" "Toplevel for OCaml" t)

(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'typerex-mode-hook 'utop-setup-ocaml-buffer)

;; eshell
(setq eshell-cmpl-ignore-case t)

;; speedbar 
(setq speedbar-show-unknown-files t)

;; server
(require 'server)
(unless (server-running-p)
  (server-start))
