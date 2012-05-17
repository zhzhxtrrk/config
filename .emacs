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

;; full develop environment
(defun my-load-dev ()
  (interactive)

  (global-ede-mode)
  
  (custom-set-variables
   '(semantic-default-submodes '(global-semantic-decoration-mode
                                 global-semantic-idle-completions-mode
                                 global-semantic-idle-scheduler-mode
                                 global-semanticdb-minor-mode
                                 global-semantic-idle-summary-mode
                                 ;; global-semantic-highlight-func-mode
                                 ;; global-semantic-idle-local-symbol-highlight-mode
                                 global-semantic-mru-bookmark-mode))
   '(semantic-idle-scheduler-idle-time 1))
  (semantic-mode)

  (global-set-key "\C-cj" 'semantic-ia-fast-jump)
  (global-set-key "\C-cc" 'semantic-ia-complete-symbol)
  (global-set-key "\C-cd" 'semantic-ia-show-doc)

  ;; (ede-cpp-root-project "FishingStar-Android"
  ;;                       :name "FishingStar-Android"
  ;;                       :file "/Users/zhzhxtrrk/Projects/Android/CMakeLists.txt"
  ;;                       :include-path '("/arm-linux-androideabi-4.4.3/arm-linux-androideabi/include/c++"
  ;;                                       "/Classes/TinyXml")
  ;;                       )
  )

(my-load-dev)

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

;; haskell mode
(load "~/.emacs.d/haskell-mode-2.8.0/haskell-site-file.el")
(defun my-haskell-hook ()
  (haskell-doc-mode 1)
  (haskell-indentation-mode -1)
  (haskell-indent-mode 1)
  )
(add-hook 'haskell-mode-hook 'my-haskell-hook)

;; go mode
(require 'go-mode-load)

;; eshell
(defun my-shell ()
  (interactive)
  (ansi-term "/bin/bash")
  ;; (rename-uniquely)
  )

;; look and feel
;; (set-cursor-color "#ff0000")
;; (set-background-color "#111111")
;; (set-foreground-color "#F4F4F4")
(setq-default cursor-type 'bar)

