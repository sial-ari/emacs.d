(require 'cl)
(require 'package)

;; Define package repositories
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")))

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))
;; old list of packages
;; TODO: migrate to use-package
(defvar my-packages
  '(
    paredit
    cheatsheet
    rainbow-delimiters
    smex
    exwm
    dash
    yaml-mode
    vagrant
    vagrant-tramp
    dracula-theme
    elpy
    pyenv-mode
    emamux
    emms
    helm
    racket-mode
    tagedit
    slime
    multiple-cursors
    ace-window
    markdown-preview-mode
    markdown-mode
    websocket
    org-bullets
    multi-term
    powerline
    flycheck
    free-keys
    google-translate
    magitk
    github-clone
    git-auto-commit-mode
    json-mode
    powerline
    ag
    nlinum
    sane-term
    ))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; load use-package
(progn ; `use-package'
  ;;(setq use-package-always-defer t)
  (setq use-package-minimum-reported-time 0)
  (setq use-package-verbose t)
  (setq use-package-compute-statistics t)
  (require 'use-package))
>>>>>>> Save current state

;; install el get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

;; install use-package if missing
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; install use-package-el-get if missing
(unless (package-installed-p 'use-package-el-get)
  (package-install 'use-package-el-get))

(setq
 my:el-get-packages
 '(el-get				; el-get is self-hosting
         ))

;; load use-package-el-get
(require 'use-package-el-get)
(setq use-package-always-ensure nil)
(use-package-el-get-setup)


;; stuff needed here
;; Free personal key bindings space.
(global-unset-key (kbd "M-j"))

;; =============================================================================
;; disabled packages
(use-package exwm
  :disabled t)

(use-package emamux
  :disabled t)

(use-package dash
  :disabled t)

(use-package vagrant
  :disabled t)

(use-package vagrant-tramp
  :disabled t)

(use-package emms
  :disabled t)

(use-package helm
  :disabled t)

(use-package racket-mode
  :disabled t)

;; ob-tmux
(use-package ob-tmux
  ;; Install package automatically (optional)
  :disabled t
  :ensure t
  :custom
  (org-babel-default-header-args:tmux
   '((:results . "silent")         ;
     (:session . "default")        ; The default tmux session to send code to
     (:socket  . "/tmp/shared-session.sock")))            ; The default tmux socket to communicate with
  ;; The tmux sessions are prefixed with the following string.
  ;; You can customize this if you like.
  (org-babel-tmux-session-prefix "ob-")
  ;; The terminal that will be used.
  ;; You can also customize the options passed to the terminal.
  ;; The default terminal is "gnome-terminal" with options "--".
  ;; (org-babel-tmux-terminal "")
  ;; (org-babel-tmux-terminal-opts '("-T" "ob-tmux" "-e"))
  ;; (org-babel-tmux-terminal-opts '("--"))
  ;; Finally, if your tmux is not in your $PATH for whatever reason, you
  ;; may set the path to the tmux binary as follows:
  (org-babel-tmux-location "/usr/local/bin/tmux"))

;; =============================================================================
;; use-package definitions

;; eshell screen
(use-package escreen
  :el-get t
  :defer t)

;; dired+
(use-package dired+
  :el-get t
  :defer t
  :config
  ;; reuse dired buffer
  (diredp-toggle-find-file-reuse-dir 1))

;; better search and replace
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode))

(use-package aria2
  :ensure t)

(use-package clipmon
  :ensure t
  :defer 0.1
  :config
  (clipmon-mode))

(use-package ace-window
  :ensure t
  :init
  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Show vertical indentation lines
(use-package indent-guide
  :ensure t
  :config
  (indent-guide-mode +1)
  (add-hook 'prog-mode-hook 'indent-guide-mode))

;; show available keybindings on inactivity
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package quelpa
  :ensure t)

(use-package quelpa-use-package
  :ensure t)

;; Easy navigation without modifier keys
(use-package god-mode
  :ensure t
  ;;:bind ("M-<return>" . god-local-mode)
  :config
  (defun god-mode-update-cursor () (setq cursor-type (if (or god-local-mode buffer-read-only) 'hbar 'box)))
  (add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
  (add-hook 'god-mode-disabled-hook 'god-mode-update-cursor))

;; save buffer when they loose focus
(use-package super-save
  :ensure t
  :diminish t
  :config
  (super-save-mode +1))

;; google-translate
(use-package google-translate
  :ensure t
  :bind (("M-j t p"   . google-translate-at-point)
         ("M-j T p"   . google-translate-at-point-reverse)
         ("M-j M-t" . google-translate-at-point)
         ("M-j M-T" . google-translate-at-point-reverse) ("M-j t q" . google-translate-query-translate)
         ("M-j T q" . google-translate-query-translate-reverse))
  :init
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "bg"))

;; modes are minor modes with no modeline display
(use-package diminish
  :ensure t
  :init
  (progn
    (diminish 'git-gutter-mode)
    (diminish 'flycheck-mode)
    (diminish 'paredit-mode)
    (diminish 'which-key-mode)
    (diminish 'flyspell-mode)))

;;  uniquify overrides Emacs’ default mechanism for making buffer names unique
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; ispell
(use-package ispell
  :bind (("M-j s"   . 'ispell-word)
         ("M-j M-s" . 'ispell-word))
  :config
  (setq-default ispell-program-name "aspell")
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

;; desktop-environment
(use-package desktop-environment
  :ensure t
  :diminish desktop-environment-mode
  :init
  (setq desktop-environment-screenshot-directory "~/")
  (setq desktop-environment-screenlock-command "xscreensaver-command --lock")
  :config
  (desktop-environment-mode +1))

;; symon
(use-package symon
  :ensure t
  :config
  (symon-mode +1))

;; exwm-edit
(use-package exwm-edit
  :ensure t)

;; dumb-jump
(use-package dumb-jump
  :ensure t
  :config
  (dumb-jump-mode +1))

;; tempbuf-mode
(use-package tempbuf
  :ensure nil
  :load-path "lisp/tempbuf"
  :config
  (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'view-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'custom-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'man-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'completion-list-mode-hook 'turn-on-tempbuf-mode)
  (add-hook 'help-mode-hook 'turn-on-tempbuf-mode))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-c h" . heplful-at-point))
  :config
  (advice-add #'describe-key :override #'helpful-key)
  (advice-add #'describe-function :override #'helpful-callable)
  (advice-add #'describe-variable :override #'helpful-variable)
  (advice-add #'describe-symbol :override #'helpful-symbol))

;; restart emacs
(use-package restart-emacs
  :ensure t)

(use-package wallpaper
  :load-path "lisp/wallpaper"
  :custom ((wallpaper-static-wallpapers '("~/downloads/narcosynthesis_by_f1x_2_d9t2a3a-fullview.jpg"))))

(use-package exwm-background
  :load-path "lisp/exwm-background")

(use-package hydra
  :ensure t)

;; all-the-icons
(use-package all-the-icons
  :ensure t
  :init
  (require 'all-the-icons))


(use-package powerline
  :ensure t
  ;; :init
  ;; (require 'powerline)
  ;; :config
  ;; (powerline-default-theme)
)

;; spaceline
(use-package spaceline
  ;; :after powerline
  :ensure t
  ;; :init
  ;; (require 'spaceline-config)
  ;; :config
  ;; (spaceline-spacemacs-theme)
  ;; (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati))))
)

;; keychain-environment
(use-package keychain-environment
 :ensure t
 :config (keychain-refresh-environment))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package perspective
  :ensure t
  :commands persp-mode
  :config)

(use-package toml-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package shell-pop
  :ensure t)

(use-package elscreen
  :ensure t
  :defer t
;;  :init (elscreen-start)
)

(use-package dumb-jump
  :ensure t)

(use-package auto-complete
  :ensure t
  :mode ("\\.go\\'" . auto-complete-mode))

(use-package go-autocomplete
  :ensure t)

(use-package ac-emoji
  :ensure t)

;; go-mode 
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . auto-complete-mode)
  :config
  (exec-path-from-shell-copy-env "GOPATH")
  ;; (add-hook 'before-save-hook 'gofmt-before-save)
  (with-eval-after-load 'go-mode
    (require 'go-autocomplete)))

(use-package ob-go
  :ensure t)

(use-package popwin
  :ensure t
  :config
  (progn
    (setq popwin:special-display-config nil)
    (push '("*Backtrace*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.33)
          popwin:special-display-config)
    (push '("*compilation*"
            :dedicated t :position bottom :stick t :noselect t   :height 0.2)
          popwin:special-display-config)
    (push '("*Compile-Log*"
            :dedicated t :position bottom :stick t :noselect t   :height 0.33)
          popwin:special-display-config)
    (push '("*Help*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.33)
          popwin:special-display-config)
    (push '("*Shell Command Output*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.33)
          popwin:special-display-config)
    (push '("*undo-tree*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.33)
          popwin:special-display-config)
    (push '("*Warnings*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.33)
          popwin:special-display-config)
    (push '("*Gofmt Errors*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.33)
          popwin:special-display-config)
    (push '("^\\*Man .*\\*$"
            :regexp t    :position bottom :stick t :noselect nil :height 0.33)
            popwin:special-display-config)
    (push '("^\\*ag\ search .*\\*$"
            :regexp t    :position bottom :stick t :noselect nil :height 0.33)
            popwin:special-display-config)
    (popwin-mode t)))

(use-package smooth-scrolling
  :ensure t
  :config
    (setq scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
    (setq-default scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01))

(use-package dired+
  :quelpa (dired+ :fetcher github :repo "emacsmirror/dired-plus")
  :config (diredp-toggle-find-file-reuse-dir 1))

;; Tree-based directory browsing
(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))



(use-package yaml-mode
  :mode 
  ("\\.yml\\'" . yaml-mode)
  ("\\.yaml\\'" . yaml-mode)
  ("\\.sls\\'" . yaml-mode))

(use-package json-mode
  :mode 
  ("\\.json\\'" . yaml-mode))

(use-package markdown-mode
  :mode 
  ("\\.md\\'" . markdown-mode))

(use-package markdown-preview-mode
  :defer t)

(use-package google-translate
  :defer t)

(use-package google-translate-smooth-ui
  :bind
  ("C-c t" . google-translate-smooth-translate))

(use-package gitlab-ci-mode
  :mode 
  ("\\.gitlab-ci.*\\'" . gitlab-ci-mode))

(use-package git-auto-commit-mode
  :defer t)

(use-package github-clone
  :defer t)

(use-package ag
  :defer t)

(use-package free-keys
  :defer t)

(use-package magit
  :defer t)

(use-package elpy
  :mode ("\\.py\\'" . elpy-mode)
  :config (elpy-enable))

(use-package pyenv
  :mode ("\\.py\\'" . pyenv-mode)
  :config
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  (pyenv-mode))

(use-package multi-term
  :defer t
  :config
  (setq multi-term-program "/bin/zsh"))

(use-package nlinum
  :defer t)

(use-package flycheck
  :hook
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package lua-mode
  :defer t)

(use-package multiple-cursors
  :defer t)

(use-package slime
  :defer t)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package websocket
  :ensure t)

(use-package fzf
  :ensure t)

(use-package edwina
  :ensure t
  :config
  (add-hook 'after-init-hook #'edwina-mode))

(use-package restclient
  :ensure t
  :mode
  ("\\.http\\'" . restclient-mode))

(use-package restclient-test
  :ensure t
  :hook
  (restclient-mode-hook . restclient-test-mode))

(use-package ob-restclient
  :ensure t
  :after org restclient
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package paredit
  :ensure t
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

(use-package rainbow-delimiters
  :ensure t)


;; (use-package browse-kill-ring
;;   ;;:defer 5
;;   :commands browse-kill-ring)

;; (use-package browse-kill-ring+
;;   :after browse-kill-ring
;;   :config (browse-kill-ring-default-keybindings))

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

;; spaceline-all-the-icons
(use-package spaceline-all-the-icons
  :after spaceline
  :ensure t
  :config (spaceline-all-the-icons-theme))


;; all-the-icons-dired
(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (all-the-icons-dired-mode 1)
              )))


(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'setup-packages)
