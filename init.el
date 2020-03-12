;;; init.el --- pid 0 for Emacs

;;; Commentary:
;;  pid 0 for Emacs  --- init

;;; Code:
;; Set up load path.(shamelessly stolen from https://github.com/ivo-)
;; garbage collector hacks :D
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'cl)
(require 'package)

;; Define package repositories
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
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

;; load use-package
(progn ; `use-package'
  ;;(setq use-package-always-defer t)
  (setq use-package-minimum-reported-time 0)
  (setq use-package-verbose t)
  (setq use-package-compute-statistics t)
  (require 'use-package))

;; el get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

(setq
 my:el-get-packages
 '(el-get				; el-get is self-hosting
   dired+))

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

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

;; Better search and replace
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode))

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

;; Show available keybindings on inactivity
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))



;; Easy navigation without modifier keys
(use-package god-mode
  :ensure t
  ;;:bind ("M-<return>" . god-local-mode)
  :config
  (defun god-mode-update-cursor () (setq cursor-type (if (or god-local-mode buffer-read-only) 'hbar 'box)))
  (add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
  (add-hook 'god-mode-disabled-hook 'god-mode-update-cursor))

;; Save buffer when they loose focus
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
         ("M-j M-T" . google-translate-at-point-reverse)
         ("M-j t q" . google-translate-query-translate)
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

;; Tree-based directory browsing
(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))



(use-package yaml-mode
  :mode 
  ("\\.yml\\" . yaml-mode)
  ("\\.yaml\\" . yaml-mode)
  ("\\.sls\\" . yaml-mode))

(use-package json-mode
  :mode 
  ("\\.json\\" . yaml-mode))

(use-package markdown-mode
  :mode 
  ("\\.md\\" . markdown-mode))

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


(use-package browse-kill-ring
  ;;:defer 5
  :commands browse-kill-ring)

(use-package browse-kill-ring+
  :after browse-kill-ring
  :config (browse-kill-ring-default-keybindings))

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

;; (provide 'setup-packages)

;; (require 'setup-packages)
(require 'setup-settings)
(require 'setup-keybindings)
(require 'setup-packages)
(require 'setup-defuns)

;; exwm
(require 'exwm)
(require 'exwm-systemtray)
(require 'exwm-config)

;; the big comment
;; (require 'exwm-randr)

;; Enable 2/3 monitor home setup with orientation
;; (defun my-exwm-xrandr-outputs (default &optional first orientation_first second orientation_second)
;;   (cond ((and first orientation_first second orientation_second) 
;;          (shell-command
;;           (concat "xrandr --output " default
;;                   " --off --output " first " --auto --primary --rotate " orientation_first
;;                   " --right-of " second
;;                   " --output " second " --auto --rotate " orientation_second )))
;;         ((and first orientation_first) 
;;          (shell-command
;;           (concat "xrandr --output " first 
;;                   " --left-of " default " --auto " 
;;                   " --rotate " orientation_first)))
;;         ((stringp default)
;;          (shell-command
;;           (concat "xrandr --output " default 
;;                   " --auto --primary")))))

;; Update exwm-randr-workspace-output-plist with two outputs named
;; 'default' and 'other'.  If the 'other' output is same as 'default'
;; then all workspaces will be redirected to the 'default' output.

;; (defun my-exwm-xrandr-config-bak (default &optional first-mon second-mon)
;;   (cond ((and default first-mon second-mon)
;;          (setq exwm-randr-workspace-output-plist '(0 "HDMI1" 1 "HDMI2" 2 "HDMI2" 3 "HDMI2" 4 "HDMI2" 5 "HDMI2" 6 "HDMI1" 7 "HDMI1" 8 "HDMI1" 9 "HDMI1")))
;;         ((and default first-mon)
;;          (setq exwm-randr-workspace-output-plist '(0 "eDP1" 1 "HDMI1" 2 "HDMI1" 3 "HDMI1" 4 "HDMI1" 5 "HDMI1" 6 "eDP1" 7 "eDP1" 8 "eDP1" 9 "eDP1")))
;;         ((stringp default)
;;          (setq exwm-randr-workspace-output-plist
;;                 (progn
;;                  (setq exwm-workspace-number 8)
;;                  (setq result ())
;;                  (setq index 1)
;;                  (while (<= index exwm-workspace-number)
;;                    (setq result (append result (list index (concat "\"" default "\"") )))
;;                    (setq index (1+ index)))
;;                  (exwm--log "cond 3 exwm-randr-workspace-output-plist: %s"
;;                             result))
;;                  result))))

;; (defun my-exwm-xrandr-config (default &optional first second)
;;   (cond ((and first second)
;;          (setq exwm-randr-workspace-output-plist
;;                (progn
;;                  (setq exwm-worspace-number 9)
;;                  (setq index 1)
;;                  (while (<= index 6)
;;                    (setq result (append result (list index (concat "\"" first "\""))))
;;                    (setq index (1+ index)))
;;                  (while (<= index exwm-workspace-number )
;;                    (setq result (append result (list index (concat "\"" second "\""))))
;;                    (setq index (1+ index)))
;;                result)))
;;         ((stringp first)
;;          (setq exwm-randr-workspace-output-plist 
;;                (progn
;;                  (setq exwm-workspace-number 10)
;;                  (setq index 1)
;;                  (while (< index (/ exwm-workspace-number 2))
;;                    (setq result (append result (list index default)))
;;                    (setq index (1+ index)))
;;                  (while (< index exwm-worspace-number)
;;                    (setq result (append result (list index first)))
;;                    (setq index (1+ index)))
;;                result))
;;         ((stringp default)
;;          (setq exwm-randr-workspace-output-plist
;;                (progn
;;                  (setq exwm-workspace-number 8)
;;                  (setq index 1)
;;                  (while (<= exwm-workspace-number)
;;                    (setq result (append result (list index default )))
;;                    (setq index (1+ index)))
;;                  result))))))

;; ;; Disable xrandr output named 'output'.
;; (defun my-exwm-xrandr-off (output)
;;   (if output (shell-command (concat "xrandr --output " output " --off"))))

;; Dynamically find the active xrandr outputs and update exwm
;; workspace configuration and enable xrandr outputs appropriately.
;; (defun my-exwm-xrandr-hook-old (default)
;;   (let* ((connected-cmd "xrandr -q|awk '/ connected/ {print $1}'")
;; 	 (connected (process-lines "bash" "-lc" connected-cmd))
;; 	 (previous (delete-dups (seq-remove
;; 				 'integerp
;; 				 exwm-randr-workspace-output-plist))))
;;     (cond ((member "HDMI1" connected)
;;            (member "HDMI2" connected)
;;            (progn (my-exwm-xrandr-config default "HDMI2" "HDMI1")
;; 		  (my-exwm-xrandr-outputs default "HDMI2" "normal" "HDMI1" "left")
;;                   (my-exwm-xrandr-off default)
;;                   (exwm--log "cond 1 connected: %s"
;;                             connected)
;;                   (exwm-randr-refresh)))
;;           ((member "HDMI1" connected)
;; 	   (progn (my-exwm-xrandr-config default "HDMI1")
;; 		  (my-exwm-xrandr-outputs default "HDMI1" "left")
;;                   (my-exwm-xrandr-off "HDMI2")
;;                   (exwm--log "cond 2 connected: %s"
;;                              connected)
;;                   (exwm-randr-refresh)))
;;           ((member "HDMI2" connected)
;; 	   (progn (my-exwm-xrandr-config default "HDMI2")
;; 		  (my-exwm-xrandr-outputs default "HDMI2" "normal")
;;                   (my-exwm-xrandr-off "HDMI1")
;;                   (exwm--log "cond 3 connected: %s"
;;                              connected)
;;                   (exwm-randr-refresh)))
;;           ((member "eDP1" connected)
;; 	   (progn (my-exwm-xrandr-config default)
;; 		  (my-exwm-xrandr-outputs default)
;;                   (my-exwm-xrandr-off "HDMI1")
;;                   (my-exwm-xrandr-off "HDMI2")
;;                   (exwm--log "cond 4 connected: %s"
;;                              connected)
;;                   (exwm-randr-refresh)))
;; 	  (t (progn (my-exwm-xrandr-config default default)
;;  		    (mapcar 'my-exwm-xrandr-off
;; 			    (delete default previous)))))))

;; (defun my-exwm-xrandr-hook (default)
;;   (let* ((connected-cmd "xrandr -q|awk '/ connected/ {print $1}'")
;; 	 (connected (process-lines "bash" "-lc" connected-cmd))
;; 	 (previous (delete-dups (seq-remove
;; 				 'integerp
;; 				 exwm-randr-workspace-output-plist))))
;;     (cond ((member "HDMI1" connected)
;;            (member "HDMI2" connected)
;; 	   (progn (my-exwm-xrandr-config default "HDMI2" "HDMI1")
;; 		  (my-exwm-xrandr-outputs default "HDMI2" "normal" "HDMI1" "left")))
;;           ((member "HDMI1" connected)
;; 	   (progn (my-exwm-xrandr-config default "HDMI1")
;; 		  (my-exwm-xrandr-outputs default "HDMI1" "left")))
;;           ((member "HDMI2" connected)
;; 	   (progn (my-exwm-xrandr-config default "HDMI2")
;; 		  (my-exwm-xrandr-outputs default "HDMI2" "normal")))
;;           ((member "eDP1" connected)
;; 	   (progn (my-exwm-xrandr-config default)
;; 		  (my-exwm-xrandr-outputs default)
;;                   (my-exwm-xrandr-off "HDMI1")
;;                   (my-exwm-xrandr-off "HDMI2"))
;; 	  (t (progn (my-exwm-xrandr-config default default)
;;  		    (mapcar 'my-exwm-xrandr-off
;; 			    (delete default previous)))))))

;; (setq exwm-randr-screen-change-hook
;;       (lambda () (my-exwm-xrandr-hook "eDP1")))

;; (exwm-randr-enable)

;; Turn on `display-time-mode' if you don't use an external bar.
(setq display-time-default-load-average nil)
(display-time-mode t)

;; disable annoying minibuffer click that is often misclicked while using systray
(define-key minibuffer-inactive-mode-map [mouse-1] #'ignore)

(fringe-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-dialog-box nil)
(setq left-margin-width 0)
(set-face-background 'fringe "black")
(setq exwm-systemtray-height 14)
(exwm-systemtray-enable)
(exwm-config-default)

;; (defun apps-at-startup ()
;;   "open default apps at startup"
;;   (interactive)
;;   (progn
;;     (exwm-workspace-switch-create 1)
;;     (exwm-workspace-switch-create 2)
;;     (start-process-shell-command "chromium" nil "/usr/bin/chromium")
;;     (exwm-workspace-switch-create 3)
;;     (sleep-for 5)
;;     (exwm-workspace-switch-create 3)
;;     (sane-term)
;;     (sleep-for 5)

;; ))

;; (add-hook 'exwm-init-hook #'apps-at-startup)

;;; init.el ends here

