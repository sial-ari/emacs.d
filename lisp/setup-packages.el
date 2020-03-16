(require 'cl)
(require 'package)

;; Avoid the "loaded old bytecode instead of newer source" pitfall.
(setq load-prefer-newer t)

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
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

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

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

;; load use-package
(require 'use-package)
(setq use-package-verbose t)

;; load use-package-el-get
(require 'use-package-el-get)
(setq use-package-always-ensure nil)
(use-package-el-get-setup)


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

;; show vertical indentation lines
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

;; easy navigation without modifier keys
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
    (diminish 'anzu-mode)
    (diminish 'super-save-mode)
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

(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'setup-packages)
