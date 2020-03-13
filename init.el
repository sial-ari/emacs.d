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

;; no splash
(setq inhibit-splash-screen t)

;; start emacsclient maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; transparency
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (90 . 50)))

;; trust me
(setq disabled-command-function nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; decrease font size
(set-face-attribute 'default nil :height 110)

;; use office code pro font
(add-to-list 'default-frame-alist '(font . "Office Code Pro-9" ))
(set-face-attribute 'default t :font "Office Code Pro-9" )

(setq redisplay-dont-pause t)

;; Lines should be 80 chars.
(setq-default fill-column 80)


(require 'setup-packages)
(require 'setup-settings)
(require 'setup-keybindings)
(require 'setup-defuns)
(require 'setup-cheatsheet)

;;; init.el ends here
