;; Define package repositories
(require 'package)
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

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(
    dired+
    
    paredit

    cheatsheet

    rainbow-delimiters

    smex

    dash

    yaml-mode

    vagrant

    vagrant-tramp

    dracula-theme

    lua-mode

    elpy

    emamux

    emms

    racket-mode
    
    tagedit

    slime

    multiple-cursors

    ace-window

    markdown-preview-mode

    vagrant

    vagrant-tramp

    markdown-mode

    websocket

    org-wunderlist

    multi-term
    
    powerline

    flycheck

    free-keys
    
    google-translate

    magit

    magithub

    github-clone

    ;; git-auto-commit-mode

    json-mode

    ;; ido-ubiquitous
    ;; persp-mode

    powerline
    
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; start emacsclient maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; cheatsheet
(require 'cheatsheet)

;; emms
(require 'emms-setup)
(emms-standard)
(emms-default-players)

;; emamux
(require 'emamux)

;; yaml (salt)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))

;; powerline
(require 'powerline)
(powerline-default-theme)

;; rainbow-delimiters
(require 'rainbow-delimiters)

;; google-translate.el
(require 'google-translate)
(require 'google-translate-smooth-ui)

;; magithub
(require 'magithub)
(magithub-feature-autoinject t)

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Custom exec-path
(add-to-list 'exec-path "~/.bin")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("8577da1641ed4bdf255341ca92e3d0e49c9f4d574458f09ce78159690442cade" "a041a61c0387c57bb65150f002862ebcfe41135a3e3425268de24200b82d6ec9" default)))
 '(highlight-changes-colors ("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   (("#49483E" . 0)
    ("#67930F" . 20)
    ("#349B8D" . 30)
    ("#21889B" . 50)
    ("#968B26" . 60)
    ("#A45E0A" . 70)
    ("#A41F99" . 85)
    ("#49483E" . 100)))
 '(magit-diff-use-overlays nil)
 '(org-babel-load-languages (quote ((emacs-lisp . t) (sh . t) (python . t))))
 '(package-selected-packages
   (quote
    (web-mode vagrant-tramp vagrant powerline dired+ yaml-mode websocket smex rainbow-delimiters persp-mode paredit org-bullets multiple-cursors multi-term markdown-mode magit lua-mode load-theme-buffer-local emamux elpy dracula-theme curl-for-url ag ace-window)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; multi-term
(setq multi-term-program "/usr/bin/zsh")

;; tramp
(setq tramp-default-method "ssh")

;; elpy
(elpy-enable)

;; reuse dired buffer
(diredp-toggle-find-file-reuse-dir 1)

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
(setq auto-save-file-name-transforms
          `((".*" ,"~/.emacs.d/emacs-backup")))

;; org-mode
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-directory "~/.org" )
(setq org-agenda-files '("~/.org"))
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-confirm-babel-evaluate nil)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; git-auto-commit-mode
;;(require 'git-auto-commit-mode)
;;(auto load 'git-auto-commit-mode "git-auto-commit-mode")
;;(setq-default gac-automatically-push-p t)

;; reuse dired buffer
(diredp-toggle-find-file-reuse-dir 1)

;; emms
(require 'emms-setup)
(emms-standard)
(emms-default-players)

(require 'powerline) 
(powerline-default-theme) 

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))

;; org-wunderlist
;; (require 'org-wunderlist)
;; (setq org-wunderlist-client-id "125188bc7590560a9328"
;;       org-wunderlist-token "858656c6a5ddccc6a34f230381b360392b01e76391006ef6e9c38d4d5282"
;;       org-wunderlist-file  "~/.org/Wunderlist.org"
;;       org-wunderlist-dir "~/.org/org-wunderlist/")

;; org-capture
(setq org-directory "~/.org/")
(setq org-default-notes-file (concat org-directory "notes"))
(define-key global-map (kbd "M-N") 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/.org/wiki.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("n" "Notes" entry (file+headline "~/.org/wiki.org" "Notes")
         "* Notes %?\n %i\n $a")))


;; sudo-edit
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Flycheck global mode
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; google-translate.el
(require 'google-translate)
(require 'google-translate-smooth-ui)
(global-set-key "\C-ct" 'google-translate-smooth-translate)

