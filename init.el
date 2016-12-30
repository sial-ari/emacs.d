;;;;
;; Packages
;;;;

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
;;                         ("melpa" . "http://melpa-stable.milkbox.net/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")))


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
;; (el-get)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)


;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ;; ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile
    dash

    ;; ansible minor mode and deps
    ansible
    yasnippet
    auto-complete
    yaml-mode

    ;; ox-gfm
    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; themes
    calmer-forest-theme
    cyberpunk-theme

    ;; Lua Mode
    lua-mode

    ;; Python (elpy) Mode
    elpy

    ;; Python Django
    python-django
    
    ;; Emamux (emacs + tmux integration)
    emamux

    ;; Racket-mode
    racket-mode
    
    ;; edit html tags like sexps
    tagedit

    ;; add slime
    slime

    ;; add multiple-cursors
    multiple-cursors

    ;; add ace-window
    ace-window

    ;; vagrant tools
    vagrant
    vagrant-tramp

    ;; markdown
    markdown-mode
    websocket

    ;; perl - template toolkit
    tt-mode

    ;; Add: org plugins
    org-bullets
    org-wunderlist

    ;; add multi-term
    multi-term

    ;; add powerline
    powerline
    
    ;; add flycheck
    flycheck
    
    ;; add free-keys
    free-keys

    ;; git integration
    magit

    ;; google-translate
    google-translate
    
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")
;; Add custom elisp scripts to load path
(add-to-list 'load-path "~/.emacs.d/lisp")

(add-to-list 'exec-path "~/.bin")


;;;;
;; Customization
;;;;

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

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("a041a61c0387c57bb65150f002862ebcfe41135a3e3425268de24200b82d6ec9" default)))
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
 '(org-agenda-files (quote ("~/.org/infrastructure.org")))
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (sh . t)
     (clojure . t)
     (python . t)
     (ruby . t))))
 '(package-selected-packages
   (quote
    (dired+ yaml-mode websocket web-mode turnip tt-mode tagedit smex slime rainbow-delimiters racket-mode python-django puppetfile-mode puppet-mode projectile persp-mode paredit ox-gfm org-wunderlist org-bullets multiple-cursors multi-term monokai-theme moe-theme material-theme markdown-mode magit lua-mode load-theme-buffer-local isend-mode farmhouse-theme exec-path-from-shell emamux elpy dracula-theme cyberpunk-theme curl-for-url color-theme-monokai clojure-mode-extra-font-locking cl-lib-highlight cider calmer-forest-theme auto-complete ansible-doc ansible ag ace-window)))
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
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq multi-term-program "/usr/bin/zsh")
(setq org-confirm-babel-evaluate nil)
(setq tramp-default-method "ssh")
(setq inferior-lisp-program "/bin/clisp")
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")
(elpy-enable)

;; reuse dired buffer
(diredp-toggle-find-file-reuse-dir 1)

;; emms
(require 'emms-setup)
(emms-standard)
(emms-default-players)

;; python-django.el
(require 'python-django)

;; projectile
(require 'projectile)

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

