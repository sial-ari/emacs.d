
;; start emacsclient maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(require 'package)

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
    magit
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


;; emms
;; (require 'emms-setup)
;; (emms-standard)
;; (emms-default-players)

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
;;(require 'magithub)
;;(magithub-feature-autoinject t)

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Custom exec-path
(add-to-list 'exec-path "~/.bin")

(org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))

;; multi-term
(setq multi-term-program "/usr/bin/zsh")

;; tramp
(setq tramp-default-method "ssh")

;; elpy
(elpy-enable)

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
      browse-url-generic-program "chromium")
(defun org-babel-tangle-block()
  (interactive)
  (let ((current-prefix-arg '(4)))
     (call-interactively 'org-babel-tangle)
))
(put 'narrow-to-region 'disabled nil)
;; org-capture
(setq org-directory "~/.org/")
(setq org-default-notes-file (concat org-directory "notes"))
(define-key global-map (kbd "M-N") 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/.org/todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal entry" plain (file+datetree+prompt "~/.org/journal.org")
         "%K - %a\n%i\n%?\n")
        ("n" "Notes" entry (file+headline "~/.org/notes.org" "Notes")
         "* Notes %?\n %i\n $a")))

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "C-c b") 'org-babel-tangle-block)
))

;; git-auto-commit-mode
;;(require 'git-auto-commit-mode)
;;(auto load 'git-auto-commit-mode "git-auto-commit-mode")
;;(setq-default gac-automatically-push-p t)

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

;; Flycheck global mode
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; google-translate.el
(require 'google-translate-smooth-ui)
(global-set-key "\C-ct" 'google-translate-smooth-translate)

;; enable globally pyenv-mode
(add-to-list 'exec-path "~/.pyenv/shims")
(setenv "WORKON_HOME" "~/.pyenv/versions/")
(pyenv-mode)

;; enable helm
;; (helm-mode 1)

;; transparency
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))

;; ace-window
(setq aw-scope 'frame)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; gpg
(require 'epa-file)
(epa-file-enable)
(setq epg-gpg-program "gpg2")
(setq epa-pinentry-mode 'loopback)
(setenv "GPG_AGENT_INFO" nil)
(pinentry-start)

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; Set up load path.(shamelessly stolen from https://github.com/ivo-)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'setup-settings)
(require 'setup-keybindings)
(require 'setup-packages)
(require 'setup-defuns)
(require 'setup-cheatsheet)

;; exwm
(require 'exwm)
(require 'exwm-systemtray)
(require 'exwm-config)
(require 'exwm-randr)

;; Enable 2/3 monitor home setup with orientation
(defun my-exwm-xrandr-outputs (default &optional first orientation_first second orientation_second)
  (cond ((and first orientation_first second orientation_second) 
         (shell-command
          (concat "xrandr --output " default
                  " --auto --right-of " first
                  " --output " first " --auto --primary --rotate " orientation_first
                  " --right-of " second
                  " --output " second " --auto --rotate " orientation_second )))
        ((and first orientation_first) 
         (shell-command
          (concat "xrandr --output " first 
                  " --left-of " default " --auto " 
                  " --rotate " orientation_first)))
        ((stringp default)
         (shell-command
          (concat "xrandr --output " default 
                  " --auto --primary")))))

;; Update exwm-randr-workspace-output-plist with two outputs named
;; 'default' and 'other'.  If the 'other' output is same as 'default'
;; then all workspaces will be redirected to the 'default' output.

(defun my-exwm-xrandr-config (default &optional first second)
  (cond ((and default first second)
         (setq exwm-randr-workspace-output-plist
               (progn
                 (setq exwm-workspace-number 10)
                 (setq result ())
                 (setq index 1)
                 (while (<= index 4)
                   (setq result (append result (list index first)))
                   (setq index (1+ index)))
                 (while (<= index 8)
                   (setq result (append result (list index second)))
                   (setq index (1+ index)))
                 (while (<= index exwm-workspace-number)
                   (setq result (append result (list index default)))
                   (setq index (1+ index)))
                 (setq result (list 0 default))
               result)))
        ((and default first)
         (setq exwm-randr-workspace-output-plist 
               (progn
                 (setq exwm-workspace-number 10)
                 (setq result ())
                 (setq index 1)
                 (while (< index (/ exwm-workspace-number 2))
                   (setq result (append result (list index default)))
                   (setq index (1+ index)))
                 (while (< index exwm-workspace-number)
                   (setq result (append result (list index first)))
                   (setq index (1+ index)))
               result)))
        ((stringp default)
         (setq exwm-randr-workspace-output-plist
                (progn
                 (setq exwm-workspace-number 8)
                 (setq result ())
                 (setq index 1)
                 (while (<= index exwm-workspace-number)
                   (setq result (append result (list index default )))
                   (setq index (1+ index)))
                 result)))))

;; Disable xrandr output named 'output'.
(defun my-exwm-xrandr-off (output)
  (if output (shell-command (concat "xrandr --output " output " --off"))))

;; Dynamically find the active xrandr outputs and update exwm
;; workspace configuration and enable xrandr outputs appropriately.
(defun my-exwm-xrandr-hook (default)
  (let* ((connected-cmd "xrandr -q|awk '/ connected/ {print $1}'")
	 (connected (process-lines "bash" "-lc" connected-cmd))
	 (previous (delete-dups (seq-remove
				 'integerp
				 exwm-randr-workspace-output-plist))))
    (cond ((member "HDMI1" connected)
           (member "HDMI2" connected)
	   (progn (my-exwm-xrandr-config default "HDMI1" "HDMI2")
		  (my-exwm-xrandr-outputs default "HDMI2" "normal" "HDMI1" "left")))
          ((member "HDMI1" connected)
	   (progn (my-exwm-xrandr-config default "HDMI1")
		  (my-exwm-xrandr-outputs default "HDMI1" "left")))
          ((member "HDMI2" connected)
	   (progn (my-exwm-xrandr-config default "HDMI2")
		  (my-exwm-xrandr-outputs default "HDMI2" "normal")))
          ((member "eDP1" connected)
	   (progn (my-exwm-xrandr-config default)
		  (my-exwm-xrandr-outputs default)))
	  (t (progn (my-exwm-xrandr-config default default)
 		    (mapcar 'my-exwm-xrandr-off
			    (delete default previous)))))))

(setq exwm-randr-screen-change-hook
      (lambda () (my-exwm-xrandr-hook "eDP1")))

(exwm-randr-enable)

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
(setq exwm-systemtray-height 13)
(exwm-systemtray-enable)
(exwm-config-default)

(defun apps-at-startup ()
  "open default apps at startup"
  (interactive)
  (progn
    (exwm-workspace-switch-create 1)
    (exwm-workspace-switch-create 2)
    (start-process-shell-command "chromium" nil "/usr/bin/chromium")
    (exwm-workspace-switch-create 3)
    (sleep-for 5)
    (exwm-workspace-switch-create 3)
    (sane-term)
    (sleep-for 5)

))

(add-hook 'exwm-init-hook #'apps-at-startup)
