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

