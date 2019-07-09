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
          (concat "xrandr --output " other 
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
  (cond ((and first second)
         (setq exwm-randr-workspace-output-plist
               (progn
                 (setq exwm-worspace-number 9)
                 (setq index 1)
                 (while (<= index 3)
                   (setq result (append result (list index first)))
                   (setq index (1+ index)))
                 (while (<= index 6 )
                   (setq result (append result (list index second)))
                   (setq index (1+ index)))
                 (while (<= index exwm-workspace-number)
                   (setq result (append result (list index default)))
                   (setq index (1+ index)))
               result))
        ((stringp first)
         (setq exwm-randr-workspace-output-plist 
               (progn
                 (setq exwm-workspace-number 10)
                 (setq index 1)
                 (while (< index (/ exwm-workspace-number 2))
                   (setq result (append result (list index default)))
                   (setq index (1+ index)))
                 (while (< index exwm-worspace-number)
                   (setq result (append result (list index first)))
                   (setq index (1+ index)))
               result))
        ((stringp default)
         (setq exwm-randr-workspace-output-plist
               (progn
                 (setq exwm-workspace-number 8)
                 (setq index 1)
                 (while (<= exwm-workspace-number)
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
	   (progn (my-exwm-xrandr-config default "HDMI2" "HDMI1")
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

(defun switch-to-last-buffer ()
  "Switch to last open buffer in current window."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(exwm-input-set-key (kbd "s-<tab>") #'switch-to-last-buffer)

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
      (exwm-workspace-switch-create 2)
      (start-process-shell-command "chromium" nil "/usr/bin/chromium")
      (sleep-for 5)
      (exwm-workspace-switch-create 3)
      (sane-term)
      (sleep-for 5)

))

(add-hook 'exwm-init-hook #'apps-at-startup)

(provide 'setup-exwm)
