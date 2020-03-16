;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; emms d-bus notifications
;; choose D-Bus to disseminate messages, if it is running.
(cond
 ;; test to see if D-Bus notifications are available
 ((if (and (require 'dbus nil t)
	   (dbus-ping :session "org.freedesktop.Notifications"))
      (progn
	(setq notify-method 'notify-via-dbus-notifications)
	(require 'notifications))))
 ;; could use the message system otherwise
 (t (setq notify-method 'notify-via-message)))

(defun notify-via-notifications (title msg icon)
  "Send notification with TITLE, MSG via `D-Bus'."
  (notifications-notify
   :title title
   :body msg
   :app-icon icon
   :urgency 'low))

(defun notify-via-messages (title msg)
  "Send notification with TITLE, MSG to message."
  (message "APPOINTMENT: %s" msg))
