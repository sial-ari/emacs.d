;; Free personal key bindings space.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(global-unset-key (kbd "M-j"))
(setq org-src-tab-acts-natively t)
(setq help-window-select t)

(provide 'setup-settings)
