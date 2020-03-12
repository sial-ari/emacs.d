;;; setup-keybindings.el --- various keybindings for emacs
;;; Commentary:
;;  Emacs keybinding file --- keybindings


;;; Code:

;; Defaults
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Regexp search by default.
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Paragraph commands are by far more useful than the senctence commands.
(global-set-key (kbd "M-e") 'forward-paragraph)
(global-set-key (kbd "M-a") 'backward-paragraph)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)

(global-set-key (kbd "M-P") 'move-line-up)
(global-set-key (kbd "M-N") 'move-line-down)
(global-set-key (kbd "M-K") 'kill-whole-line)
(global-set-key (kbd "M-L") 'duplicate-current-line-or-region)

;; =============================================================================
;; M-j space
(global-set-key (kbd "M-j SPC") 'just-one-space)
(global-set-key (kbd "M-j M-SPC") 'delete-blank-lines)

(global-set-key (kbd "M-j q") 'generalized-shell-command)
(global-set-key (kbd "M-j M-q") 'shell-command-file)

;;(global-set-key (kbd "M-j e") 'eval-and-replace)
;;(global-set-key (kbd "M-j M-e") 'eval-and-replace)

(global-set-key (kbd "M-j l") 'join-line-or-lines-in-region)
(global-set-key (kbd "M-j M-l") 'join-line-or-lines-in-region)

(global-set-key (kbd "M-j f o") 'open-with)
(global-set-key (kbd "M-j f f") 'fzf-directory)
(global-set-key (kbd "M-j f e") 'sudo-edit)
(global-set-key (kbd "M-j f d") 'delete-buffer-file)
(global-set-key (kbd "M-j f r") 'rename-buffer-file)
(global-set-key (kbd "M-j f c") 'copy-file-name-to-clipboard)

(global-set-key (kbd "M-j b n") 'create-buffer)
(global-set-key (kbd "M-j b i") 'cleanup-buffer)

(global-set-key (kbd "M-j c") 'comment-region)
(global-set-key (kbd "M-j u") 'uncomment-region)
(global-set-key (kbd "M-j a") 'ag)
(global-set-key (kbd "M-j RET") 'switch-to-previous-buffer)

(global-set-key (kbd "M-j C") (lambda() (interactive)(find-file "~/.orgfiles/cheatsheets.org")))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-x C-r") 'sudo-edit)

(global-set-key (kbd "C-c t")  'google-translate-smooth-translate)

(global-set-key (kbd "C-c C-v q")  'org-babel-execute-src-block)

(global-set-key "\C-ca" 'org-agenda)

(define-key global-map "\C-cc" 'org-capture)

(global-unset-key (kbd "ESC ESC ESC"))

;; multi-term
(define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)
(define-key term-mode-map (kbd "C-c C-j") 'term-char-mode)
;; (global-set-key (kbd "M-j m") 'multi-term)
(global-set-key (kbd "M-j m") 'sial/local-term)

;; unused

;; (global-set-key (kbd "M-j m") 'simple-mpc-view-current-playlist)
;; (global-set-key (kbd "M-j e r") 'emamux:send-region)
;; (global-set-key (kbd "C-.") emamux:keymap)
;; (global-set-key (kbd "C-S-o") 'kill-other-buffer-and-window)
;; (global-set-key (kbd "C-o") '(lambda () (interactive) (delete-other-windows)))

;; (global-set-key (kbd "M-j `") 'hide-eshell)
;; (global-set-key (kbd "M-j 1") 'toggle-eshell-1)
;; (global-set-key (kbd "M-j 2") 'toggle-eshell-2)
;; (global-set-key (kbd "M-j 3") 'toggle-eshell-3)
;; (global-set-key (kbd "M-j 4") 'toggle-eshell-4)

;; (global-set-key (kbd "M-j M-`") 'hide-eshell)
;; (global-set-key (kbd "M-j M-1") 'toggle-eshell-1)
;; (global-set-key (kbd "M-j M-2") 'toggle-eshell-2)
;; (global-set-key (kbd "M-j M-3") 'toggle-eshell-3)
;; (global-set-key (kbd "M-j M-4") 'toggle-eshell-4)

;; (add-hook 'term-mode-hook
;;           (lambda ()
;;             (add-to-list 'term-bind-key-alist '("C-c C-e" . term-send-esc))
;;             (add-to-list 'term-bind-key-alist '("C-c C-k" . term-send-kill-line))
;;             (add-to-list 'term-bind-key-alist '("A-a" . term-send-tmux-prefix))
;;             (define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)
;;             (define-key term-mode-map (kbd "C-c C-j") 'term-char-mode)
;;             (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
;;             (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))))

;; dired
(define-key dired-mode-map (kbd "C-p") 'diredp-previous-line)

(provide 'setup-keybindings)
;;; setup-keybindings.el ends here
