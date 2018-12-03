;; cheatsheet.el configuration
(cheatsheet-add :group 'Org-mode
                :key "C-c a"
                :description "org-agenda")

(cheatsheet-add :group 'Org-mode
                :key "C-c c"
                :description "org-capture")

(cheatsheet-add :group 'Org-mode
                :key "C-c C-v q"
                :description "org-babel-execute-src-block")

(cheatsheet-add :group 'Emamux
                :key "C-z"
                :description "prefix emamux:keymap")

(provide 'setup-cheatsheet)
