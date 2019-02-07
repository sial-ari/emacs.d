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

(cheatsheet-add :group 'Org-mode
                :key "C-c s"
                :description "org-search-goto")

(cheatsheet-add :group 'Emamux
                :key "C-z"
                :description "prefix emamux:keymap")

(cheatsheet-add :group 'Google-Translate
                :key "M-j t"
                :description "google-translate-at-point")

(cheatsheet-add :group 'Google-Translate
                :key "M-j T"
                :description "google-translate-at-point-reverse")

(cheatsheet-add :group 'Ispell
                :key "M-j s"
                :description "ispell-word"
)
(provide 'setup-cheatsheet)
