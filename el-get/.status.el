((el-get status "installed" recipe
         (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "master" :pkgname "dimitri/el-get" :info "." :compile
                ("el-get.*\\.el$" "methods/")
                :features el-get :post-init
                (when
                    (memq 'el-get
                          (bound-and-true-p package-activated-list))
                  (message "Deleting melpa bootstrap el-get")
                  (unless package--initialized
                    (package-initialize t))
                  (when
                      (package-installed-p 'el-get)
                    (let
                        ((feats
                          (delete-dups
                           (el-get-package-features
                            (el-get-elpa-package-directory 'el-get)))))
                      (el-get-elpa-delete-package 'el-get)
                      (dolist
                          (feat feats)
                        (unload-feature feat t))))
                  (require 'el-get))))
 (emacs-w3m status "installed" recipe
            (:name emacs-w3m :description "A simple Emacs interface to w3m" :type cvs :website "http://emacs-w3m.namazu.org/" :module "emacs-w3m" :url ":pserver:anonymous@cvs.namazu.org:/storage/cvsroot" :build
                   `(("autoconf")
                     ("./configure" ,(format "--with-emacs=%s" el-get-emacs))
                     ("make"))
                   :build/windows-nt
                   (("sh" "./autogen.sh")
                    ("sh" "./configure")
                    ("make"))
                   :info "doc"))
 (emms status "installed" recipe
       (:name emms :description "The Emacs Multimedia System" :type git :url "git://git.sv.gnu.org/emms.git" :info "doc" :load-path
              ("./lisp")
              :features emms-setup :build
              `(("mkdir" "-p" ,(expand-file-name
                                (format "%s/emms" user-emacs-directory)))
                ("make" ,(format "EMACS=%s" el-get-emacs)
                 ,(format "SITEFLAG=--no-site-file -L %s"
                          (shell-quote-argument
                           (el-get-package-directory "emacs-w3m")))
                 "autoloads" "lisp" "docs"))
              :build/berkeley-unix
              `(("mkdir" "-p" ,(expand-file-name
                                (format "%s/emms" user-emacs-directory)))
                ("gmake" ,(format "EMACS=%s" el-get-emacs)
                 ,(format "SITEFLAG=--no-site-file -L %s"
                          (shell-quote-argument
                           (el-get-package-directory "emacs-w3m")))
                 "autoloads" "lisp" "docs"))
              :depends emacs-w3m))
 (markdown-mode status "installed" recipe
                (:name markdown-mode :description "Major mode to edit Markdown files in Emacs" :website "http://jblevins.org/projects/markdown-mode/" :type git :url "git://jblevins.org/git/markdown-mode.git" :prepare
                       (add-to-list 'auto-mode-alist
                                    '("\\.\\(md\\|mdown\\|markdown\\)\\'" . markdown-mode))))
 (markdown-preview-mode status "installed" recipe
                        (:name markdown-preview-mode :description "Markdown preview mode with websocket.el" :type github :depends
                               (websocket markdown-mode)
                               :website "https://github.com/ancane/markdown-preview-mode.git" :pkgname "ancane/markdown-preview-mode"))
 (websocket status "installed" recipe
            (:name websocket :description "A websocket implementation in elisp, for emacs." :type github :pkgname "ahyatt/emacs-websocket")))
