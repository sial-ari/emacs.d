;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "el-get/el-get" "el-get/el-get.el" (22165 22619
;;;;;;  774318 383000))
;;; Generated autoloads from el-get/el-get.el

(autoload 'el-get-version "el-get/el-get" "\
Message the current el-get version

\(fn)" t nil)

(autoload 'el-get-install "el-get/el-get" "\
Cause the named PACKAGE to be installed after all of its
dependencies (if any).

PACKAGE may be either a string or the corresponding symbol.

\(fn PACKAGE)" t nil)

(autoload 'el-get-update "el-get/el-get" "\
Update PACKAGE.

\(fn PACKAGE)" t nil)

(autoload 'el-get-update-all "el-get/el-get" "\
Performs update of all installed packages.

\(fn &optional NO-PROMPT)" t nil)

(autoload 'el-get-update-packages-of-type "el-get/el-get" "\
Update all installed packages of type TYPE.

\(fn TYPE)" t nil)

(autoload 'el-get-self-update "el-get/el-get" "\
Update el-get itself.  The standard recipe takes care of reloading the code.

\(fn)" t nil)

(autoload 'el-get-remove "el-get/el-get" "\
Remove any PACKAGE that is know to be installed or required.

\(fn PACKAGE)" t nil)

(autoload 'el-get-reinstall "el-get/el-get" "\
Remove PACKAGE and then install it again.

\(fn PACKAGE)" t nil)

(autoload 'el-get-cd "el-get/el-get" "\
Open dired in the package directory.

\(fn PACKAGE)" t nil)

(autoload 'el-get-make-recipes "el-get/el-get" "\
Loop over `el-get-sources' and write a recipe file for each
entry which is not a symbol and is not already a known recipe.

\(fn &optional DIR)" t nil)

(autoload 'el-get-checksum "el-get/el-get" "\
Compute the checksum of the given package, and put it in the kill-ring

\(fn PACKAGE)" t nil)

(autoload 'el-get-self-checksum "el-get/el-get" "\
Compute the checksum of the running version of el-get itself.

Also put the checksum in the kill-ring.

\(fn)" t nil)

(autoload 'el-get "el-get/el-get" "\
Ensure that packages have been downloaded once and init them as needed.

This will not update the sources by using `apt-get install' or
`git pull', but it will ensure that:

* the packages have been installed
* load-path is set so their elisp files can be found
* Info-directory-list is set so their info files can be found
* Autoloads have been prepared and evaluated for each package
* Any post-installation setup (e.g. `(require 'feature)') happens

When SYNC is nil (the default), all installations run
concurrently, in the background.

When SYNC is 'sync, each package will be installed synchronously,
and any error will stop it all.

Please note that the `el-get-init' part of `el-get' is always
done synchronously. There's `byte-compile' support though, and
the packages you use are welcome to use `autoload' too.

PACKAGES is expected to be a list of packages you want to install
or init.  When PACKAGES is omited (the default), the list of
already installed packages is considered.

\(fn &optional SYNC &rest PACKAGES)" nil nil)

;;;***

;;;### (autoloads nil "el-get/el-get-bundle" "el-get/el-get-bundle.el"
;;;;;;  (22165 22619 770318 396000))
;;; Generated autoloads from el-get/el-get-bundle.el

(autoload 'el-get-bundle-el-get "el-get/el-get-bundle" "\


\(fn SRC)" nil nil)

(autoload 'el-get-bundle "el-get/el-get-bundle" "\
Install PACKAGE and run initialization FORM.

PACKAGE can be either a simple package name or a package name
with a modifier before the name to specify local recipe source
information:

* `<owner>/' : specifies a Github owner name
* `gist:<id>' : specifies a Gist ID
* `<type>:' : specifies a type of the package source

If `FEATURE in PACKAGE' form is used instead of PACKAGE, then
that FEATURE is `require'd after installing PACKAGE.  You can
also use `el-get-bundle!' macro if FEATURE and PACKAGE are the
same.  If you wish to `require' more than one feature, then use
`:features' property in FORM.

The initialization FORM may start with a property list that
describes a local recipe.  The FORM after the property list is
treated as initialization code, which is actually an `:after'
property of the local recipe.

A copy of the initialization code is stored in a directory
specified by `el-get-bundle-init-directory' and its byte-compiled
version is used if `el-get-bundle-byte-compile' is non-nil.

\(fn PACKAGE &rest FORM)" nil t)

(put 'el-get-bundle 'lisp-indent-function 'defun)

(autoload 'el-get-bundle! "el-get/el-get-bundle" "\
Install PACKAGE and run initialization form.
It is the same as `el-get-bundle' except that PACKAGE is explicitly
required.

\(fn PACKAGE &rest ARGS)" nil t)

(put 'el-get-bundle! 'lisp-indent-function 'defun)

;;;***

;;;### (autoloads nil "el-get/el-get-check" "el-get/el-get-check.el"
;;;;;;  (22165 22619 770318 396000))
;;; Generated autoloads from el-get/el-get-check.el

(autoload 'el-get-check-recipe "el-get/el-get-check" "\
Check the format of the recipe.
Please run this command before sending a pull request.
Usage: M-x el-get-check-recipe RET

You can run this function from checker script like this:
    test/check-recipe.el PATH/TO/RECIPE.rcp

When used as a lisp function, FILE-OR-BUFFER must be a buffer
object or a file path.

\(fn FILE-OR-BUFFER)" t nil)

;;;***

;;;### (autoloads nil "el-get/el-get-list-packages" "el-get/el-get-list-packages.el"
;;;;;;  (22165 22619 771318 393000))
;;; Generated autoloads from el-get/el-get-list-packages.el

(autoload 'el-get-list-packages "el-get/el-get-list-packages" "\
Display a list of packages.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emacs-w3m/bookmark-w3m" "emacs-w3m/bookmark-w3m.el"
;;;;;;  (19648 52975 0 0))
;;; Generated autoloads from emacs-w3m/bookmark-w3m.el

(autoload 'bookmark-w3m-bookmark-jump "emacs-w3m/bookmark-w3m" "\
Default bookmark handler for w3m buffers.

\(fn BOOKMARK)" nil nil)

;;;***

;;;### (autoloads nil "emacs-w3m/mime-w3m" "emacs-w3m/mime-w3m.el"
;;;;;;  (20927 43680 0 0))
;;; Generated autoloads from emacs-w3m/mime-w3m.el

(autoload 'mime-w3m-preview-text/html "emacs-w3m/mime-w3m" "\


\(fn ENTITY SITUATION)" nil nil)

;;;***

;;;### (autoloads nil "emacs-w3m/octet" "emacs-w3m/octet.el" (21841
;;;;;;  20772 0 0))
;;; Generated autoloads from emacs-w3m/octet.el

(autoload 'octet-buffer "emacs-w3m/octet" "\
View octet-stream content according to `octet-type-filter-alist'.
Optional NAME is the filename.
If optional CONTENT-TYPE is specified, it is used for type guess.

\(fn &optional NAME CONTENT-TYPE)" t nil)

(autoload 'octet-find-file "emacs-w3m/octet" "\
Find FILE with octet-stream decoding.

\(fn FILE)" t nil)

(autoload 'mime-preview-octet "emacs-w3m/octet" "\
A method for mime-view to preview octet message.

\(fn ENTITY SITUATION)" nil nil)

(autoload 'mime-view-octet "emacs-w3m/octet" "\
A method for mime-view to display octet message.

\(fn ENTITY SITUATION)" nil nil)

(autoload 'octet-mime-setup "emacs-w3m/octet" "\
Octet setting for MIME module.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "emacs-w3m/w3m" "emacs-w3m/w3m.el" (22044 56486
;;;;;;  0 0))
;;; Generated autoloads from emacs-w3m/w3m.el

(autoload 'w3m-retrieve "emacs-w3m/w3m" "\
Retrieve web contents pointed to by URL.
It will put the retrieved contents into the current buffer.

If HANDLER is nil, this function will retrieve web contents, return
the content type of the retrieved data, and then come to an end.  This
behavior is what is called a synchronous operation.  You have to
specify HANDLER in order to make this function show its real ability,
which is called an asynchronous operation.

If HANDLER is a function, this function will come to an end in no time.
In this case, contents will be retrieved by the asynchronous process
after a while.  And after finishing retrieving contents successfully,
HANDLER will be called on the buffer where this function starts.  The
content type of the retrieved data will be passed to HANDLER as a
string argument.

NO-UNCOMPRESS specifies whether this function should not uncompress contents.
NO-CACHE specifies whether this function should not use cached contents.
POST-DATA and REFERER will be sent to the web server with a request.

\(fn URL &optional NO-UNCOMPRESS NO-CACHE POST-DATA REFERER HANDLER)" nil nil)

(autoload 'w3m-download "emacs-w3m/w3m" "\
Download contents of URL to a file named FILENAME.
NO-CHACHE (which the prefix argument gives when called interactively)
specifies not using the cached data.

\(fn &optional URL FILENAME NO-CACHE HANDLER POST-DATA)" t nil)

(autoload 'w3m-goto-url "emacs-w3m/w3m" "\
Visit World Wide Web pages.  This is the primitive function of `w3m'.
If the second argument RELOAD is non-nil, reload a content of URL.
Except that if it is 'redisplay, re-display the page without reloading.
The third argument CHARSET specifies a charset to be used for decoding
a content.
The fourth argument POST-DATA should be a string or a cons cell.
If it is a string, it makes this function request a body as if
the content-type is \"x-www-form-urlencoded\".  If it is a cons cell,
the car of a cell is used as the content-type and the cdr of a cell is
used as the body.
If the fifth argument REFERER is specified, it is used for a Referer:
field for this request.
The remaining HANDLER, ELEMENT[1], NO-POPUP, and SAVE-POS[2] are for
the internal operations of emacs-w3m.
You can also use \"quicksearch\" url schemes such as \"gg:emacs\" which
would search for the term \"emacs\" with the Google search engine.
See the `w3m-search' function and the variable `w3m-uri-replace-alist'.

Notes for the developers:
\[1] ELEMENT is a history element which has already been registered in
the `w3m-history-flat' variable.  It is corresponding to URL to be
retrieved at this time, not for the url of the current page.

\[2] SAVE-POS leads this function to save the current emacs-w3m window
configuration; i.e. to run `w3m-history-store-position'.
`w3m-history-store-position' should be called in a w3m-mode buffer, so
this will be convenient if a command that calls this function may be
invoked in other than a w3m-mode buffer.

\(fn URL &optional RELOAD CHARSET POST-DATA REFERER HANDLER ELEMENT NO-POPUP SAVE-POS)" t nil)

(autoload 'w3m-goto-url-new-session "emacs-w3m/w3m" "\
Visit World Wide Web pages in a new session.
If you invoke this command in the emacs-w3m buffer, the new session
will be created by copying the current session.  Otherwise, the new
session will start afresh.

\(fn URL &optional RELOAD CHARSET POST-DATA REFERER)" t nil)

(autoload 'w3m-gohome "emacs-w3m/w3m" "\
Go to the Home page.

\(fn)" t nil)

(autoload 'w3m-create-empty-session "emacs-w3m/w3m" "\
Create an empty page as a new session and visit it.

\(fn)" t nil)

(autoload 'w3m "emacs-w3m/w3m" "\
Visit World Wide Web pages using the external w3m command.

When you invoke this command interactively for the first time, it will
visit a page which is pointed to by a string like url around the
cursor position or the home page specified by the `w3m-home-page'
variable, but you will be prompted for a URL if `w3m-quick-start' is
nil (default t) or `w3m-home-page' is nil.

The variables `w3m-pop-up-windows' and `w3m-pop-up-frames' control
whether this command should pop to a window or a frame up for the
session.

When emacs-w3m sessions have already been opened, this command will
pop to the existing window or frame up, but if `w3m-quick-start' is
nil, (default t), you will be prompted for a URL (which defaults to
`popup' meaning to pop to an existing emacs-w3m buffer up).

In addition, if the prefix argument is given or you enter the empty
string for the prompt, this command will visit a url at the point, or
the home page the `w3m-home-page' variable specifies, or the \"about:\"
page.

You can also run this command in the batch mode as follows:

  emacs -f w3m http://emacs-w3m.namazu.org/ &

In that case, or if this command is called non-interactively, the
variables `w3m-pop-up-windows' and `w3m-pop-up-frames' will be ignored
\(treated as nil) and it will run emacs-w3m at the current (or the
initial) window.

If the optional NEW-SESSION is non-nil, this function makes a new
emacs-w3m buffer.  Besides that, it also makes a new emacs-w3m buffer
if `w3m-make-new-session' is non-nil and a user specifies a url string.

The optional INTERACTIVE-P is for the internal use; it is mainly used
to check whether Emacs 22 or later calls this function as an
interactive command in the batch mode.

\(fn &optional URL NEW-SESSION INTERACTIVE-P)" t nil)

(autoload 'w3m-browse-url "emacs-w3m/w3m" "\
Ask emacs-w3m to browse URL.
NEW-SESSION specifies whether to create a new emacs-w3m session.  URL
defaults to the string looking like a url around the cursor position.
Pop to a window or a frame up according to `w3m-pop-up-windows' and
`w3m-pop-up-frames'.

\(fn URL &optional NEW-SESSION)" t nil)

(autoload 'w3m-find-file "emacs-w3m/w3m" "\
Function used to open FILE whose name is expressed in ordinary format.
The file name will be converted into the file: scheme.

\(fn FILE)" t nil)

(autoload 'w3m-region "emacs-w3m/w3m" "\
Render the region of the current buffer between START and END.
URL specifies the address where the contents come from.  It can be
omitted or nil when the address is not identified.  CHARSET is used
for decoding the contents.  If it is nil, this function attempts to
parse the meta tag to extract the charset.

\(fn START END &optional URL CHARSET)" t nil)

(autoload 'w3m-buffer "emacs-w3m/w3m" "\
Render the current buffer.
See `w3m-region' for the optional arguments.

\(fn &optional URL CHARSET)" t nil)

;;;***

;;;### (autoloads nil "emacs-w3m/w3m-antenna" "emacs-w3m/w3m-antenna.el"
;;;;;;  (19187 58267 0 0))
;;; Generated autoloads from emacs-w3m/w3m-antenna.el

(autoload 'w3m-about-antenna "emacs-w3m/w3m-antenna" "\


\(fn URL &optional NO-DECODE NO-CACHE POST-DATA REFERER HANDLER)" nil nil)

(autoload 'w3m-antenna "emacs-w3m/w3m-antenna" "\
Report changes of WEB sites, which is specified in `w3m-antenna-sites'.

\(fn &optional NO-CACHE)" t nil)

;;;***

;;;### (autoloads nil "emacs-w3m/w3m-bookmark" "emacs-w3m/w3m-bookmark.el"
;;;;;;  (20597 64488 0 0))
;;; Generated autoloads from emacs-w3m/w3m-bookmark.el

(autoload 'w3m-bookmark-add-this-url "emacs-w3m/w3m-bookmark" "\
Add link under cursor to bookmark.

\(fn)" t nil)

(autoload 'w3m-bookmark-add-current-url "emacs-w3m/w3m-bookmark" "\
Add a url of the current page to the bookmark.
With prefix, ask for a new url instead of the present one.

\(fn &optional ARG)" t nil)

(autoload 'w3m-bookmark-add-all-urls "emacs-w3m/w3m-bookmark" "\
Add urls of all pages being visited to the bookmark.

\(fn)" t nil)

(autoload 'w3m-bookmark-add-current-url-group "emacs-w3m/w3m-bookmark" "\
Add link of the group of current urls to the bookmark.

\(fn)" t nil)

(autoload 'w3m-bookmark-view "emacs-w3m/w3m-bookmark" "\
Display the bookmark.

\(fn &optional RELOAD)" t nil)

(autoload 'w3m-bookmark-view-new-session "emacs-w3m/w3m-bookmark" "\
Display the bookmark on a new session.

\(fn &optional RELOAD)" t nil)

(autoload 'w3m-about-bookmark "emacs-w3m/w3m-bookmark" "\


\(fn &rest ARGS)" nil nil)

(autoload 'w3m-setup-bookmark-menu "emacs-w3m/w3m-bookmark" "\
Setup w3m bookmark items in menubar.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "emacs-w3m/w3m-cookie" "emacs-w3m/w3m-cookie.el"
;;;;;;  (19449 53703 0 0))
;;; Generated autoloads from emacs-w3m/w3m-cookie.el

(autoload 'w3m-cookie-shutdown "emacs-w3m/w3m-cookie" "\
Save cookies, and reset cookies' data.

\(fn)" t nil)

(autoload 'w3m-cookie-set "emacs-w3m/w3m-cookie" "\
Register cookies which correspond to URL.
BEG and END should be an HTTP response header region on current buffer.

\(fn URL BEG END)" nil nil)

(autoload 'w3m-cookie-get "emacs-w3m/w3m-cookie" "\
Get a cookie field string which corresponds to the URL.

\(fn URL)" nil nil)

(autoload 'w3m-cookie "emacs-w3m/w3m-cookie" "\
Display cookies and enable you to manage them.

\(fn &optional NO-CACHE)" t nil)

(autoload 'w3m-about-cookie "emacs-w3m/w3m-cookie" "\
Make the html contents to display and to enable you to manage cookies.

\(fn URL &optional NO-DECODE NO-CACHE POST-DATA &rest ARGS)" nil nil)

;;;***

;;;### (autoloads nil "emacs-w3m/w3m-dtree" "emacs-w3m/w3m-dtree.el"
;;;;;;  (18964 36505 0 0))
;;; Generated autoloads from emacs-w3m/w3m-dtree.el

(autoload 'w3m-about-dtree "emacs-w3m/w3m-dtree" "\


\(fn URL &optional NODECODE ALLFILES &rest ARGS)" nil nil)

(autoload 'w3m-dtree "emacs-w3m/w3m-dtree" "\
Display directory tree on local file system.
If called with 'prefix argument', display all directorys and files.

\(fn ALLFILES PATH)" t nil)

;;;***

;;;### (autoloads nil "emacs-w3m/w3m-fb" "emacs-w3m/w3m-fb.el" (17681
;;;;;;  2386 0 0))
;;; Generated autoloads from emacs-w3m/w3m-fb.el

(defvar w3m-fb-mode nil "\
Non-nil if W3m-Fb mode is enabled.
See the command `w3m-fb-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `w3m-fb-mode'.")

(custom-autoload 'w3m-fb-mode "emacs-w3m/w3m-fb" nil)

(autoload 'w3m-fb-mode "emacs-w3m/w3m-fb" "\
Toggle W3M Frame Buffer mode.
This allows frame-local lists of buffers (tabs).

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "emacs-w3m/w3m-filter" "emacs-w3m/w3m-filter.el"
;;;;;;  (21762 45261 0 0))
;;; Generated autoloads from emacs-w3m/w3m-filter.el

(autoload 'w3m-filter "emacs-w3m/w3m-filter" "\
Apply filtering rule of URL against a content in this buffer.

\(fn URL)" nil nil)

;;;***

;;;### (autoloads nil "emacs-w3m/w3m-form" "emacs-w3m/w3m-form.el"
;;;;;;  (22045 37908 0 0))
;;; Generated autoloads from emacs-w3m/w3m-form.el

(autoload 'w3m-fontify-forms "emacs-w3m/w3m-form" "\
Process half-dumped data and fontify forms in this buffer.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "emacs-w3m/w3m-lnum" "emacs-w3m/w3m-lnum.el"
;;;;;;  (21621 11507 0 0))
;;; Generated autoloads from emacs-w3m/w3m-lnum.el

(autoload 'w3m-lnum-mode "emacs-w3m/w3m-lnum" "\
Minor mode to extend point commands by using Conkeror style number selection.
With prefix ARG 0 disable battery included point functions, otherwise
enable them.  With no prefix ARG - toggle.

\(fn &optional ARG)" t nil)

(autoload 'w3m-lnum-goto "emacs-w3m/w3m-lnum" "\
Turn on link, image and form numbers and ask for one to go to.
0 corresponds to location url.

\(fn)" t nil)

(autoload 'w3m-lnum-follow "emacs-w3m/w3m-lnum" "\
Turn on link numbers, ask for one and execute appropriate action on it.
If link - visit it, when button - press, when input - activate it,
If image - toggle it.
With prefix ARG visit link in new session or don't move over
field/button/image on activation/push/toggle.
With `-' ARG, for link image - go to it and toggle it, if link,
visit in background.  With -4 ARG, for link image - toggle it.
With double prefix ARG, prompt for url to visit.
With triple prefix ARG, prompt for url to visit in new session.

\(fn ARG)" t nil)

(autoload 'w3m-lnum-universal "emacs-w3m/w3m-lnum" "\
Turn on link numbers, ask for one and offer actions over it depending on selection type.
Actions may be selected either by hitting corresponding key,
pressing <return> over the action line or left clicking.

\(fn)" t nil)

(autoload 'w3m-lnum-toggle-inline-image "emacs-w3m/w3m-lnum" "\
If image at point, toggle it.
Otherwise turn on link numbers and toggle selected image.
With prefix ARG open url under image in new session.
If no such url, move over image and toggle it.

\(fn &optional ARG)" t nil)

(autoload 'w3m-lnum-view-image "emacs-w3m/w3m-lnum" "\
Display the image under point in the external viewer.
If no image at poing, turn on image numbers and display selected.
The viewer is defined in `w3m-content-type-alist' for every type of an
image.

\(fn)" t nil)

(autoload 'w3m-lnum-save-image "emacs-w3m/w3m-lnum" "\
Save the image under point to a file.
If no image at poing, turn on image numbers and save selected.
The default name will be the original name of the image.

\(fn)" t nil)

(autoload 'w3m-lnum-external-view-this-url "emacs-w3m/w3m-lnum" "\
Launch the external browser and display the link at point.
If no link at point, turn on link numbers and open selected externally.

\(fn)" t nil)

(autoload 'w3m-lnum-edit-this-url "emacs-w3m/w3m-lnum" "\
Edit the page linked from the anchor under the cursor.
If no such, turn on link numbers and edit selected.

\(fn)" t nil)

(autoload 'w3m-lnum-print-this-url "emacs-w3m/w3m-lnum" "\
Display the url under point in the echo area and put it into `kill-ring'.
If no url under point, activate numbering and select one.

\(fn)" t nil)

(autoload 'w3m-lnum-download-this-url "emacs-w3m/w3m-lnum" "\
Download the file or the page pointed to by the link under point.
If no point, activate numbering and select andchor to download.

\(fn)" t nil)

(autoload 'w3m-lnum-bookmark-add-this-url "emacs-w3m/w3m-lnum" "\
Add link under cursor to bookmarks.
If no link under point, activate numbering and ask for one.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emacs-w3m/w3m-namazu" "emacs-w3m/w3m-namazu.el"
;;;;;;  (18964 36505 0 0))
;;; Generated autoloads from emacs-w3m/w3m-namazu.el

(autoload 'w3m-about-namazu "emacs-w3m/w3m-namazu" "\


\(fn URL &optional NO-DECODE NO-CACHE &rest ARGS)" nil nil)

(autoload 'w3m-namazu "emacs-w3m/w3m-namazu" "\
Search indexed files with Namazu.

\(fn INDEX QUERY &optional RELOAD)" t nil)

;;;***

;;;### (autoloads nil "emacs-w3m/w3m-perldoc" "emacs-w3m/w3m-perldoc.el"
;;;;;;  (18197 61165 0 0))
;;; Generated autoloads from emacs-w3m/w3m-perldoc.el

(autoload 'w3m-about-perldoc "emacs-w3m/w3m-perldoc" "\


\(fn URL &optional NO-DECODE NO-CACHE &rest ARGS)" nil nil)

(autoload 'w3m-perldoc "emacs-w3m/w3m-perldoc" "\
View Perl documents.

\(fn DOCNAME)" t nil)

;;;***

;;;### (autoloads nil "emacs-w3m/w3m-search" "emacs-w3m/w3m-search.el"
;;;;;;  (20697 1486 0 0))
;;; Generated autoloads from emacs-w3m/w3m-search.el

(autoload 'w3m-search "emacs-w3m/w3m-search" "\
Search QUERY using SEARCH-ENGINE.
When called interactively with a prefix argument, you can choose one of
the search engines defined in `w3m-search-engine-alist'.  Otherwise use
`w3m-search-default-engine'.
If Transient Mark mode, use the region as an initial string of query
and deactivate the mark.

\(fn SEARCH-ENGINE QUERY)" t nil)

(autoload 'w3m-search-new-session "emacs-w3m/w3m-search" "\
Like `w3m-search', but do the search in a new session.

\(fn SEARCH-ENGINE QUERY)" t nil)

(autoload 'w3m-search-uri-replace "emacs-w3m/w3m-search" "\
Generate query string for ENGINE from URI matched by last search.

\(fn URI ENGINE)" nil nil)

;;;***

;;;### (autoloads nil "emacs-w3m/w3m-session" "emacs-w3m/w3m-session.el"
;;;;;;  (20839 34704 0 0))
;;; Generated autoloads from emacs-w3m/w3m-session.el

(autoload 'w3m-session-save "emacs-w3m/w3m-session" "\
Save list of displayed session.

\(fn)" t nil)

(autoload 'w3m-session-crash-recovery-remove "emacs-w3m/w3m-session" "\
Remove crash recovery session set.

\(fn)" nil nil)

(autoload 'w3m-session-select "emacs-w3m/w3m-session" "\
Select session from session list.

\(fn)" t nil)

(autoload 'w3m-setup-session-menu "emacs-w3m/w3m-session" "\
Setup w3m session items in menubar.

\(fn)" nil nil)

(autoload 'w3m-session-last-autosave-session "emacs-w3m/w3m-session" "\


\(fn)" nil nil)

(autoload 'w3m-session-last-crashed-session "emacs-w3m/w3m-session" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "emacs-w3m/w3m-symbol" "emacs-w3m/w3m-symbol.el"
;;;;;;  (18791 2971 0 0))
;;; Generated autoloads from emacs-w3m/w3m-symbol.el

(autoload 'w3m-replace-symbol "emacs-w3m/w3m-symbol" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "emacs-w3m/w3m-weather" "emacs-w3m/w3m-weather.el"
;;;;;;  (20486 20998 0 0))
;;; Generated autoloads from emacs-w3m/w3m-weather.el

(autoload 'w3m-weather "emacs-w3m/w3m-weather" "\
Display weather report.

\(fn AREA)" t nil)

(autoload 'w3m-about-weather "emacs-w3m/w3m-weather" "\


\(fn URL NO-DECODE NO-CACHE POST-DATA REFERER HANDLER)" nil nil)

;;;***

;;;### (autoloads nil "emms/lisp/emms-cache" "emms/lisp/emms-cache.el"
;;;;;;  (22171 35399 475200 200000))
;;; Generated autoloads from emms/lisp/emms-cache.el

(autoload 'emms-cache-enable "emms/lisp/emms-cache" "\
Enable caching of Emms track data.

\(fn)" t nil)

(autoload 'emms-cache-disable "emms/lisp/emms-cache" "\
Disable caching of Emms track data.

\(fn)" t nil)

(autoload 'emms-cache-toggle "emms/lisp/emms-cache" "\
Toggle caching of Emms track data.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emms/lisp/emms-lyrics" "emms/lisp/emms-lyrics.el"
;;;;;;  (22171 35399 476200 198000))
;;; Generated autoloads from emms/lisp/emms-lyrics.el

(autoload 'emms-lyrics-enable "emms/lisp/emms-lyrics" "\
Enable displaying emms lyrics.

\(fn)" t nil)

(autoload 'emms-lyrics-disable "emms/lisp/emms-lyrics" "\
Disable displaying emms lyrics.

\(fn)" t nil)

(autoload 'emms-lyrics-toggle "emms/lisp/emms-lyrics" "\
Toggle displaying emms lyrics.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emms/lisp/emms-mode-line" "emms/lisp/emms-mode-line.el"
;;;;;;  (22171 35399 477200 197000))
;;; Generated autoloads from emms/lisp/emms-mode-line.el

(autoload 'emms-mode-line-enable "emms/lisp/emms-mode-line" "\
Turn on `emms-mode-line'.

\(fn)" t nil)

(autoload 'emms-mode-line-disable "emms/lisp/emms-mode-line" "\
Turn off `emms-mode-line'.

\(fn)" t nil)

(autoload 'emms-mode-line-toggle "emms/lisp/emms-mode-line" "\
Toggle `emms-mode-line'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emms/lisp/emms-player-mpd" "emms/lisp/emms-player-mpd.el"
;;;;;;  (22171 35399 477200 197000))
;;; Generated autoloads from emms/lisp/emms-player-mpd.el

(autoload 'emms-player-mpd-clear "emms/lisp/emms-player-mpd" "\
Clear the MusicPD playlist.

\(fn)" t nil)

(autoload 'emms-player-mpd-connect "emms/lisp/emms-player-mpd" "\
Connect to MusicPD and retrieve its current playlist.

Afterward, the status of MusicPD will be tracked.

This also has the effect of changing the current EMMS playlist to
be the same as the current MusicPD playlist.  Thus, this
function is useful to call if the contents of the EMMS playlist
buffer get out-of-sync for some reason.

\(fn)" t nil)

(autoload 'emms-player-mpd-show "emms/lisp/emms-player-mpd" "\
Describe the current EMMS track in the minibuffer.

If INSERTP is non-nil, insert the description into the current
buffer instead.

If CALLBACK is a function, call it with the current buffer and
description as arguments instead of displaying the description or
inserting it.

This function uses `emms-show-format' to format the current track.
It differs from `emms-show' in that it asks MusicPD for the current track,
rather than EMMS.

\(fn &optional INSERTP CALLBACK)" t nil)

;;;***

;;;### (autoloads nil "emms/lisp/emms-playing-time" "emms/lisp/emms-playing-time.el"
;;;;;;  (22171 35399 478200 196000))
;;; Generated autoloads from emms/lisp/emms-playing-time.el

(autoload 'emms-playing-time-enable-display "emms/lisp/emms-playing-time" "\
Display playing time on mode line.

\(fn)" t nil)

(autoload 'emms-playing-time-disable-display "emms/lisp/emms-playing-time" "\
Remove playing time from mode line.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emms/lisp/emms-playlist-limit" "emms/lisp/emms-playlist-limit.el"
;;;;;;  (22171 35399 478200 196000))
;;; Generated autoloads from emms/lisp/emms-playlist-limit.el

(autoload 'emms-playlist-limit-enable "emms/lisp/emms-playlist-limit" "\
Turn on emms playlist limit.

\(fn)" t nil)

(autoload 'emms-playlist-limit-disable "emms/lisp/emms-playlist-limit" "\
Turn off emms playlist limit.

\(fn)" t nil)

(autoload 'emms-playlist-limit-toggle "emms/lisp/emms-playlist-limit" "\
Toggle emms playlist limit.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emms/lisp/emms-playlist-mode" "emms/lisp/emms-playlist-mode.el"
;;;;;;  (22171 35399 478200 196000))
;;; Generated autoloads from emms/lisp/emms-playlist-mode.el

(autoload 'emms-playlist-mode "emms/lisp/emms-playlist-mode" "\
A major mode for Emms playlists.
\\{emms-playlist-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emms/lisp/emms-score" "emms/lisp/emms-score.el"
;;;;;;  (22171 35399 479200 194000))
;;; Generated autoloads from emms/lisp/emms-score.el

(autoload 'emms-score-enable "emms/lisp/emms-score" "\
Turn on emms-score.

\(fn)" t nil)

(autoload 'emms-score-disable "emms/lisp/emms-score" "\
Turn off emms-score.

\(fn)" t nil)

(autoload 'emms-score-toggle "emms/lisp/emms-score" "\
Toggle emms-score.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emms/lisp/emms-setup" "emms/lisp/emms-setup.el"
;;;;;;  (22171 35399 479200 194000))
;;; Generated autoloads from emms/lisp/emms-setup.el

(autoload 'emms-minimalistic "emms/lisp/emms-setup" "\
An Emms setup script.
Invisible playlists and all the basics for playing media.

\(fn)" nil nil)

(autoload 'emms-all "emms/lisp/emms-setup" "\
An Emms setup script.
Everything included in the `emms-minimalistic' setup and adds all
the stable features which come with the Emms distribution.

\(fn)" nil nil)

(autoload 'emms-default-players "emms/lisp/emms-setup" "\
Set `emms-player-list' to `emms-setup-default-player-list'.

\(fn)" nil nil)

(autoload 'emms-devel "emms/lisp/emms-setup" "\


\(fn)" nil nil)

(autoload 'emms-standard "emms/lisp/emms-setup" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "emms/lisp/emms-source-file" "emms/lisp/emms-source-file.el"
;;;;;;  (22171 35399 479200 194000))
;;; Generated autoloads from emms/lisp/emms-source-file.el
 (autoload 'emms-play-file "emms-source-file" nil t)
 (autoload 'emms-add-file "emms-source-file" nil t)
 (autoload 'emms-play-directory "emms-source-file" nil t)
 (autoload 'emms-add-directory "emms-source-file" nil t)
 (autoload 'emms-play-directory-tree "emms-source-file" nil t)
 (autoload 'emms-add-directory-tree "emms-source-file" nil t)
 (autoload 'emms-play-find "emms-source-file" nil t)
 (autoload 'emms-add-find "emms-source-file" nil t)
 (autoload 'emms-play-dired "emms-source-file" nil t)
 (autoload 'emms-add-dired "emms-source-file" nil t)

(autoload 'emms-source-file-directory-tree "emms/lisp/emms-source-file" "\
Return a list of all files under DIR that match REGEX.
This function uses `emms-source-file-directory-tree-function'.

\(fn DIR REGEX)" nil nil)

(autoload 'emms-source-file-regex "emms/lisp/emms-source-file" "\
Return a regexp that matches everything any player (that supports
files) can play.

\(fn)" nil nil)

(autoload 'emms-locate "emms/lisp/emms-source-file" "\
Search for REGEXP and display the results in a locate buffer

\(fn REGEXP)" t nil)
 (autoload 'emms-play-url "emms-source-file" nil t)
 (autoload 'emms-add-url "emms-source-file" nil t)
 (autoload 'emms-play-streamlist "emms-source-file" nil t)
 (autoload 'emms-add-streamlist "emms-source-file" nil t)

;;;***

;;;### (autoloads nil "emms/lisp/emms-source-playlist" "emms/lisp/emms-source-playlist.el"
;;;;;;  (22171 35399 479200 194000))
;;; Generated autoloads from emms/lisp/emms-source-playlist.el
 (autoload 'emms-play-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-native-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-native-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-m3u-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-m3u-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-pls-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-pls-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-playlist-file "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist-file "emms-source-playlist" nil t)
 (autoload 'emms-play-playlist-directory
          "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist-directory
          "emms-source-playlist" nil t)
 (autoload 'emms-play-playlist-directory-tree
          "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist-directory-tree
          "emms-source-file" nil t)

;;;***

;;;### (autoloads nil "emms/lisp/emms-streams" "emms/lisp/emms-streams.el"
;;;;;;  (22171 35399 480200 193000))
;;; Generated autoloads from emms/lisp/emms-streams.el

(autoload 'emms-streams "emms/lisp/emms-streams" "\
Opens the EMMS Streams interface.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emms/lisp/emms-volume" "emms/lisp/emms-volume.el"
;;;;;;  (22171 35399 480200 193000))
;;; Generated autoloads from emms/lisp/emms-volume.el

(autoload 'emms-volume-raise "emms/lisp/emms-volume" "\
Raise the speaker volume.

\(fn)" t nil)

(autoload 'emms-volume-lower "emms/lisp/emms-volume" "\
Lower the speaker volume.

\(fn)" t nil)

(autoload 'emms-volume-mode-plus "emms/lisp/emms-volume" "\
Raise volume and enable or extend the `emms-volume-minor-mode' timeout.

\(fn)" t nil)

(autoload 'emms-volume-mode-minus "emms/lisp/emms-volume" "\
Lower volume and enable or extend the `emms-volume-minor-mode' timeout.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emms/lisp/emms-volume-amixer" "emms/lisp/emms-volume-amixer.el"
;;;;;;  (22171 35399 480200 193000))
;;; Generated autoloads from emms/lisp/emms-volume-amixer.el

(autoload 'emms-volume-amixer-change "emms/lisp/emms-volume-amixer" "\
Change amixer master volume by AMOUNT.

\(fn AMOUNT)" nil nil)

;;;***

;;;### (autoloads nil "emms/lisp/emms-volume-pulse" "emms/lisp/emms-volume-pulse.el"
;;;;;;  (22171 35399 480200 193000))
;;; Generated autoloads from emms/lisp/emms-volume-pulse.el

(autoload 'emms-volume-pulse-change "emms/lisp/emms-volume-pulse" "\
Change amixer master volume by AMOUNT.

\(fn AMOUNT)" nil nil)

;;;***

;;;### (autoloads nil "emms/lisp/tq" "emms/lisp/tq.el" (22171 35399
;;;;;;  481200 192000))
;;; Generated autoloads from emms/lisp/tq.el

(autoload 'tq-create "emms/lisp/tq" "\
Create and return a transaction queue communicating with PROCESS.
PROCESS should be a subprocess capable of sending and receiving
streams of bytes.  It may be a local process, or it may be connected
to a tcp server on another machine.

\(fn PROCESS)" nil nil)

;;;***

;;;### (autoloads nil "markdown-mode/markdown-mode" "markdown-mode/markdown-mode.el"
;;;;;;  (22165 22682 127125 844000))
;;; Generated autoloads from markdown-mode/markdown-mode.el

(autoload 'markdown-mode "markdown-mode/markdown-mode" "\
Major mode for editing Markdown files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode/markdown-mode" "\
Major mode for editing GitHub Flavored Markdown files.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "markdown-preview-mode/markdown-preview-mode"
;;;;;;  "markdown-preview-mode/markdown-preview-mode.el" (22165 22685
;;;;;;  212116 317000))
;;; Generated autoloads from markdown-preview-mode/markdown-preview-mode.el

(autoload 'markdown-preview-open-browser "markdown-preview-mode/markdown-preview-mode" "\
Open the `markdown-preview' in the browser.

\(fn)" t nil)

(autoload 'markdown-preview-cleanup "markdown-preview-mode/markdown-preview-mode" "\
Cleanup `markdown-preview' mode.

\(fn)" t nil)

(autoload 'markdown-preview-mode "markdown-preview-mode/markdown-preview-mode" "\
Markdown preview mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("el-get/el-get-autoloading.el" "el-get/el-get-build.el"
;;;;;;  "el-get/el-get-byte-compile.el" "el-get/el-get-core.el" "el-get/el-get-custom.el"
;;;;;;  "el-get/el-get-dependencies.el" "el-get/el-get-install.el"
;;;;;;  "el-get/el-get-methods.el" "el-get/el-get-notify.el" "el-get/el-get-recipes.el"
;;;;;;  "el-get/el-get-status.el" "emacs-w3m/mew-w3m.el" "emacs-w3m/w3m-bug.el"
;;;;;;  "emacs-w3m/w3m-ccl.el" "emacs-w3m/w3m-ems.el" "emacs-w3m/w3m-favicon.el"
;;;;;;  "emacs-w3m/w3m-hist.el" "emacs-w3m/w3m-image.el" "emacs-w3m/w3m-load.el"
;;;;;;  "emacs-w3m/w3m-mail.el" "emacs-w3m/w3m-proc.el" "emacs-w3m/w3m-rss.el"
;;;;;;  "emacs-w3m/w3m-save.el" "emacs-w3m/w3m-tabmenu.el" "emacs-w3m/w3m-ucs.el"
;;;;;;  "emacs-w3m/w3m-util.el" "emacs-w3m/w3m-xmas.el" "emacs-w3m/w3mhack.el"
;;;;;;  "emms/lisp/emms-auto.el" "emms/lisp/emms-bookmarks.el" "emms/lisp/emms-browser.el"
;;;;;;  "emms/lisp/emms-compat.el" "emms/lisp/emms-cue.el" "emms/lisp/emms-history.el"
;;;;;;  "emms/lisp/emms-i18n.el" "emms/lisp/emms-info-libtag.el"
;;;;;;  "emms/lisp/emms-info-metaflac.el" "emms/lisp/emms-info-mp3info.el"
;;;;;;  "emms/lisp/emms-info-ogginfo.el" "emms/lisp/emms-info.el"
;;;;;;  "emms/lisp/emms-last-played.el" "emms/lisp/emms-librefm-scrobbler.el"
;;;;;;  "emms/lisp/emms-librefm-stream.el" "emms/lisp/emms-maint.el"
;;;;;;  "emms/lisp/emms-mark.el" "emms/lisp/emms-metaplaylist-mode.el"
;;;;;;  "emms/lisp/emms-mode-line-icon.el" "emms/lisp/emms-player-mpg321-remote.el"
;;;;;;  "emms/lisp/emms-player-mplayer.el" "emms/lisp/emms-player-simple.el"
;;;;;;  "emms/lisp/emms-player-vlc.el" "emms/lisp/emms-player-xine.el"
;;;;;;  "emms/lisp/emms-playlist-sort.el" "emms/lisp/emms-stream-info.el"
;;;;;;  "emms/lisp/emms-tag-editor.el" "emms/lisp/emms-url.el" "emms/lisp/emms.el"
;;;;;;  "emms/lisp/jack.el" "emms/lisp/later-do.el" "websocket/websocket-functional-test.el"
;;;;;;  "websocket/websocket-test.el" "websocket/websocket.el") (22171
;;;;;;  35406 352085 380000))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
