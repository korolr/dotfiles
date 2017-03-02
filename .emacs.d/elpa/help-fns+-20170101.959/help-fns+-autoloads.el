;;; help-fns+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "help-fns+" "help-fns+.el" (22688 13566 139979
;;;;;;  513000))
;;; Generated autoloads from help-fns+.el

(autoload 'describe-command "help-fns+" "\
Describe an Emacs command (interactive function).
Equivalent to using a prefix arg with `describe-function'.

If you use Icicles then in Icicle mode keys bound to the commands are
shown next to them in `*Completions*.  You can toggle this keys
display on/off using `C-x C-a'.

\(fn FUNCTION)" t nil)

(autoload 'describe-option "help-fns+" "\
Describe an Emacs user variable (option).
Same as using a prefix arg with `describe-variable'.

\(fn VARIABLE &optional BUFFER)" t nil)

(autoload 'describe-option-of-type "help-fns+" "\
Describe an Emacs user OPTION (variable) of a given `defcustom' TYPE.
A prefix argument determines the type-checking behavior:
 - None:         OPTION is defined with TYPE or a subtype of TYPE.
 - Plain `C-u':  OPTION is defined with TYPE or a subtype of TYPE,
                 or its current value is compatible with TYPE.
 - Negative:     OPTION is defined with TYPE (exact match).
 - Non-negative: OPTION is defined with TYPE (exact match),
                 or its current value is compatible with TYPE.

If TYPE is nil (default value) then *all* `defcustom' variables are
potential candidates.  That is different from using `describe-option',
because `describe-option' includes user-variable candidates not
defined with `defcustom' (with `*'-prefixed doc strings).

\(fn TYPE OPTION)" t nil)

(autoload 'describe-file "help-fns+" "\
Describe the file named FILENAME.
If FILENAME is nil, describe current directory (`default-directory').

Starting with Emacs 22, if the file is an image file then:
 * Show a thumbnail of the image as well.
 * If you have command-line tool `exiftool' installed and in your
   `$PATH' or `exec-path', then show EXIF data (metadata) about the
   image.  See standard Emacs library `image-dired.el' for more
   information about `exiftool'.

If FILENAME is the name of an autofile bookmark and you use library
`Bookmark+', then show also the bookmark information (tags etc.).  In
this case, a prefix arg shows the internal form of the bookmark.

In Lisp code:

Non-nil optional arg INTERNAL-FORM-P shows the internal form.
Non-nil optional arg NO-ERROR-P prints an error message but does not
 raise an error.

\(fn FILENAME &optional INTERNAL-FORM-P NO-ERROR-P)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; help-fns+-autoloads.el ends here
