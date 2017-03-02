;;; info+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "info+" "info+.el" (22688 13571 643979 354000))
;;; Generated autoloads from info+.el

(let ((loads (get 'Info-Plus 'custom-loads))) (if (member '"info+" loads) nil (put 'Info-Plus 'custom-loads (cons '"info+" loads))))

(defface info-double-quoted-name '((((background dark)) (:inherit font-lock-string-face :foreground "Cyan")) (t (:inherit font-lock-string-face :foreground "DarkOrange"))) "\
*Face for names enclosed in curly double-quotes (“...”) in `info'." :group (quote Info-Plus) :group (quote faces))

(defface info-emphasis '((t (:inherit italic))) "\
*Face for emphasizing text enclosed with underscores (_..._) in `info'." :group (quote Info-Plus) :group (quote faces))

(defface info-file '((((background dark)) (:foreground "Yellow" :background "DimGray")) (t (:foreground "Blue" :background "LightGray"))) "\
*Face for file heading labels in `info'." :group (quote Info-Plus) :group (quote faces))

(defface info-menu '((((background dark)) (:foreground "Yellow")) (t (:foreground "Blue"))) "\
*Face used for menu items in `info'." :group (quote Info-Plus) :group (quote faces))

(defface info-quoted-name '((((background dark)) (:inherit font-lock-string-face :foreground "#6B6BFFFF2C2C")) (((background light)) (:inherit font-lock-string-face :foreground "DarkViolet")) (t (:foreground "yellow"))) "\
*Face for quoted names (‘...’ or `...') in `info'." :group (quote Info-Plus) :group (quote faces))

(defface info-string '((((background dark)) (:inherit font-lock-string-face :foreground "Orange")) (t (:inherit font-lock-string-face :foreground "red3"))) "\
*Face for strings (\"...\") in `info'." :group (quote Info-Plus) :group (quote faces))

(defface info-single-quote '((((background dark)) (:inherit font-lock-keyword-face :foreground "Green")) (t (:inherit font-lock-keyword-face :foreground "Magenta"))) "\
*Face for isolated single-quote marks (') in `info'." :group (quote Info-Plus) :group (quote faces))

(defface info-title-1 '((((type tty pc) (class color) (background dark)) :foreground "yellow" :weight bold) (((type tty pc) (class color) (background light)) :foreground "brown" :weight bold)) "\
*Face for info titles at level 1." :group (quote info))

(defface info-title-2 '((((type tty pc) (class color)) :foreground "lightblue" :weight bold)) "\
*Face for info titles at level 2." :group (quote info))

(defface info-title-3 '((((type tty pc) (class color)) :weight bold)) "\
*Face for info titles at level 3." :group (quote info))

(defface info-title-4 '((((type tty pc) (class color)) :weight bold)) "\
*Face for info titles at level 4." :group (quote info))

(defface info-command-ref-item '((((background dark)) (:foreground "#7474FFFF7474" :background "DimGray")) (t (:foreground "Blue" :background "LightGray"))) "\
*Face used for \"Command:\" reference items in `info' manual." :group (quote Info-Plus) :group (quote faces))

(defface info-constant-ref-item '((((background dark)) (:foreground "DeepPink" :background "DimGray")) (t (:foreground "DeepPink" :background "LightGray"))) "\
*Face used for \"Constant:\" reference items in `info' manual." :group (quote Info-Plus) :group (quote faces))

(defface info-function-ref-item '((((background dark)) (:foreground "#4D4DDDDDDDDD" :background "DimGray")) (t (:foreground "DarkBlue" :background "LightGray"))) "\
*Face used for \"Function:\" reference items in `info' manual." :group (quote Info-Plus) :group (quote faces))

(defface info-macro-ref-item '((((background dark)) (:foreground "Yellow" :background "DimGray")) (t (:foreground "DarkMagenta" :background "LightGray"))) "\
*Face used for \"Macro:\" reference items in `info' manual." :group (quote Info-Plus) :group (quote faces))

(defface info-reference-item '((((background dark)) (:background "DimGray")) (t (:background "LightGray"))) "\
*Face used for reference items in `info' manual." :group (quote Info-Plus) :group (quote faces))

(defface info-special-form-ref-item '((((background dark)) (:foreground "Yellow" :background "DimGray")) (t (:foreground "DarkMagenta" :background "LightGray"))) "\
*Face used for \"Special Form:\" reference items in `info' manual." :group (quote Info-Plus) :group (quote faces))

(defface info-syntax-class-item '((((background dark)) (:foreground "#FFFF9B9BFFFF" :background "DimGray")) (t (:foreground "DarkGreen" :background "LightGray"))) "\
*Face used for \"Syntax Class:\" reference items in `info' manual." :group (quote Info-Plus) :group (quote faces))

(defface info-user-option-ref-item '((((background dark)) (:foreground "Red" :background "DimGray")) (t (:foreground "Red" :background "LightGray"))) "\
*Face used for \"User Option:\" reference items in `info' manual." :group (quote Info-Plus) :group (quote faces))

(defface info-variable-ref-item '((((background dark)) (:foreground "Orange" :background "DimGray")) (t (:foreground "FireBrick" :background "LightGray"))) "\
*Face used for \"Variable:\" reference items in `info' manual." :group (quote Info-Plus) :group (quote faces))

(defvar Info-breadcrumbs-in-header-flag nil "\
*Non-nil means breadcrumbs are shown in the header line.")

(custom-autoload 'Info-breadcrumbs-in-header-flag "info+" t)

(defvar Info-display-node-header-fn 'Info-display-node-default-header "\
*Function to insert header by `Info-merge-subnodes'.")

(custom-autoload 'Info-display-node-header-fn "info+" t)

(defvar Info-emphasis-regexp "_\\(\\(\\sw\\(\\s-\\|\\sw\\|\\s.\\)*\\)\\|\\(\\(\\s-\\|\\sw\\|\\s.\\)\\sw*\\)\\)_" "\
Regexp to match text enclosed in underscore (`_') characters.

The default value matches the following (enclosed in underscores):
word, punctuation, and whitespace characters, plus hyphens, with at
least one word character.  Hyphen is included explicitly because it
generally has symbol syntax in Info.

Some possible values include:

 _\\(\\(\\sw\\(\\s-\\|\\sw\\|\\s.\\)*\\)\\|\\(\\(\\s-\\|\\sw\\|\\s.\\)\\sw*\\)\\)_ (default)
 _\\(\\(\\s-\\|\\sw\\|\\s.\\)+\\)_ (word, punctuation, whitespace)
 _\\(\\sw+\\)_		  (single words)
 _\\(\\s-*\\sw+\\s-*\\)_	  (single words, maybe whitespace-separated)
 _\\([^_\\n]+\\)_		  (anything except newlines)
 _\\([^_]+\\)_		  (anything)

Note that any value can be problematic for some Info text - see
`Info-fontify-emphasis-flag'.")

(custom-autoload 'Info-emphasis-regexp "info+" t)

(defvar Info-fit-frame-flag t "\
*Non-nil means call `fit-frame' on Info buffer.")

(custom-autoload 'Info-fit-frame-flag "info+" t)

(defvar Info-fontify-angle-bracketed-flag t "\
*Non-nil means `info' fontifies text within <...>.
A non-nil value has no effect unless `Info-fontify-quotations-flag' is
also non-nil.

Note: This fontification can never be 100% reliable.  It aims to be
useful in most Info texts, but it can occasionally result in
fontification that you might not expect.  This is not a bug; it is
part of the design to be able to appropriately fontify a great variety
of texts.  Set this flag to nil if you do not find this fontification
useful.  You can use command `Info-toggle-fontify-angle-bracketed' to
toggle the option value.")

(custom-autoload 'Info-fontify-angle-bracketed-flag "info+" t)

(defvar Info-fontify-emphasis-flag t "\
*Non-nil means `info' fontifies text between underscores (`_').
The text that is highlighted matches the value of option
`Info-emphasis-regexp'.

Note 1:
This fontification hides the underscores that surround text that is
emphasized.  Because this fontification is not 100% reliable (see Note
2), in cases where it is inappropriate or unhelpful you might want to
see the hidden underscore characters.  You can toggle showing all
hidden text (not just hidden underscores) using `M-x visible-mode'.
See (info) `Help-Inv' for more information about this.

Note 2:
This fontification can never be 100% reliable.  It aims to be useful
in most Info texts, but it can occasionally result in fontification
that you might not expect.  This is not a bug; it is part of the
design to be able to appropriately fontify a great variety of texts.
Set this flag to nil if you do not find this fontification useful.
You can use command `Info-toggle-fontify-emphasis' to toggle the
option value.

Note 3:
If internal variable `info-fontify-emphasis' is `nil' then emphasis is
never highlighted, and this option has no effect.  This gives you a
way to turn off all matching of `Info-emphasis-regexp'.")

(custom-autoload 'Info-fontify-emphasis-flag "info+" t)

(defvar Info-fontify-quotations-flag t "\
*Non-nil means `info' fontifies text between quotes.
This applies to double-quoted text (“...” or \"...\") and text
between single-quotes (‘...’ or `...').

Note: This fontification can never be 100% reliable.  It aims to be
useful in most Info texts, but it can occasionally result in
fontification that you might not expect.  This is not a bug; it is
part of the design to be able to appropriately fontify a great variety
of texts.  Set this flag to nil if you do not find this fontification
useful.  You can use command `Info-toggle-fontify-quotations' to
toggle the option value.")

(custom-autoload 'Info-fontify-quotations-flag "info+" t)

(defvar Info-fontify-reference-items-flag t "\
*Non-nil means `info' fontifies reference items such as \"Function:\".")

(custom-autoload 'Info-fontify-reference-items-flag "info+" t)

(defvar Info-saved-nodes nil "\
*List of Info node names you can visit using `\\<Info-mode-map>\\[Info-virtual-book]'.
Each node name is a string.  The node name can be absolute, including
a filename, such as \"(emacs)Basic\", or it can be relative, such as
\"Basic\".
You can customize this option, but you can also add node names to it
easily using `\\<Info-mode-map>\\[Info-save-current-node]'.")

(custom-autoload 'Info-saved-nodes "info+" t)

(defvar Info-fontify-single-quote-flag t "\
*Non-nil means `info' fontifies ' when not preceded by `....
A non-nil value has no effect unless `Info-fontify-quotations-flag' is
also non-nil.

Note: This fontification can never be 100% reliable.  It aims to be
useful in most Info texts, but it can occasionally result in
fontification that you might not expect.  This is not a bug; it is
part of the design to be able to appropriately fontify a great variety
of texts.  Set this flag to nil if you do not find this fontification
useful.  You can use command `Info-toggle-fontify-single-quote' to
toggle the option value.")

(custom-autoload 'Info-fontify-single-quote-flag "info+" t)

(defvar Info-subtree-separator "\n* " "\
*A string used to separate Info node descriptions.
Inserted by `Info-merge-subnodes' just before each node title.
Setting this to a string that includes a form-feed (^L), such as
\"\\f\\n* \", will cause a page break before each node description.

Use command `set-variable' to set this, quoting any control characters
you want to include, such as form-feed (^L) and newline (^J), with ^Q.
For example, type `^Q^L^Q^J* ' to set this to \"\\f\\n* \".")

(custom-autoload 'Info-subtree-separator "info+" t)
 (autoload 'Info-mouse-follow-nearest-node-new-window "info+")
 (autoload 'Info-follow-nearest-node-new-window "info+")
 (autoload 'Info-clear "info+")
 (autoload 'Info-toggle-breadcrumbs-in-header "info+")
 (autoload 'Info-toggle-fontify-emphasis "info+")
 (autoload 'Info-toggle-fontify-quotations "info+")
 (autoload 'Info-toggle-fontify-single-quote "info+")
 (autoload 'Info-toggle-fontify-angle-bracketed "info+")
 (autoload 'Info-save-current-node "info+")
 (autoload 'Info-merge-subnodes "info+")
 (autoload 'Info-virtual-book "info+")
 (autoload 'Info-goto-node-web "info+")
 (autoload 'Info-url-for-node "info+")
 (autoload 'info-manual "info+")

(autoload 'Info-goto-emacs-command-node "info+" "\
Go to the Info node in the Emacs manual for command COMMAND.
The command is found by looking it up in Emacs manual's indexes,
or in another manual found via COMMAND's `info-file' property or
the variable `Info-file-list-for-emacs'.
COMMAND must be a symbol or string.

\(fn COMMAND &optional MSGP)" t nil)

(autoload 'Info-goto-emacs-key-command-node "info+" "\
Go to the node in the Emacs manual describing command bound to KEY.
KEY is a string.

Interactively, if the binding is `execute-extended-command', then a
command is read.

The command is found by looking it up in Emacs manual's indexes,
or in another manual's index found via COMMAND's `info-file' property
or the variable `Info-file-list-for-emacs'.

If key's command cannot be found by looking in indexes, then
`Info-search' is used to search for the key sequence in the info text.

\(fn KEY &optional MSGP)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; info+-autoloads.el ends here
