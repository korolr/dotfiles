;;; hungry-delete-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "hungry-delete" "hungry-delete.el" (22688 13569
;;;;;;  147979 426000))
;;; Generated autoloads from hungry-delete.el

(autoload 'hungry-delete-forward "hungry-delete" "\
Delete the following character, or all of the following
whitespace, up to the next non-whitespace character.  See
\\[c-hungry-delete-forward].

hungry-delete-backward tries to mimic delete-backward-char's
behavior in several ways: if the region is activate, it deletes
the text in the region.  If a prefix argument is given, delete
the following N characters (previous if N is negative).

Optional second arg KILLFLAG non-nil means to kill (save in kill
ring) instead of delete.  Interactively, N is the prefix arg, and
KILLFLAG is set if N was explicitly specified.

\(fn N &optional KILLFLAG)" t nil)

(autoload 'hungry-delete-backward "hungry-delete" "\
Delete the preceding character or all preceding whitespace
back to the previous non-whitespace character.  See also
\\[c-hungry-delete-backward].

hungry-delete-backward tries to mimic delete-backward-char's
behavior in several ways: if the region is activate, it deletes
the text in the region.  If a prefix argument is given, delete
the previous N characters (following if N is negative).

In Overwrite mode, single character backward deletion may replace
tabs with spaces so as to back over columns, unless point is at
the end of the line.

Optional second arg KILLFLAG, if non-nil, means to kill (save in
kill ring) instead of delete.  Interactively, N is the prefix
arg, and KILLFLAG is set if N is explicitly specified.

\(fn N &optional KILLFLAG)" t nil)

(autoload 'hungry-delete-mode "hungry-delete" "\
Minor mode to enable hungry deletion.  This will delete all
whitespace after or before point when the deletion command is
executed.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-hungry-delete-mode "hungry-delete" "\
Turn on hungry delete mode if the buffer is appropriate.

\(fn)" t nil)

(defvar global-hungry-delete-mode nil "\
Non-nil if Global Hungry-Delete mode is enabled.
See the `global-hungry-delete-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-hungry-delete-mode'.")

(custom-autoload 'global-hungry-delete-mode "hungry-delete" nil)

(autoload 'global-hungry-delete-mode "hungry-delete" "\
Toggle Hungry-Delete mode in all buffers.
With prefix ARG, enable Global Hungry-Delete mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Hungry-Delete mode is enabled in all buffers where
`turn-on-hungry-delete-mode' would do it.
See `hungry-delete-mode' for more information on Hungry-Delete mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; hungry-delete-autoloads.el ends here
