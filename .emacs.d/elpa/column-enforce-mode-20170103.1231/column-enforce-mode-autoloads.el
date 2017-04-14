;;; column-enforce-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "column-enforce-mode" "column-enforce-mode.el"
;;;;;;  (22768 50807 34792 98000))
;;; Generated autoloads from column-enforce-mode.el

(autoload 'column-enforce-n "column-enforce-mode" "\
Turn on `column-enforce-mode' with warnings at column N.
N can be given as a prefix argument.

Ex:
  C-u 70 M-x column-enforce-n <enter>
  sets up `column-enforce-mode' to mark text that extends beyond 70 columns.

\(fn N)" t nil)

(autoload 'make-column-rule "column-enforce-mode" "\
Create an interactive function to enforce an N-column-rule.

\(fn N)" nil t)

(autoload 'column-enforce-mode "column-enforce-mode" "\
Minor mode for highlighting text that extends beyond a certain column.

Variable `column-enforce-column' decides which column to start warning at.
 Default is 80
Variable `column-enforce-face' decides how to display the warnings

\(fn &optional ARG)" t nil)

(defvar global-column-enforce-mode nil "\
Non-nil if Global Column-Enforce mode is enabled.
See the `global-column-enforce-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-column-enforce-mode'.")

(custom-autoload 'global-column-enforce-mode "column-enforce-mode" nil)

(autoload 'global-column-enforce-mode "column-enforce-mode" "\
Toggle Column-Enforce mode in all buffers.
With prefix ARG, enable Global Column-Enforce mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Column-Enforce mode is enabled in all buffers where
`column-enforce-mode-toggle-if-applicable' would do it.
See `column-enforce-mode' for more information on Column-Enforce mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; column-enforce-mode-autoloads.el ends here
