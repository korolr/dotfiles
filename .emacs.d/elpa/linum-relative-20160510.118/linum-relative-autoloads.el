;;; linum-relative-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "linum-relative" "linum-relative.el" (22688
;;;;;;  13572 643979 325000))
;;; Generated autoloads from linum-relative.el

(autoload 'linum-relative-toggle "linum-relative" "\
Toggle between linum-relative and linum.

\(fn)" t nil)

(autoload 'linum-relative-mode "linum-relative" "\
Display relative line numbers for current buffer.

\(fn &optional ARG)" t nil)

(defvar linum-relative-global-mode nil "\
Non-nil if Linum-Relative-Global mode is enabled.
See the `linum-relative-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `linum-relative-global-mode'.")

(custom-autoload 'linum-relative-global-mode "linum-relative" nil)

(autoload 'linum-relative-global-mode "linum-relative" "\
Toggle Linum-Relative mode in all buffers.
With prefix ARG, enable Linum-Relative-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Linum-Relative mode is enabled in all buffers where
`(lambda nil (unless (linum-relative-in-helm-p) (linum-relative-mode 1)))' would do it.
See `linum-relative-mode' for more information on Linum-Relative mode.

\(fn &optional ARG)" t nil)

(autoload 'helm-linum-relative-mode "linum-relative" "\
Turn on `linum-relative-mode' in helm.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; linum-relative-autoloads.el ends here
