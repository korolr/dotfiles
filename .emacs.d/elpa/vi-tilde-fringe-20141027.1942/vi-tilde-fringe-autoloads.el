;;; vi-tilde-fringe-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "vi-tilde-fringe" "vi-tilde-fringe.el" (22768
;;;;;;  50879 934794 32000))
;;; Generated autoloads from vi-tilde-fringe.el

(autoload 'vi-tilde-fringe-mode "vi-tilde-fringe" "\
Buffer-local minor mode to display tildes in the fringe when the line is
empty.

\(fn &optional ARG)" t nil)

(defvar global-vi-tilde-fringe-mode nil "\
Non-nil if Global Vi-Tilde-Fringe mode is enabled.
See the `global-vi-tilde-fringe-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-vi-tilde-fringe-mode'.")

(custom-autoload 'global-vi-tilde-fringe-mode "vi-tilde-fringe" nil)

(autoload 'global-vi-tilde-fringe-mode "vi-tilde-fringe" "\
Toggle Vi-Tilde-Fringe mode in all buffers.
With prefix ARG, enable Global Vi-Tilde-Fringe mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Vi-Tilde-Fringe mode is enabled in all buffers where
`vi-tilde-fringe-mode--turn-on' would do it.
See `vi-tilde-fringe-mode' for more information on Vi-Tilde-Fringe mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; vi-tilde-fringe-autoloads.el ends here
