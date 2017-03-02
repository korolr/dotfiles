;;; evil-mc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "evil-mc" "evil-mc.el" (22688 13543 723980
;;;;;;  160000))
;;; Generated autoloads from evil-mc.el

(autoload 'evil-mc-mode "evil-mc" "\
Toggle evil multiple cursors in a single buffer.

\(fn &optional ARG)" t nil)

(defvar global-evil-mc-mode nil "\
Non-nil if Global Evil-Mc mode is enabled.
See the `global-evil-mc-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-mc-mode'.")

(custom-autoload 'global-evil-mc-mode "evil-mc" nil)

(autoload 'global-evil-mc-mode "evil-mc" "\
Toggle Evil-Mc mode in all buffers.
With prefix ARG, enable Global Evil-Mc mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Evil-Mc mode is enabled in all buffers where
`evil-mc-initialize' would do it.
See `evil-mc-mode' for more information on Evil-Mc mode.

\(fn &optional ARG)" t nil)

(autoload 'evil-mc-initialize "evil-mc" "\
Enable `evil-mc-mode' in the current buffer.

\(fn)" nil nil)

(autoload 'turn-on-evil-mc-mode "evil-mc" "\
Turn on evil-mc mode in the current buffer.

\(fn)" t nil)

(autoload 'turn-off-evil-mc-mode "evil-mc" "\
Turn off evil-mc mode in the current buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("evil-mc-command-execute.el" "evil-mc-command-record.el"
;;;;;;  "evil-mc-common.el" "evil-mc-cursor-make.el" "evil-mc-cursor-state.el"
;;;;;;  "evil-mc-known-commands.el" "evil-mc-pkg.el" "evil-mc-region.el"
;;;;;;  "evil-mc-scratch.el" "evil-mc-setup.el" "evil-mc-undo.el"
;;;;;;  "evil-mc-vars.el") (22688 13543 731980 160000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-mc-autoloads.el ends here
