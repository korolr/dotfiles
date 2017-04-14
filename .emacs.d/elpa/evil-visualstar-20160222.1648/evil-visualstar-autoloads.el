;;; evil-visualstar-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "evil-visualstar" "evil-visualstar.el" (22768
;;;;;;  50831 334792 743000))
;;; Generated autoloads from evil-visualstar.el

(autoload 'evil-visualstar-mode "evil-visualstar" "\
Minor mode for visual star selection.

\(fn &optional ARG)" t nil)

(defvar global-evil-visualstar-mode nil "\
Non-nil if Global Evil-Visualstar mode is enabled.
See the `global-evil-visualstar-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-visualstar-mode'.")

(custom-autoload 'global-evil-visualstar-mode "evil-visualstar" nil)

(autoload 'global-evil-visualstar-mode "evil-visualstar" "\
Toggle Evil-Visualstar mode in all buffers.
With prefix ARG, enable Global Evil-Visualstar mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Evil-Visualstar mode is enabled in all buffers where
`turn-on-evil-visualstar-mode' would do it.
See `evil-visualstar-mode' for more information on Evil-Visualstar mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-evil-visualstar-mode "evil-visualstar" "\
Turns on visual star selection.

\(fn)" t nil)

(autoload 'turn-off-evil-visualstar-mode "evil-visualstar" "\
Turns off visual star selection.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-visualstar-autoloads.el ends here
