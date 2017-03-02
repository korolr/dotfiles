;;; pyenv-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "pyenv-mode" "pyenv-mode.el" (22688 13738 83974
;;;;;;  553000))
;;; Generated autoloads from pyenv-mode.el

(autoload 'pyenv-mode-set "pyenv-mode" "\
Set python shell VERSION.

\(fn VERSION)" t nil)

(autoload 'pyenv-mode-unset "pyenv-mode" "\
Unset python shell version.

\(fn)" t nil)

(defvar pyenv-mode nil "\
Non-nil if Pyenv mode is enabled.
See the `pyenv-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pyenv-mode'.")

(custom-autoload 'pyenv-mode "pyenv-mode" nil)

(autoload 'pyenv-mode "pyenv-mode" "\
Minor mode for pyenv interaction.

\\{pyenv-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; pyenv-mode-autoloads.el ends here
