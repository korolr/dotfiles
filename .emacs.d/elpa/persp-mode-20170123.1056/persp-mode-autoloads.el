;;; persp-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "persp-mode" "persp-mode.el" (22688 13590 743978
;;;;;;  803000))
;;; Generated autoloads from persp-mode.el

(autoload 'def-auto-persp "persp-mode" "\


\(fn NAME &rest KEYARGS &key BUFFER-NAME FILE-NAME MODE MODE-NAME MINOR-MODE MINOR-MODE-NAME PREDICATE HOOKS DYN-ENV GET-NAME-EXPR GET-BUFFER-EXPR GET-PERSP-EXPR SWITCH PARAMETERS NOAUTO WEAK ON-MATCH AFTER-MATCH DONT-PICK-UP-BUFFERS DELETE)" nil nil)

(autoload 'def-persp-buffer-save/load "persp-mode" "\


\(fn &rest KEYARGS &key BUFFER-NAME FILE-NAME MODE MODE-NAME MINOR-MODE MINOR-MODE-NAME PREDICATE TAG-SYMBOL SAVE-VARS SAVE-FUNCTION LOAD-FUNCTION AFTER-LOAD-FUNCTION APPEND)" nil nil)

(defvar persp-mode nil "\
Non-nil if Persp mode is enabled.
See the `persp-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `persp-mode'.")

(custom-autoload 'persp-mode "persp-mode" nil)

(autoload 'persp-mode "persp-mode" "\
Toggle the persp-mode.
When active, keeps track of multiple 'perspectives',
named collections of buffers and window configurations.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; persp-mode-autoloads.el ends here
