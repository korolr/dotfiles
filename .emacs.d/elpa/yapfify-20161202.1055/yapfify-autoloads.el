;;; yapfify-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "yapfify" "yapfify.el" (22769 3792 204835 443000))
;;; Generated autoloads from yapfify.el

(autoload 'yapfify-buffer "yapfify" "\
Try to yapfify the current buffer.
If yapf exits with an error, the output will be shown in a help-window.

\(fn)" t nil)

(autoload 'yapf-mode "yapfify" "\
Automatically run YAPF before saving.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; yapfify-autoloads.el ends here
