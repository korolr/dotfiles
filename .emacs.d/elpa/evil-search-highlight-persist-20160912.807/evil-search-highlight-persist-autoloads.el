;;; evil-search-highlight-persist-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "evil-search-highlight-persist" "evil-search-highlight-persist.el"
;;;;;;  (22688 13545 139980 119000))
;;; Generated autoloads from evil-search-highlight-persist.el

(autoload 'evil-search-highlight-persist "evil-search-highlight-persist" "\
Keep the highlights persist after a search

\(fn &optional ARG)" t nil)

(autoload 'turn-on-search-highlight-persist "evil-search-highlight-persist" "\
Enable search-highlight-persist in the current buffer.

\(fn)" nil nil)

(autoload 'turn-off-search-highlight-persist "evil-search-highlight-persist" "\
Disable evil-search-highlight-persist in the current buffer.

\(fn)" nil nil)

(defvar global-evil-search-highlight-persist nil "\
Non-nil if Global Evil-Search-Highlight-Persist mode is enabled.
See the `global-evil-search-highlight-persist' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-search-highlight-persist'.")

(custom-autoload 'global-evil-search-highlight-persist "evil-search-highlight-persist" nil)

(autoload 'global-evil-search-highlight-persist "evil-search-highlight-persist" "\
Toggle Evil-Search-Highlight-Persist mode in all buffers.
With prefix ARG, enable Global Evil-Search-Highlight-Persist mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Evil-Search-Highlight-Persist mode is enabled in all buffers where
`turn-on-search-highlight-persist' would do it.
See `evil-search-highlight-persist' for more information on Evil-Search-Highlight-Persist mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-search-highlight-persist-autoloads.el ends here
