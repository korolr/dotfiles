;;; helm-themes.el --- Color theme selection with helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-themes
;; Package-Version: 20160917.2245
;; Version: 0.05
;; Package-Requires: ((helm-core "2.0") (emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; helm-themes.el provide theme selection with helm interface.
;; Its persistent action can set theme temporary.
;;

;;; Code:

(require 'helm)

;; Loading mutiple themes makes Emacs too slow
(defsubst helm-themes--delete-theme ()
  (mapc 'disable-theme custom-enabled-themes))

(defun helm-themes--load-theme (theme-str)
  (helm-themes--delete-theme)
  (if (string= theme-str "default")
      t
    (load-theme (intern theme-str) t)))

(defun helm-themes--candidates ()
  (cons 'default (custom-available-themes)))

(defvar helm-themes-source
  (helm-build-sync-source "Selection Theme"
    :candidates 'helm-themes--candidates
    :action 'helm-themes--load-theme
    :persistent-action 'helm-themes--load-theme))

;;;###autoload
(defun helm-themes ()
  "Theme selection with helm interface"
  (interactive)
  (let ((changed nil)
        (orig-theme (when custom-enabled-themes
                      (car custom-enabled-themes))))
    (unwind-protect
        (progn
          (when (helm :sources helm-themes-source :buffer "*helm-themes*")
            (setq changed t)))
      (when (not changed)
        (helm-themes--delete-theme)
        (when orig-theme
          (load-theme orig-theme t))))))

(provide 'helm-themes)

;;; helm-themes.el ends here
