;;; evil-anzu.el --- anzu for evil-mode

;; Copyright (C) 2017 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;;         Fredrik Bergroth <fbergroth@gmail.com>
;; URL: https://github.com/syohex/emacs-evil-anzu
;; Package-Version: 20170123.2318
;; Version: 0.02
;; Package-Requires: ((evil "1.0.0") (anzu "0.46"))

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

;;; Code:

(require 'evil)
(require 'anzu)

(defadvice evil-search (after evil-anzu-compat (string forward &optional regexp-p start) activate)
  (anzu--cons-mode-line-search)
  (let ((isearch-regexp regexp-p))
    (anzu--update string)))

(defadvice evil-ex-find-next (after evil-anzu-compat (&optional pattern direction nowrap) activate)
  "Make anzu work with the 'evil-search search module.
If PATTERN is not specified the current global pattern `evil-ex-search-pattern' is used."
  (anzu--cons-mode-line-search)
  (let* ((isearch-regexp t)      ; all evil-ex searches are regexp searches
         (current-pattern (or pattern evil-ex-search-pattern))
         (regexp (evil-ex-pattern-regex current-pattern)))
    (save-match-data            ; don't let anzu's searching mess up evil
      (anzu--update regexp))))

(defadvice evil-flash-hook (after evil-anzu-compat activate)
  ;; Prevent flickering, only run if timer is not active
  (unless (memq evil-flash-timer timer-list)
    (anzu--reset-mode-line)))

(defadvice evil-ex-delete-hl (after evil-anzu-compat (name) activate)
  (when (eq name 'evil-ex-search)
    (anzu--reset-mode-line)))

(provide 'evil-anzu)

;;; evil-anzu.el ends here
