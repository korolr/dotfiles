;;; hungry-delete.el --- hungry delete minor mode

;; Copyright (C) 2009 - 2014 Nathaniel Flath <flat0103@gmail.com>

;; Author: Nathaniel Flath <flat0103@gmail.com>
;; URL: http://github.com/nflath/hungry-delete
;; Package-Version: 20161128.2238
;; Version: 1.1.5

;; This file is not part of GNU Emacs.

;;; Commentary:

;; cc-mode implements hungry deletion for its programming modes.  This
;; package borrows its implementation in a minor mode, so that hungry
;; deletion can be used in all modes.

;;; Installation

;; To use this mode, put the following in your init.el:
;; (require 'hungry-delete)

;; You then need to enable hungry-delete-mode, either in
;; relevant hooks, with turn-on-hungry-delete-mode, or with
;; global-hungry-delete-mode.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(defvar hungry-delete-mode-map (make-keymap)
  "Keymap for hungry-delete-minor-mode.")

(if (fboundp 'delete-forward-char)
    (define-key hungry-delete-mode-map [remap delete-forward-char] 'hungry-delete-forward))

(if (fboundp 'delete-char)
    (define-key hungry-delete-mode-map [remap delete-char] 'hungry-delete-forward))

(define-key hungry-delete-mode-map [remap delete-backward-char] 'hungry-delete-backward)
(define-key hungry-delete-mode-map [remap backward-delete-char-untabify] 'hungry-delete-backward)
(define-key hungry-delete-mode-map [remap c-electric-backspace] 'hungry-delete-backward)
(define-key hungry-delete-mode-map [remap c-electric-delete-forward] 'hungry-delete-forward)

(defvar hungry-delete-chars-to-skip " \t\n\r\f\v"
  "String of characters to skip.")

(defvar hungry-delete-except-modes '(help-mode minibuffer-inactive-mode)
  "List of modes hungry-delete will not be turned on in.")

(defun hungry-delete-skip-ws-forward ()
  "Skip over any whitespace following point.
This function skips over horizontal and vertical whitespace and
line continuations."
  (while (and
          (> (skip-chars-forward hungry-delete-chars-to-skip) 0)
          (eq (char-after) ?\\)
          (progn
            (forward-char)
            (or (eolp) (backward-char)))))
  (while (get-text-property (point) 'read-only)
    (backward-char)))

(defun hungry-delete-skip-ws-backward ()
  "Skip over any whitespace preceding point.
This function skips over horizontal and vertical whitespace and
line continuations."
  (skip-chars-backward hungry-delete-chars-to-skip)
  (while (and
          (eolp)
          (eq (char-before) ?\\)
          (progn
            (backward-char)
            (or
             (= (point) (point-min))
             (< (skip-chars-backward hungry-delete-chars-to-skip) 0)
             (forward-char)))))
  (while (get-text-property (point) 'read-only)
    (forward-char)))

;;;###autoload
(defun hungry-delete-forward (n &optional killflag)
  "Delete the following character, or all of the following
whitespace, up to the next non-whitespace character.  See
\\[c-hungry-delete-forward].

hungry-delete-backward tries to mimic delete-backward-char's
behavior in several ways: if the region is activate, it deletes
the text in the region.  If a prefix argument is given, delete
the following N characters (previous if N is negative).

Optional second arg KILLFLAG non-nil means to kill (save in kill
ring) instead of delete.  Interactively, N is the prefix arg, and
KILLFLAG is set if N was explicitly specified."
  (interactive "p\nP")
  (unless (integerp n)
    (signal 'wrong-type-argument (list 'integerp n)))
  (cond ((and
          (use-region-p)
	      delete-active-region
	      (= n 1))
	 ;; If a region is active, kill or delete it.
	 (if (eq delete-active-region 'kill)
	     (kill-region (region-beginning) (region-end))
	   (delete-region (region-beginning) (region-end))))
	;; If a prefix argument has been given, delete n characters.
	(current-prefix-arg (delete-char n killflag))
	;; Otherwise, call hungry-delete-forward-impl.
	(t (hungry-delete-forward-impl))))




;;;###autoload
(defun hungry-delete-backward (n &optional killflag)
  "Delete the preceding character or all preceding whitespace
back to the previous non-whitespace character.  See also
\\[c-hungry-delete-backward].

hungry-delete-backward tries to mimic delete-backward-char's
behavior in several ways: if the region is activate, it deletes
the text in the region.  If a prefix argument is given, delete
the previous N characters (following if N is negative).

In Overwrite mode, single character backward deletion may replace
tabs with spaces so as to back over columns, unless point is at
the end of the line.

Optional second arg KILLFLAG, if non-nil, means to kill (save in
kill ring) instead of delete.  Interactively, N is the prefix
arg, and KILLFLAG is set if N is explicitly specified."
  (interactive "p\nP")
  (unless (integerp n)
    (signal 'wrong-type-argument (list 'integerp n)))
  (cond ((and
          (use-region-p)
          delete-active-region
          (= n 1))
         ;; If a region is active, kill or delete it.
         (if (eq delete-active-region 'kill)
             (kill-region (region-beginning) (region-end))
           (delete-region (region-beginning) (region-end))))
        ;; In Overwrite mode, maybe untabify while deleting
        ((null (or (null overwrite-mode)
                   (<= n 0)
                   (memq (char-before) '(?\t ?\n))
                   (eobp)
                   (eq (char-after) ?\n)))
         (let ((ocol (current-column)))
           (delete-char (- n) killflag)
           (save-excursion
             (insert-char ?\s (- ocol (current-column)) nil))))
        ;; If a prefix has been given, delete n characters backwards.
        (current-prefix-arg (delete-char (- n) killflag))
        ;; Otherwise, call hungry-delete-backward-impl.
        (t (hungry-delete-backward-impl))))

(defun hungry-delete-impl (fn n)
  "Implementation of hungry-delete functionality.
FN is the function to call to go to the end of whitespace (will
be either hungry-delete-skip-ws-forward or
hungry-delete-skip-ws-backwards by default).  N is the number of
characters to delete if there is no whitespace (will be either 1
or -1 by default)."
  (let ((here (point)))
    (funcall fn)
    (if (/= (point) here)
        (delete-region (point) here)
      (let ((hungry-delete-mode nil))
        (delete-char n)))))

(defun hungry-delete-forward-impl ()
  "Do the dirty work of calling hungry-delete-forward."
  (hungry-delete-impl 'hungry-delete-skip-ws-forward 1))

(defun hungry-delete-backward-impl ()
  "Do the dirty work of calling hungry-delete-backward."
  (hungry-delete-impl 'hungry-delete-skip-ws-backward -1))

;;;###autoload
(define-minor-mode hungry-delete-mode
  "Minor mode to enable hungry deletion.  This will delete all
whitespace after or before point when the deletion command is
executed."
  :init-value nil
  :group 'hungry-delete
  :lighter " h")

;;;###autoload
(defun turn-on-hungry-delete-mode ()
  "Turn on hungry delete mode if the buffer is appropriate."
  (interactive)
  (unless (member major-mode hungry-delete-except-modes)
    (hungry-delete-mode t)))

;;;###autoload
(define-globalized-minor-mode global-hungry-delete-mode hungry-delete-mode turn-on-hungry-delete-mode)

(provide 'hungry-delete)
;;; hungry-delete.el ends here
