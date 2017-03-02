;;; evil-indent-plus.el --- Evil textobjects based on indentation

;; Original evil-indent-textobject.el Copyright (C) 2013 Michael Markert
;; Modifications Copyright (C) 2014-2015 Eivind Fonn
;;
;; Author: Eivind Fonn <evfonn@gmail.com>
;; Version: 1.0.0
;; Package-Version: 20151109.1106
;; Keywords: convenience evil
;; URL: http://github.com/TheBB/evil-indent-plus
;; Package-Requires: ((evil "0") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Adds new text objects:
;;
;; ii, ai: Block of text with same or higher indentation
;; iI, aI: Block of text with same or higher indentation, including the first line
;;         above with smaller indentation
;; iJ, aJ: Block of text with same or higher indentation, including the first lines
;;         above and below with smaller indentation

;;; Code:

(require 'cl-lib)
(require 'evil)

(defun evil-indent-plus--chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
  str)

(defun evil-indent-plus--empty-line-p ()
  (string= "" (evil-indent-plus--chomp (thing-at-point 'line))))

(defun evil-indent-plus--not-empty-line-p ()
  (not (evil-indent-plus--empty-line-p)))

(defun evil-indent-plus--geq-p ()
  (>= (current-indentation) base))

(defun evil-indent-plus--geq-or-empty-p ()
  (or (evil-indent-plus--empty-line-p) (evil-indent-plus--geq-p)))

(defun evil-indent-plus--g-p ()
  (> (current-indentation) base))

(defun evil-indent-plus--g-or-empty-p ()
  (or (evil-indent-plus--empty-line-p) (evil-indent-plus--g-p)))

(defun evil-indent-plus--seek (start direction before skip predicate)
  "Seeks forward (if direction is 1) or backward (if direction is -1) from start, until predicate
fails. If before is nil, it will return the first line where predicate fails, otherwise it returns
the last line where predicate holds."
  (save-excursion
    (goto-char start)
    (goto-char (point-at-bol))
    (let ((bnd (if (> 0 direction)
                   (point-min)
                 (point-max)))
          (pt (point)))
      (when skip (forward-line direction))
      (cl-loop while (and (/= (point) bnd) (funcall predicate))
               do (progn
                    (when before (setq pt (point-at-bol)))
                    (forward-line direction)
                    (unless before (setq pt (point-at-bol)))))
      pt)))

(defun evil-indent-plus--same-indent-range (&optional point)
  "Return the point at the begin and end of the text block with the same (or greater) indentation.
If `point' is supplied and non-nil it will return the begin and end of the block surrounding point."
  (save-excursion
    (when point
      (goto-char point))
    (let ((base (current-indentation))
          (begin (point))
          (end (point)))
      (setq begin (evil-indent-plus--seek begin -1 t nil 'evil-indent-plus--geq-or-empty-p))
      (setq begin (evil-indent-plus--seek begin 1 nil nil 'evil-indent-plus--g-or-empty-p))
      (setq end (evil-indent-plus--seek end 1 t nil 'evil-indent-plus--geq-or-empty-p))
      (setq end (evil-indent-plus--seek end -1 nil nil 'evil-indent-plus--empty-line-p))
      (list begin end base))))

(defun evil-indent-plus--up-indent-range (&optional point)
  (let* ((range (evil-indent-plus--same-indent-range point))
         (base (cl-third range))
         (begin (evil-indent-plus--seek (cl-first range)
                                        -1 nil nil
                                        'evil-indent-plus--geq-or-empty-p)))
    (list begin (cl-second range) base)))

(defun evil-indent-plus--up-down-indent-range (&optional point)
  (let* ((range (evil-indent-plus--same-indent-range point))
         (base (cl-third range))
         (begin (evil-indent-plus--seek (cl-first range)
                                        -1 nil nil
                                        'evil-indent-plus--geq-or-empty-p))
         (end (evil-indent-plus--seek (cl-second range)
                                      1 nil nil
                                      'evil-indent-plus--geq-or-empty-p)))
    (list begin end base)))

(defun evil-indent-plus--linify (range)
  (let ((nbeg (save-excursion (goto-char (cl-first range)) (point-at-bol)))
        (nend (save-excursion (goto-char (cl-second range)) (point-at-eol))))
    (evil-range nbeg nend 'line)))

(defun evil-indent-plus--extend (range)
  (let ((begin (cl-first range))
        (end (cl-second range))
        nend)
    (setq nend (evil-indent-plus--seek end 1 t t 'evil-indent-plus--empty-line-p))
    (when (= nend end)
      (setq begin (evil-indent-plus--seek begin -1 t t 'evil-indent-plus--empty-line-p)))
    (list begin nend)))

;;;###autoload (autoload 'evil-indent-plus-i-indent "evil-indent-plus" nil t)
(evil-define-text-object evil-indent-plus-i-indent (&optional count beg end type)
  "Text object describing the block with the same (or greater) indentation as the current line,
skipping empty lines."
  :type line
  (evil-indent-plus--linify (evil-indent-plus--same-indent-range)))

;;;###autoload (autoload 'evil-indent-plus-a-indent "evil-indent-plus" nil t)
(evil-define-text-object evil-indent-plus-a-indent (&optional count beg end type)
  "Text object describing the block with the same (or greater) indentation as the current line,
skipping empty lines."
  :type line
  (evil-indent-plus--linify (evil-indent-plus--extend (evil-indent-plus--same-indent-range))))

;;;###autoload (autoload 'evil-indent-plus-i-indent-up "evil-indent-plus" nil t)
(evil-define-text-object evil-indent-plus-i-indent-up (&optional count beg end type)
  "Text object describing the block with the same (or greater) indentation as the current line,
and the line above, skipping empty lines."
  :type line
  (evil-indent-plus--linify (evil-indent-plus--up-indent-range)))

;;;###autoload (autoload 'evil-indent-plus-a-indent-up "evil-indent-plus" nil t)
(evil-define-text-object evil-indent-plus-a-indent-up (&optional count beg end type)
  "Text object describing the block with the same (or greater) indentation as the current line,
and the line above, skipping empty lines."
  :type line
  (evil-indent-plus--linify (evil-indent-plus--extend (evil-indent-plus--up-indent-range))))

;;;###autoload (autoload 'evil-indent-plus-i-indent-up-down "evil-indent-plus" nil t)
(evil-define-text-object evil-indent-plus-i-indent-up-down (&optional count beg end type)
  "Text object describing the block with the same (or greater) indentation as the current line,
and the line above and below, skipping empty lines."
  :type line
  (evil-indent-plus--linify (evil-indent-plus--up-down-indent-range)))

;;;###autoload (autoload 'evil-indent-plus-a-indent-up-down "evil-indent-plus" nil t)
(evil-define-text-object evil-indent-plus-a-indent-up-down (&optional count beg end type)
  "Text object describing the block with the same (or greater) indentation as the current line,
and the line above and below, skipping empty lines."
  :type line
  (evil-indent-plus--linify (evil-indent-plus--extend (evil-indent-plus--up-down-indent-range))))

;;;###autoload
(defun evil-indent-plus-default-bindings ()
  "Set the default evil-indent-plus keybindings."
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
  (define-key evil-inner-text-objects-map "J" 'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "J" 'evil-indent-plus-a-indent-up-down))

(provide 'evil-indent-plus)

;;; evil-indent-plus.el ends here
