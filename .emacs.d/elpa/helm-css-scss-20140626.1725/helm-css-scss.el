;;; helm-css-scss.el --- CSS/SCSS/LESS Selectors with helm interface -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2013 by Shingo Fukuyama

;; Version: 1.3
;; Package-Version: 20140626.1725
;; Author: Shingo Fukuyama - http://fukuyama.co
;; URL: https://github.com/ShingoFukuyama/helm-css-scss
;; Created: Oct 18 2013
;; Keywords: scss css less selector helm
;; Package-Requires: ((helm "1.0") (emacs "24"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Commentary:

;; Example config

;; ----------------------------------------------------
;; ;; helm from https://github.com/emacs-helm/helm
;; (require 'helm)

;; (add-to-list 'load-path "~/.emacs.d/elisp/helm-css-scss")
;; (require 'helm-css-scss)

;; ;; Allow comment inserting depth at each end of a brace
;; (setq helm-css-scss-insert-close-comment-depth 2)
;; ;; If this value is t, split window appears inside the current window
;; (setq helm-css-scss-split-with-multiple-windows nil)
;; ;; Split direction. 'split-window-vertically or 'split-window-horizontally
;; (setq helm-css-scss-split-direction 'split-window-vertically)

;; ;; Set local keybind map for css-mode / scss-mode
;; (dolist ($hook '(css-mode-hook scss-mode-hook less-css-mode-hook))
;;   (add-hook
;;    $hook (lambda ()
;;            (local-set-key (kbd "s-i") 'helm-css-scss)
;;            (local-set-key (kbd "s-I") 'helm-css-scss-back-to-last-point))))

;; (define-key isearch-mode-map (kbd "s-i") 'helm-css-scss-from-isearch)
;; (define-key helm-css-scss-map (kbd "s-i") 'helm-css-scss-multi-from-helm-css-scss)
;; ----------------------------------------------------

;; This program has two main functions

;; (helm-css-scss)
;;   Easily jumping between CSS/SCSS selectors powerd by helm.el

;; (helm-css-scss-insert-close-comment &optional $depth)
;;   Insert inline comment like " //__ comment" at the next of
;;   a close brace "}". If it's aleardy there, update it.
;;   You can also specify a nest $depth of selector.
;;

;;; Code:

(eval-when-compile (require 'cl))

(require 'helm)

(defgroup helm-css-scss nil
  "Open helm-css-scss."
  :prefix "helm-css-scss-" :group 'helm)

;;; config -----------------------------

(defcustom helm-css-scss-insert-close-comment-depth 3
  "Set SCSS style nest depth"
  :group 'helm-css-scss
  :type 'number)

(defcustom helm-css-scss-split-with-multiple-windows nil
 "Split window when having multiple windows open"
 :group 'helm-css-scss
 :type 'boolean)

(defcustom helm-css-scss-split-direction 'split-window-vertically
 "Split direction"
 :type '(choice (const :tag "vertically"   split-window-vertically)
                (const :tag "horizontally" split-window-horizontally))
 :group 'helm-css-scss)

(defcustom helm-css-scss-include-commented-selector t
  "Don't list selectors which is commented, if this value is nil"
  :group 'helm-css-scss
  :type 'boolean)

(defface helm-css-scss-target-line-face
  '((((class color) (background dark))
     (:background "#e3e300" :foreground "#333333"))
    (((class color) (background light))
     (:background "#0033ff" :foreground "#ffffff"))
    (t (:background "#0033ff" :foreground "#ffffff")))
  "Face for helm-css-scss target line"
  :group 'helm-css-scss)

(defface helm-css-scss-selector-depth-face-1
  '((((class color) (background dark)) (:foreground "#ffff00"))
    (((class color) (background light)) (:foreground "#0000ff"))
    (t (:foreground "#ffff00")))
  "Selector depth 1"
  :group 'helm-css-scss)
(defface helm-css-scss-selector-depth-face-2
  '((((class color) (background dark)) (:foreground "#ffdd00"))
    (((class color) (background light)) (:foreground "#3300ff"))
    (t (:foreground "#ffdd00")))
  "Selector depth 2"
  :group 'helm-css-scss)
(defface helm-css-scss-selector-depth-face-3
  '((((class color) (background dark)) (:foreground "#ffbb00"))
    (((class color) (background light)) (:foreground "#6600ff"))
    (t (:foreground "#ffbb00")))
  "Selector depth 3"
  :group 'helm-css-scss)
(defface helm-css-scss-selector-depth-face-4
  '((((class color) (background dark)) (:foreground "#ff9900"))
    (((class color) (background light)) (:foreground "#9900ff"))
    (t (:foreground "#ff9900")))
  "Selector depth 4"
  :group 'helm-css-scss)
(defface helm-css-scss-selector-depth-face-5
  '((((class color) (background dark)) (:foreground "#ff7700"))
    (((class color) (background light)) (:foreground "#cc00ff"))
    (t (:foreground "#ff7700")))
  "Selector depth 5"
  :group 'helm-css-scss)
(defface helm-css-scss-selector-depth-face-6
  '((((class color) (background dark)) (:foreground "#ff5500"))
    (((class color) (background light)) (:foreground "#ff00ff"))
    (t (:foreground "#ff5500")))
  "Selector depth 6"
  :group 'helm-css-scss)

(defvar helm-css-scss-split-window-function
  (lambda ($buf)
    (if helm-css-scss-split-with-multiple-windows
        (funcall helm-css-scss-split-direction)
      (when (one-window-p)
        (funcall helm-css-scss-split-direction)))
    (other-window 1)
    (switch-to-buffer $buf))
  "Change the way to split window only when `helm-css-scss' is calling")

;; Avoide compile error for apply buffer local variable
(defvar helm-css-scss-cache)
(defvar helm-css-scss-last-point nil)
(defvar helm-css-scss-last-query)

(defvar helm-css-scss-overlay nil
  "Store overlay object")

(defvar helm-css-scss-buffer "*Helm Css SCSS*")
(defvar helm-css-scss-multi-buffer "*Helm Css SCSS multi buffers*")
(defvar helm-css-scss-candidate-number-limit 999)
(defvar helm-css-scss-last-line-info nil)

(defvar helm-css-scss-target-buffer nil)
(defvar helm-css-scss-invisible-targets nil)
(defvar helm-css-scss-move-line-action-last-buffer nil)

;;; common parts -----------------------------

(defun helm-css-scss--target-overlay-move (&optional $beg $end $buf)
  "Move target overlay"
  (move-overlay helm-css-scss-overlay (or $beg (point-at-bol)) (or $end (point)) $buf)
  (helm-css-scss--unveil-invisible-overlay))

(defun helm-css-scss-nthcar ($i $l)
  "Return n($i) of values from the head of a list($l)"
  (loop for $k from 1 to $i
        collect (nth (- $k 1) $l) into $res
        finally return (delq nil $res)))

(defun helm-css-scss-substruct-last-string ($text $key)
  "Return the tail of $text without $key strings"
  (while (string-match $key $text)
      (setq $text (substring $text (1+ (string-match $key $text)))))
  $text)

(defsubst helm-css-scss--trim-whitespace ($str)
  "Return string without whitespace at the both beginning and end"
  (if (string-match "\\`\\(?:\\s-+\\)?\\(.+?\\)\\(?:\\s-+\\)?\\'" $str)
      (match-string 1 $str)
    $str))

(defsubst helm-css-scss--goto-line ($line)
  (goto-char (point-min))
  (forward-line (1- $line)))

(defun helm-css-scss-delete-all-matches-in-buffer ($regexp)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward $regexp nil t)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun helm-css-scss--restore-unveiled-overlay ()
  (when helm-css-scss-invisible-targets
    (mapc (lambda ($ov) (overlay-put (car $ov) 'invisible (cdr $ov)))
          helm-css-scss-invisible-targets)
    (setq helm-css-scss-invisible-targets nil)))

(defun helm-css-scss--unveil-invisible-overlay ()
  "Show hidden text temporarily to view it during helm-css-scss.
This function needs to call after latest helm-css-scss-overlay set."
  (helm-css-scss--restore-unveiled-overlay)
  (mapc (lambda ($ov)
          (let (($type (overlay-get $ov 'invisible)))
            (when $type
              (overlay-put $ov 'invisible nil)
              (setq helm-css-scss-invisible-targets
                    (cons (cons $ov $type) helm-css-scss-invisible-targets)))))
        (overlays-in (overlay-start helm-css-scss-overlay)
                     (overlay-end helm-css-scss-overlay))))

(defsubst helm-css-scss--recenter ()
  (recenter (/ (window-height) 2)))

(defun helm-css-scss--nearest-line ($target $list)
  "Return the nearest number of $target out of $list."
  (when (and $target $list)
    (let ($result)
      (cl-labels
          ((filter ($fn $elem $list)
                   (let ($r)
                     (mapc (lambda ($e)
                             (if (funcall $fn $elem $e)
                                 (setq $r (cons $e $r))))
                           $list) $r)))
        (if (eq 1 (length $list))
            (setq $result (car $list))
          (let* (($lts (filter '> $target $list))
                 ($gts (filter '< $target $list))
                 ($lt (if $lts (apply 'max $lts)))
                 ($gt (if $gts (apply 'min $gts)))
                 ($ltg (if $lt (- $target $lt)))
                 ($gtg (if $gt (- $gt $target))))
            (setq $result
                  (cond ((memq $target $list) $target)
                        ((and (not $lt) (not $gt)) nil)
                        ((not $gtg) $lt)
                        ((not $ltg) $gt)
                        ((eq $ltg $gtg) $gt)
                        ((< $ltg $gtg) $lt)
                        ((> $ltg $gtg) $gt)
                        (t 1))))))
      $result)))

(defun helm-css-scss--keep-nearest-position ()
  (with-helm-window
    (let (($p (point-min)) $list $bound
          $nearest-line $target-point
          ($buf (buffer-name (car helm-css-scss-last-line-info))))
      (save-excursion
        (goto-char $p)
        (while (if $p (setq $p (re-search-forward (concat "^" $buf "$") nil t)))
          (when (get-text-property (point-at-bol) 'helm-header)
            (forward-char 1)
            (setq $bound (next-single-property-change (point) 'helm-header))
            (while (re-search-forward "^[0-9]+" $bound t)
              (setq $list (cons
                           (string-to-number (match-string 0))
                           $list)))
            (setq $nearest-line (helm-css-scss--nearest-line
                                 (cdr helm-css-scss-last-line-info)
                                 $list))
            (goto-char $p)
            (re-search-forward (concat "^"
                                       (number-to-string $nearest-line)
                                       ":") $bound t)
            (setq $target-point (point))
            (setq $p nil))))
      (when $target-point
        (goto-char $target-point)
        (helm-mark-current-line)
        (if (equal helm-css-scss-buffer (buffer-name (current-buffer)))
            (helm-css-scss--synchronizing-position)
          (helm-css-scss--move-line-action))))))

;;; scan selector -----------------------------

(defun helm-css-scss-comment-p (&optional $pos)
  (or $pos (setq $pos (point)))
  (nth 4 (parse-partial-sexp (point-min) $pos)))

(defun helm-css-scss-selector-to-hash ()
  "Collect all selectors and make hash table"
  (let ($selector $paren-beg $paren-end $hash $dep $max $sl
                  $selector-name $selector-beg $selector-end
                  $selector-line)
    (setq $hash (make-hash-table :test 'equal))
    (save-excursion
      (goto-char (point-min))
      (while (setq $selector (helm-css-scss-selector-next))
        (setq $paren-beg (point))
        (setq $paren-end (scan-sexps $paren-beg 1))
        (setq $max (cons $paren-end $max))
        (setq $max (mapcar (lambda ($p) (if (< $p $paren-beg) nil $p)) $max))
        (setq $max (delq nil $max))
        (setq $dep (length $max))
        (setq $selector-name (car $selector))
        (setq
         $selector-name
         (cl-case $dep
           (1 (propertize $selector-name 'face 'helm-css-scss-selector-depth-face-1))
           (2 (propertize $selector-name 'face 'helm-css-scss-selector-depth-face-2))
           (3 (propertize $selector-name 'face 'helm-css-scss-selector-depth-face-3))
           (4 (propertize $selector-name 'face 'helm-css-scss-selector-depth-face-4))
           (5 (propertize $selector-name 'face 'helm-css-scss-selector-depth-face-5))
           (6 (propertize $selector-name 'face 'helm-css-scss-selector-depth-face-6))))
        (setq $selector-beg (cadr $selector))
        (setq $selector-end (cddr $selector))
        (setq $selector-line (line-number-at-pos $selector-beg))
        (if (<= $dep (length $sl))
            (loop repeat (- (1+ (length $sl)) $dep) do (pop $sl)))
        (setq $sl (cons $selector-name $sl))
        (puthash (format "%s: %s"
                         (propertize (number-to-string
                                      $selector-line)
                                     'face 'font-lock-function-name-face)
                         (mapconcat 'identity (reverse $sl) " "))
                 (list $paren-beg $paren-end $dep $selector-beg $selector-end $selector-line)
                 $hash)))
    $hash))

(defun helm-css-scss-selector-hash-to-list ()
  "Collected selector hash table to list"
  (let (($hash (helm-css-scss-selector-to-hash)))
    (loop for $k being hash-key in $hash using (hash-values $v)
          collect (cons $k $v))))

;;; core -----------------------------

(defvar helm-css-scss-map
  (let (($map (make-sparse-keymap)))
    (set-keymap-parent $map helm-map)
    (define-key $map (kbd "s-i") 'helm-css-scss-multi-from-helm-css-scss)
    (delq nil $map)))

(defun* helm-css-scss--extract-selector ()
  "Return selector infomation at the point"
  (let (($multi "") $s $po1 $po2 $po3 $str $commentp)
    ;; Collect multiple selector across previous lines
    ;; (i.e. "div, \n p, \n span {...}")
    (save-excursion
      (while (string-match ",[\s\t]*$"
                           (setq $s (helm-css-scss-fetch-previous-line)))
        ;; Skip commented selector (i.e. " // .blue,")
        (save-excursion
          (move-beginning-of-line 1)
          (setq $po3 (point))
          (setq $commentp (helm-css-scss-comment-p (search-forward ","))))
        (unless $commentp
          (setq $multi (format "%s %s"
                               (helm-css-scss--trim-whitespace $s)
                               $multi)))))
    ;; Extract selector include one-line-nesting (i.e. "div { p {...} }")
    (save-excursion
      (skip-chars-backward "^{};\n")
      (setq $po1 (point))
      ;; (setq $beg2 $po1)
      (skip-chars-forward "^{")
      (setq $po2 (point))
      (setq $str (buffer-substring-no-properties $po1 $po2))
      ;; i.e. "div { .box { p"  ->  " p"
      (setq $str (helm-css-scss-substruct-last-string $str "{\\|}"))
      (setq $str (helm-css-scss--trim-whitespace $str))
      ;; Return (selector-name . (selector-beginning-point . selector-end-point))
      (if (equal $multi "")
          (cons (format "%s" $str) (cons $po1 $po2))
        (cons (format "%s %s" (helm-css-scss--trim-whitespace $multi) $str)
              (cons $po3 $po2))))))

(defun* helm-css-scss-selector-next (&optional $bound)
  "Return and goto next selector."
  (unless (helm-css-scss-open-brace-forward $bound)
    (return-from helm-css-scss-selector-next nil))
  (helm-css-scss--extract-selector))

(defun* helm-css-scss-selector-previous (&optional $bound)
  "Return and goto previous selector."
  (unless (helm-css-scss-open-brace-backward $bound)
    (return-from helm-css-scss-selector-previous nil))
  (helm-css-scss--extract-selector))

(defun* helm-css-scss-fetch-previous-line (&optional $prev $noexcursion)
  "Return previous nth ($prev) line strings.
If $noexcursion is not-nil cursor doesn't move."
  ;; In compressed Css without this return, it takes long time
  (if (eq 1 (line-number-at-pos))
      (return-from helm-css-scss-fetch-previous-line ""))
  (or $prev (setq $prev 1))
  (if $noexcursion (setq $noexcursion (point)))
  (move-beginning-of-line (- 1 $prev))
  (let (($po (point)) $res)
    (move-end-of-line 1)
    (setq $res (buffer-substring-no-properties $po (point)))
    (if $noexcursion (goto-char $noexcursion))
    $res))

(defun* helm-css-scss-open-brace-forward (&optional $bound)
  (interactive)
  "Move to next open brace, skip commented brace"
  (let ($ret)
    (setq $ret (re-search-forward "[^#]{" $bound t))
    (unless $ret (return-from helm-css-scss-open-brace-forward nil))
    (backward-char)
    (if (and (helm-css-scss-comment-p (point))
             (not helm-css-scss-include-commented-selector))
        (helm-css-scss-open-brace-forward $bound)
      $ret)))

(defun* helm-css-scss-open-brace-backward (&optional $bound)
  (interactive)
  "Move to previous open brace, skip commented brace"
  (let ($ret)
    (setq $ret (re-search-backward "[^#]{" $bound t))
    (unless $ret (return-from helm-css-scss-open-brace-backward nil))
    (forward-char)
    (if (and (helm-css-scss-comment-p (point))
             (not helm-css-scss-include-commented-selector))
        (helm-css-scss-open-brace-backward $bound)
      $ret)))

;;;###autoload
(defun* helm-css-scss-insert-close-comment ($depth)
  (interactive (list (read-number
                      "Nest Depth: "
                      helm-css-scss-insert-close-comment-depth)))
  ;; Delete original comment for update comments
  (helm-css-scss-delete-all-matches-in-buffer "[ \t]?\\/\\*__.*?\\*\\/")
  (if (<= $depth 0) (return-from helm-css-scss-insert-close-comment nil))
  (let (($list (helm-css-scss-selector-to-hash))
        $r1 $r2 $ordered)
    (save-excursion
      ;; Extract selector and close-brace point
      (loop for $k being hash-key in $list using (hash-values $v)
            if (<= (caddr $v) $depth)
            do (let (($v2 (cadr $v)))
                 (setq $r1 (cons (cons $v2 $k) $r1))
                 (setq $r2 (cons $v2 $r2))))
      ;;(setq $hash (sort* $hash '> :key 'car))
      (setq $r2 (sort $r2 '<))
      (mapc (lambda ($x) (setq $ordered (cons (assoc $x $r1) $ordered))) $r2)
      (loop for ($end . $sel) in $ordered
            do (progn (goto-char $end)
                      (insert (format " /*__ %s */" $sel)))))))

(defun helm-css-scss-current-selector (&optional $list $pos)
  (interactive)
  "Return selector that $pos is in"
  (unless $list (setq $list (helm-css-scss-selector-hash-to-list)))
  (or $pos (setq $pos (point)))
  (let ($r1 $r2)
    (loop for ($sel $beg $end $dep) in $list
          if (and (< $pos $end) (>= $pos $beg))
          do (progn (setq $r1 (cons (cons $dep $sel) $r1))
                    (setq $r2 (cons $dep $r2))))
    (helm-css-scss--target-overlay-move)
    (helm-css-scss--recenter)
    ;; Get the deepest selector
    (assoc-default (car (sort $r2 '>)) $r1)))

;;;###autoload
(defun helm-css-scss-move-and-echo-next-selector ()
  (interactive)
  (let ($s)
    (message (if (setq $s (car (helm-css-scss-selector-next)))
               $s
             (goto-char (point-max))
             "No more exist the next target from here"))))

;;;###autoload
(defun helm-css-scss-move-and-echo-previous-selector ()
  (interactive)
  (let ($s)
    (message (if (setq $s (car (helm-css-scss-selector-previous)))
               $s
             (goto-char (point-min))
             "No more exist the previous target from here"))))

;;; helm -----------------------------

(defadvice helm-next-line (around helm-css-scss--next-line disable)
  (let ((helm-move-to-line-cycle-in-source t))
    ad-do-it
    (when (called-interactively-p 'any)
      (helm-css-scss--synchronizing-position))))
(defadvice helm-previous-line (around helm-css-scss--previous-line disable)
  (let ((helm-move-to-line-cycle-in-source t))
    ad-do-it
    (when (called-interactively-p 'any)
      (helm-css-scss--synchronizing-position))))

(defvar helm-css-scss-synchronizing-window nil
  "Store window identity for synchronizing")
(defun helm-css-scss--synchronizing-position ()
  (with-helm-window
    (let* (($key (helm-css-scss--trim-whitespace
                  (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
           ($cand (assoc-default 'candidates (helm-get-current-source)))
           ($prop (assoc-default $key $cand))
           ($buf helm-css-scss-target-buffer))
      ;; Synchronizing selecter list to buffer
      (with-selected-window helm-css-scss-synchronizing-window
        ;;(helm-css-scss--goto-line (line-number-at-pos (nth 3 $prop)))
        (goto-char (car $prop))
        (with-current-buffer $buf
          (helm-css-scss--target-overlay-move (nth 3 $prop) (nth 4 $prop)))
        (helm-css-scss--recenter))
      (setq helm-css-scss-last-line-info (cons $buf (nth 5 $prop))))))

(defun helm-c-source-helm-css-scss ($list)
  `((name . ,(buffer-name helm-css-scss-target-buffer))
    (candidates . ,$list)
    (action ("Goto open brace"  . (lambda ($pos)
                                    (goto-char (car $pos)) (helm-css-scss--recenter)))
            ("Goto close brace" . (lambda ($pos)
                                    (goto-char (nth 1 $pos)) (helm-css-scss--recenter))))
    (header-line . "helm-css-scss")
    (keymap . ,helm-css-scss-map)))

(defun helm-css-scss-back-to-last-point (&optional $cancel)
  (interactive)
  "Go back to last position where `helm-css-scss' was called"
  (if helm-css-scss-last-point
      (let (($po (point)))
        (switch-to-buffer (cdr helm-css-scss-last-point))
        (goto-char (car helm-css-scss-last-point))
        (unless $cancel
          (setq helm-css-scss-last-point
                (cons $po (buffer-name (current-buffer))))))))

(defun helm-css-scss--clear-cache ()
  "Clear cache when buffer saved"
  (if (boundp 'helm-css-scss-cache) (setq helm-css-scss-cache nil)))
(add-hook 'after-save-hook 'helm-css-scss--clear-cache)

(defun helm-css-scss--set ()
  "Override helm's default behavior for helm-css-scss"
  (setq helm-css-scss-synchronizing-window (selected-window))
  (setq helm-css-scss-last-point (cons (point) (buffer-name (current-buffer))))
  (setq helm-css-scss-target-buffer (current-buffer))
  (setq helm-css-scss-overlay (make-overlay (point-at-bol) (point-at-eol)))
  (overlay-put helm-css-scss-overlay 'face 'helm-css-scss-target-line-face)
  (unless (boundp 'helm-css-scss-last-query)
    (set (make-local-variable 'helm-css-scss-last-query) "")))

(defun helm-css-scss--restore ()
  "Restore helm's hook and window function"
  (when (= 1 helm-exit-status)
    (helm-css-scss-back-to-last-point t)
    (helm-css-scss--restore-unveiled-overlay))
  (setq helm-css-scss-last-query helm-pattern)
  (delete-overlay helm-css-scss-overlay))

;;;###autoload
(defun helm-css-scss (&optional $query)
  (interactive)
  (or $query (setq $query ""))
  ;; Cache
  (cond ((not (boundp 'helm-css-scss-cache))
         (set (make-local-variable 'helm-css-scss-cache)
              (helm-css-scss-selector-hash-to-list)))
        ((not helm-css-scss-cache)
         (setq helm-css-scss-cache (helm-css-scss-selector-hash-to-list)))
        ((buffer-modified-p)
         (setq helm-css-scss-cache (helm-css-scss-selector-hash-to-list))))
  (setq helm-css-scss-last-line-info
        (cons (current-buffer) (line-number-at-pos)))
  (unwind-protect
      (let (($list helm-css-scss-cache))
        (helm-css-scss--set)
        (ad-enable-advice 'helm-next-line 'around 'helm-css-scss--next-line)
        (ad-activate 'helm-next-line)
        (ad-enable-advice 'helm-previous-line
                          'around 'helm-css-scss--previous-line)
        (ad-activate 'helm-previous-line)
        (add-hook 'helm-after-update-hook 'helm-css-scss--keep-nearest-position t)
        ;; Execute helm
        (let ((helm-display-source-at-screen-top nil)
              (helm-display-function helm-css-scss-split-window-function))
          (helm :sources (helm-c-source-helm-css-scss $list)
                :buffer helm-css-scss-buffer
                :input $query
                :prompt "Selector: "
                :preselect (helm-css-scss-current-selector $list)
                :candidate-number-limit helm-css-scss-candidate-number-limit)))
    (progn
      (helm-css-scss--restore)
      (ad-disable-advice 'helm-next-line 'around 'helm-css-scss--next-line)
      (ad-activate 'helm-next-line)
      (ad-disable-advice 'helm-previous-line 'around 'helm-css-scss--previous-line)
      (ad-activate 'helm-previous-line)
      (remove-hook 'helm-after-update-hook 'helm-css-scss--keep-nearest-position))))

;; multi buffers -----------------------------------------

(defadvice helm-move--next-line-fn (around helm-css-scss--next-line-cycle disable)
  (if (not (helm-pos-multiline-p))
      (progn (forward-line 1)
             (when (eobp)
               (helm-beginning-of-buffer)
               (helm-css-scss--recenter)))
    (let ((line-num (line-number-at-pos)))
      (helm-move--next-multi-line-fn)
      (when (eq line-num (line-number-at-pos))
        (helm-beginning-of-buffer)))))
(defadvice helm-move--previous-line-fn (around
                                        helm-css-scss--previous-line-cycle disable)
  (if (not (helm-pos-multiline-p))
      (forward-line -1)
    (helm-move--previous-multi-line-fn))
  (when (helm-pos-header-line-p)
    (when (eq (point) (save-excursion (forward-line -1) (point)))
      (helm-end-of-buffer)
      (and (helm-pos-multiline-p) (helm-move--previous-multi-line-fn)))))

(defadvice helm-next-line (around helm-css-scss-multi--next-line disable)
  (let ((helm-move-to-line-cycle-in-source nil))
    ad-do-it
    (when (called-interactively-p 'any)
      (helm-css-scss--move-line-action))))
(defadvice helm-previous-line (around helm-css-scss-multi--previous-line disable)
  (let ((helm-move-to-line-cycle-in-source nil))
    ad-do-it
    (when (called-interactively-p 'any)
      (helm-css-scss--move-line-action))))

(defun helm-css-scss--move-line-action ()
  (with-helm-window
    (let* (($key (helm-css-scss--trim-whitespace
                  (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
           ($source (helm-get-current-source))
           ($cand (assoc-default 'candidates $source))
           ($buf (get-buffer (assoc-default 'name $source)))
           ($prop (assoc-default $key $cand)))
      ;; Synchronizing line position
      (with-selected-window helm-css-scss-synchronizing-window
        (with-current-buffer $buf
          (when (not (eq $buf helm-css-scss-move-line-action-last-buffer))
            (set-window-buffer nil $buf))
          ;; (helm-css-scss--goto-line (line-number-at-pos (nth 3 $prop)))
          (goto-char (car $prop))
          (helm-css-scss--target-overlay-move (nth 3 $prop) (nth 4 $prop) $buf))
        (setq helm-css-scss-move-line-action-last-buffer $buf)
        (helm-css-scss--recenter))
      (setq helm-css-scss-last-line-info (cons $buf (nth 5 $prop))))))

(defun helm-css-scss--multi (ignored $query $buffers)
  (let (($buffs (or $buffers (helm-css-scss--get-buffer-list)))
        $contents
        $preserve-position
        $preselect)
    (setq helm-css-scss-last-line-info
          (cons (current-buffer) (line-number-at-pos)))
    (mapc (lambda ($buf)
            (with-current-buffer $buf
              (let* (($cont (helm-css-scss-selector-hash-to-list)))
                (if (eq $buf (buffer-name (current-buffer)))
                    (setq $preselect $cont))
                (setq $preserve-position
                      (cons (cons $buf (point)) $preserve-position))
                ;;(setq $cont (propertize $cont 'buffer-name $buf))
                (setq
                 $contents
                 (cons
                  `((name . ,$buf)
                    (candidates . ,$cont)
                    (action
                     ("Goto open brace"  . (lambda ($po)
                                             (switch-to-buffer ,$buf)
                                             (goto-char (car $po)) (helm-css-scss--recenter)))
                     ("Goto close brace" . (lambda ($po)
                                             (switch-to-buffer ,$buf)
                                             (goto-char (nth 1 $po)) (helm-css-scss--recenter))))
                    (header-line . "helm-css-scss-multi"))
                  $contents)))))
          $buffs)
    (unwind-protect
        (progn
          (helm-css-scss--set)
          (ad-enable-advice 'helm-next-line 'around 'helm-css-scss-multi--next-line)
          (ad-activate 'helm-next-line)
          (ad-enable-advice 'helm-previous-line
                            'around 'helm-css-scss-multi--previous-line)
          (ad-activate 'helm-previous-line)
          (ad-enable-advice 'helm-move--next-line-fn 'around
                            'helm-css-scss--next-line-cycle)
          (ad-activate 'helm-move--next-line-fn)
          (ad-enable-advice 'helm-move--previous-line-fn 'around
                            'helm-css-scss--previous-line-cycle)
          (ad-activate 'helm-move--previous-line-fn)
          (let ((helm-display-source-at-screen-top nil)
                (helm-display-function helm-css-scss-split-window-function))
            (helm :sources $contents
                  :buffer helm-css-scss-multi-buffer
                  :input (or $query "")
                  :prompt "Selector: "
                  :preselect (helm-css-scss-current-selector $preselect)
                  :candidate-number-limit helm-css-scss-candidate-number-limit)))
      (progn
        (helm-css-scss--restore)
        (ad-disable-advice 'helm-next-line 'around 'helm-css-scss-multi--next-line)
        (ad-activate 'helm-next-line)
        (ad-disable-advice 'helm-previous-line
                           'around 'helm-css-scss-multi--previous-line)
        (ad-activate 'helm-previous-line)
        (ad-disable-advice 'helm-move--next-line-fn 'around
                           'helm-css-scss--next-line-cycle)
        (ad-activate 'helm-move--next-line-fn)
        (ad-disable-advice 'helm-move--previous-line-fn 'around
                           'helm-css-scss--previous-line-cycle)
        (ad-activate 'helm-move--previous-line-fn)
        (mapc (lambda ($buf)
                (let (($current-buffer (buffer-name (current-buffer))))
                  (with-current-buffer (car $buf)
                    ;; Delete overlay
                    (delete-overlay helm-css-scss-overlay)
                    ;; Restore each buffer's position
                    (unless (equal (car $buf) $current-buffer)
                      (goto-char (cdr $buf))))))
              $preserve-position)))))

(defun helm-css-scss--get-buffer-list ()
  "Get all CSS/SCSS/LESS buffers currently open"
  (let ($buflist1 $file)
    (mapc (lambda ($buf)
            (setq $file (buffer-file-name $buf))
            (and $file
                 (string-match "\\.\\(s?css\\|less\\)$" $file)
                 (setq $buflist1 (cons (buffer-name $buf) $buflist1))))
          (buffer-list))
    $buflist1))

(defun helm-css-scss-multi (&optional $query)
  (interactive)
  "Apply all CSS/SCSS/LESS buffers"
  (if $query
      (setq helm-css-scss-last-query $query)
    (setq helm-css-scss-last-query ""))
  (helm-css-scss--multi nil
                        helm-css-scss-last-query
                        (helm-css-scss--get-buffer-list)))

(defun helm-css-scss-from-isearch ()
"Invoke `helm-css-scss-multi' from isearch."
  (interactive)
  (let (($input (if isearch-regexp
                    isearch-string
                  (regexp-quote isearch-string))))
    (helm-css-scss $input)))
;; When doing isearch, hand the word over to helm-css-scss
;; (define-key isearch-mode-map (kbd "s-i") 'helm-css-scss-from-isearch)

(defun helm-css-scss-multi-from-helm-css-scss ()
  "Invoke `helm-css-scss-multi' from helm-css-scss."
  (interactive)
  (helm-css-scss--restore)
  (ad-disable-advice 'helm-next-line 'around 'helm-css-scss--next-line)
  (ad-activate 'helm-next-line)
  (ad-disable-advice 'helm-previous-line
                     'around 'helm-css-scss--previous-line)
  (ad-activate 'helm-previous-line)
  (delete-overlay helm-css-scss-overlay)
  (helm-quit-and-execute-action (lambda (ignored)
                                  (helm-css-scss-multi helm-pattern))))

;; For helm-resum ---------------------------------
(defadvice helm-resume (around helm-css-scss-resume activate)
  "Resume if the last used helm buffer is helm-css-scss-buffer"
  (if (equal helm-last-buffer helm-css-scss-buffer)
      (if (boundp 'helm-css-scss-last-query)
          (if (not (ad-get-arg 0))
              (helm-css-scss helm-css-scss-last-query))
        ;; Temporary apply second last buffer
        (let ((helm-last-buffer (cadr helm-buffers))) ad-do-it))
    ad-do-it))

(defadvice helm-resume (around helm-css-scss-multi-resume activate)
  "Resume if the last used helm buffer is helm-css-scss-multi-buffer"
  (if (equal helm-last-buffer helm-css-scss-multi-buffer)
      (if (boundp 'helm-css-scss-last-query)
          (if (not (ad-get-arg 0))
              (helm-css-scss-multi helm-css-scss-last-query))
        ;; Temporary apply second last buffer
        (let ((helm-last-buffer (cadr helm-buffers))) ad-do-it))
    ad-do-it))

(provide 'helm-css-scss)
;;; helm-css-scss.el ends here
