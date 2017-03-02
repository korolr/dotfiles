;;; pug-mode.el --- Major mode for jade/pug template files
;;
;; Copyright (c) 2007, 2008 Nathan Weizenbaum
;; Copyright (c) 2009-2013 Daniel Mendler
;; Copyright (c) 2012-2014 Bozhidar Batsov
;; Copyright (c) 2016 Henrik Lissner
;;
;; Author: Nathan Weizenbaum
;; Author: Daniel Mendler
;; Author: Bozhidar Batsov
;; Author: Henrik Lissner
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: February 18, 2016
;; Modified: September 15, 2016
;; Version: 1.0.4
;; Package-Version: 20170127.1949
;; Homepage: https://github.com/hlissner/emacs-pug-mode
;; Keywords: markup, language, jade, pug
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; `pug-mode' offers Emacs support for Pug (http://jade-lang.com/) based off of
;; slim-mode.
;;
;;; Code:

(eval-when-compile
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (require 'cl-lib))

;; User definable variables
(defgroup pug nil
  "Support for the Pug template language."
  :group 'languages
  :prefix "pug-")

(defcustom pug-mode-hook nil
  "Hook run when entering Pug mode."
  :type 'hook
  :group 'pug)

(defcustom pug-backspace-backdents-nesting t
  "Non-nil to have `pug-electric-backspace' re-indent all code nested beneath
the backspaced line be re-indented along with the line itself."
  :type 'boolean
  :group 'pug)

(defvar pug-indent-function 'pug-indent-p
  "This function should look at the current line and return true if the next
line could be nested within this line.")

(defconst pug-tags-re
  (concat "\\(?:^\\s-*\\|:\\s-+\\)"
          (regexp-opt
           '("a" "abbr" "acronym" "address" "applet" "area" "article" "aside"
             "audio" "b" "base" "basefont" "bdo" "big" "blockquote" "body" "br"
             "button" "canvas" "caption" "center" "cite" "code" "col" "colgroup"
             "command" "datalist" "dd" "del" "details" "dialog" "dfn" "dir"
             "div" "dl" "dt" "em" "embed" "fieldset" "figure" "figcaption" "font" "footer"
             "form" "frame" "frameset" "h1" "h2" "h3" "h4" "h5" "h6" "head"
             "header" "hgroup" "hr" "html" "i" "iframe" "img" "input" "ins"
             "keygen" "kbd" "label" "legend" "li" "link" "map" "main" "mark" "menu"
             "meta" "meter" "nav" "noframes" "noscript" "object" "ol" "optgroup"
             "option" "output" "p" "param" "pre" "progress" "q" "rp" "rt" "ruby"
             "s" "samp" "script" "section" "select" "small" "source" "span"
             "strike" "strong" "style" "sub" "sup" "table" "tbody" "td"
             "textarea" "tfoot" "th" "thead" "time" "title" "tr" "tt" "u" "ul"
             "var" "video" "xmp") 'words))
  "Regex of all html4/5 tags.")

(defconst pug-selfclosing-tags-re
  (concat "^ *"
          (regexp-opt
           '("meta" "img" "area" "base" "br" "col" "command" "embed"
             "hr" "input" "link" "param" "source" "track" "wbr") t)))

(defconst pug-keywords-re
  (concat "^ *\\(?:- \\)?" (regexp-opt '("extends" "block") t)))

(defconst pug-control-re
  (concat "^ *\\(- \\)?\\("
          (regexp-opt
           '("if" "unless" "while" "until" "else" "for" "each" "in" "begin" 
             "elsif" "when" "default" "case" "var'"

             "extends" "block" "mixin"
             ) 'words)
          "\\)"))

(defconst pug-embedded-re "^ *:[a-z0-9_-]+"
  "Regexp matching filter and embedded elements.")

(defconst pug-comment-re "^ *-?//-?"
  "Regexp matching comment lines.")

(defconst pug-tag-declaration-char-re "[-a-zA-Z0-9_.#+]"
  "Regexp used to match a character in a tag declaration")


;; Helper for nested blocks (comment, embedded, text)
(defun pug-nested-re (re)
  (concat "^\\( *\\)" re "\\(\\(\n\\(?:\\1 +[^\n]*\\)?\\)*\\)"))

;; Font lock
;; TODO pug-mode specific faces?
(defconst pug-font-lock-keywords
  `(("^\\s-*[[:alnum:]_#.]"
     ;; id selectors
     ("\\(#[[:alnum:]_-]+\\)(?"
      (beginning-of-line) nil
      (1 font-lock-keyword-face append))
     ;; class selectors
     ("\\(\\.[a-z0-9_-]+\\)(?"
      (beginning-of-line) nil
      (1 font-lock-variable-name-face append))
     ;; Clear after : or selectors
     ("[[:alnum:]_)]\\(?::\\s-+[^ ]+\\|\\s-+\\)\\([^\n]*\\)"
      (beginning-of-line) nil
      (1 nil t)))
    ;; Tags
    (,pug-tags-re (1 font-lock-function-name-face))
    ;; +mixin invocation
    ("^ *\\+\\([a-z0-9_-]+\\)"
     0 font-lock-builtin-face)

    ;; comment block
    (,(pug-nested-re "-?//-?")
     (0 font-lock-comment-face))
    ;; comment line
    ("^ *\\(-//\\|//-?\\).*"
     (0 font-lock-comment-face prepend))
    ;; html comment block
    ("<!--.*-->"
     (0 font-lock-comment-face))
    ;; filters
    (,(pug-nested-re "\\(:[a-z0-9_]+\\)")
     (0 font-lock-preprocessor-face prepend))
    ;; block keywords
    (,pug-control-re
     (2 font-lock-keyword-face append))
    ;; "in" keyword in "each" statement
    ("each\\s-+\\w*\\s-+\\(in\\)" (1 font-lock-keyword-face))
    
    ;; Single quote string
    ("[^a-z]\\('[^'\n]*'\\)"
     1 font-lock-string-face append)
    ;; Double quoted string
    ("\\(\"[^\"]*\"\\)"
     1 font-lock-string-face append)

    ;; plain text block
    ;;(,(pug-nested-re "[\\.#+a-z][^ \t]*\\(?:(.+)\\)?\\(\\.\\)")
    ;; (3 font-lock-string-face t))

    ;; Plain text inline
    ("^\\s-*\\(|\\).*$"
     (1 font-lock-function-name-face t))

    ;; String interpolation
    ("[#!]{\\([^}]+\\)}"
     (1 font-lock-preprocessor-face))

    ;; Tag interpolation
    ("#\\[\\(\\sw+\\).*?\\]"
     (1 font-lock-function-name-face))
    
    ;; doctype
    ("^\\(doctype .*$\\)"
     1 font-lock-comment-face)
    ;; include/extends statements
    ("\\<\\(include\\|extends\\)\\(:[^ \t]+\\|\\s-+\\)\\([^\n]+\\)\n"
     (1 font-lock-keyword-face)
     (2 font-lock-preprocessor-face)
     (3 font-lock-string-face))

    ;; attributes
    ;; FIXME Doesn't take inline js or multiline attributes into account
    ("[a-z0-9-_]("
     ("\\(?:(\\|,?\\s-*\\)\\([[:alnum:]_-]+\\)\\(\\s-*=\\s-*\\('[^']+'\\|\"[^\"]+\"\\)\\)?"
      (backward-char) (forward-char)
      (1 font-lock-constant-face)))

    ;; ==', =', -
    ("^\\s-*\\(!?==?'?\\|-\\)\\s-"
     (1 font-lock-preprocessor-face)
     (,(regexp-opt
        '("if" "else" "elsif" "for" "in" "do" "unless"
          "while" "yield" "not" "and" "or" "return"
          "function" "var")
        'words) nil nil
        (0 font-lock-keyword-face)))
    ;; tag ==, tag =
    ("^\\s-*[.#a-z0-9_-]\\([#a-z0-9_.-]\\|([^)]*)\\)+\\(!?=\\)\\s-"
     (2 font-lock-preprocessor-face append)
     ("\\([[:alnum:]_]+\\)("
      nil nil
      (1 font-lock-function-name-face)))))

(cl-defun pug-extend-region ()
  "Extend the font-lock region to encompass embedded engines and comments."
  (let ((old-beg font-lock-beg)
        (old-end font-lock-end))
    (save-excursion
      (goto-char font-lock-beg)
      (unless (looking-at "\\.$")
        (beginning-of-line)
        (unless (or (looking-at pug-embedded-re)
                    (looking-at pug-comment-re))
          (cl-return-from pug-extend-region)))
      (setq font-lock-beg (point))
      (pug-forward-sexp)
      (beginning-of-line)
      (setq font-lock-end (max font-lock-end (point))))
    (or (/= old-beg font-lock-beg)
        (/= old-end font-lock-end))))

(defun pug-goto-end-of-tag ()
  "Skip ahead over whitespace, tag characters (defined in
`pug-tag-declaration-char-re'), and paren blocks (using
`forward-sexp') to put point at the end of a full tag declaration (but
before its content). Use when point is inside or to the left of a tag
declaration"
  (interactive)

  ;; skip indentation characters
  (while (looking-at "[ \t]")
    (forward-char 1))

  (while (looking-at pug-tag-declaration-char-re)
    (forward-char 1))
  (if (looking-at "(")
      (forward-sexp 1)))

;; Mode setup
(defvar pug-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?= " " table)
    (modify-syntax-entry ?# "." table)
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?# "." table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table in use in pug-mode buffers.")

(defvar pug-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\177" 'pug-electric-backspace)
    (define-key map "\C-?" 'pug-electric-backspace)
    (define-key map "\C-c\C-f" 'pug-forward-sexp)
    (define-key map "\C-c\C-b" 'pug-backward-sexp)
    (define-key map "\C-c\C-u" 'pug-up-list)
    (define-key map "\C-c\C-d" 'pug-down-list)
    (define-key map "\C-c\C-k" 'pug-kill-line-and-indent)
    map))

;; For compatibility with Emacs < 24, derive conditionally
(defalias 'pug-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode pug-mode pug-parent-mode "Pug"
  "Major mode for editing Pug files.

\\{pug-mode-map}"
  (set-syntax-table pug-mode-syntax-table)
  (add-to-list 'font-lock-extend-region-functions 'pug-extend-region)
  (setq-local font-lock-multiline t)
  (setq-local indent-line-function 'pug-indent-line)
  (setq-local indent-region-function 'pug-indent-region)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local electric-indent-chars '(?| ?+))
  (setq-local comment-start "//")
  (setq-local comment-start-skip "//+ *")
  (setq-local comment-end "")
  (setq-local comment-end-skip "[ 	]*\\(\\s>\\|\n\\)")
  (setq-local indent-tabs-mode nil)
  (setq font-lock-defaults '((pug-font-lock-keywords) nil t)))

;; Useful functions
(defun pug-comment-block ()
  "Comment the current block of Pug code."
  (interactive)
  (save-excursion
    (let ((indent (current-indentation)))
      (back-to-indentation)
      (insert "//")
      (newline)
      (indent-to indent)
      (beginning-of-line)
      (pug-mark-sexp)
      (pug-reindent-region-by tab-width))))

(defun pug-uncomment-block ()
  "Uncomment the current block of Pug code."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (not (looking-at pug-comment-re))
      (pug-up-list)
      (beginning-of-line))
    (pug-mark-sexp)
    (kill-line 1)
    (pug-reindent-region-by (- tab-width))))

;; Navigation
(defun pug-forward-through-whitespace (&optional backward)
  "Move the point forward at least one line, until it reaches either the end of
the buffer or a line with no whitespace.

If `backward' is non-nil, move the point backward instead."
  (let ((arg (if backward -1 1))
        (endp (if backward 'bobp 'eobp)))
    (cl-loop do (forward-line arg)
             while (and (not (funcall endp))
                        (looking-at "^[ \t]*$")))))

(defun pug-at-indent-p ()
  "Returns whether or not the point is at the first non-whitespace character in
a line or whitespace preceding that character."
  (let ((opoint (point)))
    (save-excursion
      (back-to-indentation)
      (>= (point) opoint))))

(defun pug-forward-sexp (&optional arg)
  "Move forward across one nested expression. With `arg', do it that many times.
Negative arg -N means move backward across N balanced expressions.

A sexp in Pug is defined as a line of Pug code as well as any lines nested
beneath it."
  (interactive "p")
  (or arg (setq arg 1))
  (if (and (< arg 0) (not (pug-at-indent-p)))
      (back-to-indentation)
    (while (/= arg 0)
      (let ((indent (current-indentation)))
        (cl-loop do (pug-forward-through-whitespace (< arg 0))
                 while (and (not (eobp))
                            (not (bobp))
                            (> (current-indentation) indent)))
        (back-to-indentation)
        (setq arg (+ arg (if (> arg 0) -1 1)))))))

(defun pug-backward-sexp (&optional arg)
  "Move backward across one nested expression. With ARG, do it that many times.
Negative arg -N means move forward across N balanced expressions.

A sexp in Pug is defined as a line of Pug code as well as any lines nested
beneath it."
  (interactive "p")
  (pug-forward-sexp (if arg (- arg) -1)))

(defun pug-up-list (&optional arg)
  "Move out of one level of nesting. With ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (let ((indent (current-indentation)))
      (cl-loop do (pug-forward-through-whitespace t)
               while (and (not (bobp))
                          (>= (current-indentation) indent)))
      (setq arg (- arg 1))))
  (back-to-indentation))

(defun pug-down-list (&optional arg)
  "Move down one level of nesting. With ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (let ((indent (current-indentation)))
      (pug-forward-through-whitespace)
      (when (<= (current-indentation) indent)
        (pug-forward-through-whitespace t)
        (back-to-indentation)
        (error "Nothing is nested beneath this line"))
      (setq arg (- arg 1))))
  (back-to-indentation))

(defun pug-mark-sexp ()
  "Marks the next Pug block."
  (let ((forward-sexp-function 'pug-forward-sexp))
    (mark-sexp)))

(defun pug-mark-sexp-but-not-next-line ()
  "Marks the next Pug block, but puts the mark at the end of the last line of
the sexp rather than the first non-whitespace character of the next line."
  (pug-mark-sexp)
  (let ((pos-of-end-of-line (save-excursion
                              (goto-char (mark))
                              (end-of-line)
                              (point))))
    (when (/= pos-of-end-of-line (mark))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (forward-line -1)
         (end-of-line)
         (point))))))

;; Indentation and electric keys
(defun pug-indent-p ()
  "Returns true if the current line can have lines nested beneath it."
  ;; FIXME Optimize
  (or (looking-at-p pug-comment-re)
      (looking-at-p pug-embedded-re)
      (and (save-excursion
             (back-to-indentation)
             (not (memq (face-at-point) '(font-lock-preprocessor-face))))
           (not (looking-at-p pug-selfclosing-tags-re))
           (cl-loop for opener in `(,(concat "\\(^ *[\\.#+]\\|" pug-tags-re "\\)[^ \t]*\\((.+)\\)?\n")
                                    "^ *[\\.#+a-z][^ \t]*\\(?:(.+)\\)?\\.\n"
                                    "^ *[-=].*do[ \t]*\\(|.*|[ \t]*\\)?$"
                                    ,pug-control-re)
                    if (looking-at-p opener) return t
                    finally return nil))))

(defun pug-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (bobp) 0
      (pug-forward-through-whitespace t)
      (+ (current-indentation)
         (if (funcall pug-indent-function)
             tab-width
           0)))))

(defun pug-indent-region (start end)
  "Indent each nonblank line in the region. This is done by indenting the first
line based on `pug-compute-indentation' and preserving the relative indentation
of the rest of the region.

If this command is used multiple times in a row, it will cycle between possible
indentations."
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (let (this-line-column current-column
          (next-line-column
           (if (and (equal last-command this-command) (/= (current-indentation) 0))
               (* (/ (- (current-indentation) 1) tab-width) tab-width)
             (pug-compute-indentation))))
      (while (< (point) end)
        (setq this-line-column next-line-column
              current-column (current-indentation))
        ;; Delete whitespace chars at beginning of line
        (delete-horizontal-space)
        (unless (eolp)
          (setq next-line-column (save-excursion
                                   (cl-loop do (forward-line 1)
                                            while (and (not (eobp)) (looking-at "^[ \t]*$")))
                                   (+ this-line-column
                                      (- (current-indentation) current-column))))
          ;; Don't indent an empty line
          (unless (eolp) (indent-to this-line-column)))
        (forward-line 1)))
    (move-marker end nil)))

(defun pug-indent-line ()
  "Indent the current line. The first time this command is used, the line will
be indented to the maximum sensible indentation. Each immediately subsequent
usage will back-dent the line by `tab-width' spaces. On reaching column 0, it
will cycle back to the maximum sensible indentation."
  (interactive "*")
  (let ((ci (current-indentation))
        (cc (current-column))
        (need (pug-compute-indentation)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (if (and (equal last-command this-command) (/= ci 0))
          (indent-to (* (/ (- ci 1) tab-width) tab-width))
        (indent-to need)))
      (if (< (current-column) (current-indentation))
          (forward-to-indentation 0))))

(defun pug-reindent-region-by (n)
  "Add N spaces to the beginning of each line in the region. If N is negative,
will remove the spaces instead. Assumes all lines in the region have indentation
>= that of the first line."
  (let ((ci (current-indentation))
        (bound (mark)))
    (save-excursion
      (while (re-search-forward (concat "^" (make-string ci ? )) bound t)
        (replace-match (make-string (max 0 (+ ci n)) ? ) bound nil)))))

(defun pug-electric-backspace (arg)
  "Delete characters or back-dent the current line. If invoked following only
whitespace on a line, will back-dent the line and all nested lines to the
immediately previous multiple of `tab-width' spaces.

Set `pug-backspace-backdents-nesting' to nil to just back-dent the current
line."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
          (bolp)
          (looking-at "^[ \t]+$"))
      (backward-delete-char-untabify arg)
    (save-excursion
      (let ((ci (current-column)))
        (beginning-of-line)
        (if pug-backspace-backdents-nesting
            (pug-mark-sexp-but-not-next-line)
          (set-mark (save-excursion (end-of-line) (point))))
        (pug-reindent-region-by (* (- arg) tab-width))
        (back-to-indentation)
        (pop-mark)))))

(defun pug-kill-line-and-indent ()
  "Kill the current line, and re-indent all lines nested beneath it."
  (interactive)
  (beginning-of-line)
  (pug-mark-sexp-but-not-next-line)
  (kill-line 1)
  (pug-reindent-region-by (* -1 tab-width)))

(defun pug-indent-string ()
  "Return the indentation string for `tab-width'."
  (mapconcat 'identity (make-list tab-width " ") ""))

;;;###autoload
(defun pug-compile ()
  (interactive)
  (if (memq major-mode '(pug-mode jade-mode))
      (compile (format "pug %s" buffer-file-name))
    (user-error "Not in a pug-mode buffer")))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(jade\\|pug\\)\\'" . pug-mode))

(provide 'pug-mode)
;;; pug-mode.el ends here
