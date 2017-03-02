;;; slim-mode.el --- Major mode for editing Slim files

;; Copyright (c) 2007, 2008 Nathan Weizenbaum
;; Copyright (c) 2009-2013 Daniel Mendler
;; Copyright (c) 2012-2014 Bozhidar Batsov

;; Author: Nathan Weizenbaum
;; Author: Daniel Mendler
;; Author: Bozhidar Batsov
;; URL: http://github.com/slim-template/emacs-slim
;; Package-Version: 20140611.950
;; Version: 1.1
;; Keywords: markup, language

;;; Commentary:

;; Because Slim's indentation schema is similar
;; to that of YAML and Python, many indentation-related
;; functions are similar to those in yaml-mode and python-mode.

;; To install, save this on your load path and add the following to
;; your .emacs file:

;;
;; (require 'slim-mode)

;;; Code:

(eval-when-compile
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (require 'cl))

;; User definable variables

(defgroup slim nil
  "Support for the Slim template language."
  :group 'languages
  :prefix "slim-")

(defcustom slim-mode-hook nil
  "Hook run when entering Slim mode."
  :type 'hook
  :group 'slim)

(defcustom slim-indent-offset 2
  "Amount of offset per level of indentation."
  :type 'integer
  :group 'slim)

(defcustom slim-backspace-backdents-nesting t
  "Non-nil to have `slim-electric-backspace' re-indent all code
nested beneath the backspaced line be re-indented along with the
line itself."
  :type 'boolean
  :group 'slim)

(defvar slim-indent-function 'slim-indent-p
  "This function should look at the current line and return true
if the next line could be nested within this line.")

(defvar slim-block-openers
  `("^ *\\([\\.#a-z][^ \t]*\\)\\(\\[.*\\]\\)?"
    "^ *[-=].*do[ \t]*\\(|.*|[ \t]*\\)?$"
    ,(concat "^ *-[ \t]*\\("
             (regexp-opt '("if" "unless" "while" "until" "else"
                           "begin" "elsif" "rescue" "ensure" "when"))
             "\\)")
    "^ *|"
    "^ */"
    "^ *[a-z0-9_]:")
  "A list of regexps that match lines of Slim that could have
text nested beneath them.")

;; Font lock

;; Helper for nested block (comment, embedded, text)
(defun slim-nested-re (re)
  (concat "^\\( *\\)" re "\n\\(?:\\(?:\\1 .*\\)\n\\)*"))

(defvar html-tags
  '("a" "abbr" "acronym" "address" "applet" "area" "article" "aside"
    "audio" "b" "base" "basefont" "bdo" "big" "blockquote" "body"
    "br" "button" "canvas" "caption" "center" "cite" "code" "col"
    "colgroup" "command" "datalist" "dd" "del" "details" "dialog" "dfn"
    "dir" "div" "dl" "dt" "em" "embed" "fieldset" "figure" "font" "footer"
    "form" "frame" "frameset" "h1" "h2" "h3" "h4" "h5" "h6"
    "head" "header" "hgroup" "hr" "html" "i"
    "iframe" "img" "input" "ins" "keygen" "kbd" "label" "legend" "li" "link"
    "map" "mark" "menu" "meta" "meter" "nav" "noframes" "noscript" "object"
    "ol" "optgroup" "option" "output" "p" "param" "pre" "progress" "q" "rp"
    "rt" "ruby" "s" "samp" "script" "section" "select" "small" "source" "span"
    "strike" "strong" "style" "sub" "sup" "table" "tbody" "td" "textarea" "tfoot"
    "th" "thead" "time" "title" "tr" "tt" "u" "ul" "var" "video" "xmp")
  "A list of all valid HTML4/5 tag names.")

(defvar html-tags-re (concat "^ *\\(" (regexp-opt html-tags 'words) "\/?\\)"))

(defconst slim-font-lock-keywords
  `(;; comment block
    (,(slim-nested-re "/.*")
     0 font-lock-comment-face)
    ;; embedded block
    (,(slim-nested-re "\\([a-z0-9_]+:\\)")
     0 font-lock-preprocessor-face)
    ;; text block
    (,(slim-nested-re "[\|'`].*")
     0 font-lock-string-face)
    ;; directive
    ("^!.*"
     0 font-lock-constant-face)
    ;; Single quote string TODO
    ("[^=]\\('[^'\n]*'\\)"
     1 font-lock-string-face append)
    ;; Double quoted string TODO
    ("\\(\"[^\"]*\"\\)"
     1 font-lock-string-face append)
    ;; Class variable TODO
    ("@[a-z0-9_]+"
     0 font-lock-variable-name-face append)
    ;; @var.method
    ("@[a-z0-9_]+"
     (0 font-lock-variable-name-face)
     ("\\.[a-z0-9_-]+" nil nil
      (0 font-lock-variable-name-face)))
    ;; ruby symbol
    (":\\w+" . font-lock-constant-face)
    ;; ruby symbol (1.9)
    ("\\w+:" . font-lock-constant-face)
    ;; #id
    ("^ *[a-z0-9_.-]*\\(#[a-z0-9_-]+\/?\\)"
     1 font-lock-keyword-face)
    ;; .class
    ("^ *[a-z0-9_#-]*\\(\\(\\.[a-z0-9_-]+\/?\\)+\\)"
     1 font-lock-type-face)
    ;; tag
    (,html-tags-re
     1 font-lock-function-name-face)
    ;; doctype
    ("^\\(doctype .*$\\)"
     1 font-lock-preprocessor-face)
    ;; ==', =', -
    ("^ *\\(==?'?\\|-\\)"
      (1 font-lock-preprocessor-face)
      (,(regexp-opt
         '("if" "else" "elsif" "for" "in" "do" "unless"
           "while" "yield" "not" "and" "or")
         'words) nil nil
           (0 font-lock-keyword-face)))
    ;; tag ==, tag =
    ("^ *[\\.#a-z0-9_-]+.*[^<>!]\\(==?'?\\) +"
     1 font-lock-preprocessor-face)))

(defconst slim-embedded-re "^ *[a-z0-9_-]+:")
(defconst slim-comment-re  "^ */")

(defun* slim-extend-region ()
  "Extend the font-lock region to encompass embedded engines and comments."
  (let ((old-beg font-lock-beg)
        (old-end font-lock-end))
    (save-excursion
      (goto-char font-lock-beg)
      (beginning-of-line)
      (unless (or (looking-at slim-embedded-re)
                  (looking-at slim-comment-re))
        (return-from slim-extend-region))
      (setq font-lock-beg (point))
      (slim-forward-sexp)
      (beginning-of-line)
      (setq font-lock-end (max font-lock-end (point))))
    (or (/= old-beg font-lock-beg)
        (/= old-end font-lock-end))))


;; Mode setup

(defvar slim-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table in use in slim-mode buffers.")

(defvar slim-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\177" 'slim-electric-backspace)
    (define-key map "\C-?" 'slim-electric-backspace)
    (define-key map "\C-c\C-f" 'slim-forward-sexp)
    (define-key map "\C-c\C-b" 'slim-backward-sexp)
    (define-key map "\C-c\C-u" 'slim-up-list)
    (define-key map "\C-c\C-d" 'slim-down-list)
    (define-key map "\C-c\C-k" 'slim-kill-line-and-indent)
    map))

;; For compatibility with Emacs < 24, derive conditionally
(defalias 'slim-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode slim-mode slim-parent-mode "Slim"
  "Major mode for editing Slim files.

\\{slim-mode-map}"
  (set-syntax-table slim-mode-syntax-table)
  (add-to-list 'font-lock-extend-region-functions 'slim-extend-region)
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'indent-line-function) 'slim-indent-line)
  (set (make-local-variable 'indent-region-function) 'slim-indent-region)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'electric-indent-chars) nil)
  (setq comment-start "/")
  (setq indent-tabs-mode nil)
  (setq font-lock-defaults '((slim-font-lock-keywords) nil t)))

;; Useful functions

(defun slim-comment-block ()
  "Comment the current block of Slim code."
  (interactive)
  (save-excursion
    (let ((indent (current-indentation)))
      (back-to-indentation)
      (insert "/")
      (newline)
      (indent-to indent)
      (beginning-of-line)
      (slim-mark-sexp)
      (slim-reindent-region-by slim-indent-offset))))

(defun slim-uncomment-block ()
  "Uncomment the current block of Slim code."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (not (looking-at slim-comment-re))
      (slim-up-list)
      (beginning-of-line))
    (slim-mark-sexp)
    (kill-line 1)
    (slim-reindent-region-by (- slim-indent-offset))))

;; Navigation

(defun slim-forward-through-whitespace (&optional backward)
  "Move the point forward at least one line, until it reaches
either the end of the buffer or a line with no whitespace.

If `backward' is non-nil, move the point backward instead."
  (let ((arg (if backward -1 1))
        (endp (if backward 'bobp 'eobp)))
    (loop do (forward-line arg)
          while (and (not (funcall endp))
                     (looking-at "^[ \t]*$")))))

(defun slim-at-indent-p ()
  "Returns whether or not the point is at the first
non-whitespace character in a line or whitespace preceding that
character."
  (let ((opoint (point)))
    (save-excursion
      (back-to-indentation)
      (>= (point) opoint))))

(defun slim-forward-sexp (&optional arg)
  "Move forward across one nested expression.
With `arg', do it that many times.  Negative arg -N means move
backward across N balanced expressions.

A sexp in Slim is defined as a line of Slim code as well as any
lines nested beneath it."
  (interactive "p")
  (or arg (setq arg 1))
  (if (and (< arg 0) (not (slim-at-indent-p)))
      (back-to-indentation)
    (while (/= arg 0)
      (let ((indent (current-indentation)))
        (loop do (slim-forward-through-whitespace (< arg 0))
              while (and (not (eobp))
                         (not (bobp))
                         (> (current-indentation) indent)))
        (back-to-indentation)
        (setq arg (+ arg (if (> arg 0) -1 1)))))))

(defun slim-backward-sexp (&optional arg)
  "Move backward across one nested expression.
With ARG, do it that many times.  Negative arg -N means move
forward across N balanced expressions.

A sexp in Slim is defined as a line of Slim code as well as any
lines nested beneath it."
  (interactive "p")
  (slim-forward-sexp (if arg (- arg) -1)))

(defun slim-up-list (&optional arg)
  "Move out of one level of nesting.
With ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (let ((indent (current-indentation)))
      (loop do (slim-forward-through-whitespace t)
            while (and (not (bobp))
                       (>= (current-indentation) indent)))
      (setq arg (- arg 1))))
  (back-to-indentation))

(defun slim-down-list (&optional arg)
  "Move down one level of nesting.
With ARG, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (let ((indent (current-indentation)))
      (slim-forward-through-whitespace)
      (when (<= (current-indentation) indent)
        (slim-forward-through-whitespace t)
        (back-to-indentation)
        (error "Nothing is nested beneath this line"))
      (setq arg (- arg 1))))
  (back-to-indentation))

(defun slim-mark-sexp ()
  "Marks the next Slim block."
  (let ((forward-sexp-function 'slim-forward-sexp))
    (mark-sexp)))

(defun slim-mark-sexp-but-not-next-line ()
  "Marks the next Slim block, but puts the mark at the end of the
last line of the sexp rather than the first non-whitespace
character of the next line."
  (slim-mark-sexp)
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

(defun slim-indent-p ()
  "Returns true if the current line can have lines nested beneath it."
  (loop for opener in slim-block-openers
        if (looking-at opener) return t
        finally return nil))

(defun slim-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (bobp) 0
      (slim-forward-through-whitespace t)
      (+ (current-indentation)
         (if (funcall slim-indent-function) slim-indent-offset
           0)))))

(defun slim-indent-region (start end)
  "Indent each nonblank line in the region.
This is done by indenting the first line based on
`slim-compute-indentation' and preserving the relative
indentation of the rest of the region.

If this command is used multiple times in a row, it will cycle
between possible indentations."
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (let (this-line-column current-column
          (next-line-column
           (if (and (equal last-command this-command) (/= (current-indentation) 0))
               (* (/ (- (current-indentation) 1) slim-indent-offset) slim-indent-offset)
             (slim-compute-indentation))))
      (while (< (point) end)
        (setq this-line-column next-line-column
              current-column (current-indentation))
        ;; Delete whitespace chars at beginning of line
        (delete-horizontal-space)
        (unless (eolp)
          (setq next-line-column (save-excursion
                                   (loop do (forward-line 1)
                                         while (and (not (eobp)) (looking-at "^[ \t]*$")))
                                   (+ this-line-column
                                      (- (current-indentation) current-column))))
          ;; Don't indent an empty line
          (unless (eolp) (indent-to this-line-column)))
        (forward-line 1)))
    (move-marker end nil)))

(defun slim-indent-line ()
  "Indent the current line.
The first time this command is used, the line will be indented to the
maximum sensible indentation.  Each immediately subsequent usage will
back-dent the line by `slim-indent-offset' spaces.  On reaching column
0, it will cycle back to the maximum sensible indentation."
  (interactive "*")
  (let ((ci (current-indentation))
        (cc (current-column))
        (need (slim-compute-indentation)))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (if (and (equal last-command this-command) (/= ci 0))
          (indent-to (* (/ (- ci 1) slim-indent-offset) slim-indent-offset))
        (indent-to need)))
      (if (< (current-column) (current-indentation))
          (forward-to-indentation 0))))

(defun slim-reindent-region-by (n)
  "Add N spaces to the beginning of each line in the region.
If N is negative, will remove the spaces instead.  Assumes all
lines in the region have indentation >= that of the first line."
  (let ((ci (current-indentation))
        (bound (mark)))
    (save-excursion
      (while (re-search-forward (concat "^" (make-string ci ? )) bound t)
        (replace-match (make-string (max 0 (+ ci n)) ? ) bound nil)))))

(defun slim-electric-backspace (arg)
  "Delete characters or back-dent the current line.
If invoked following only whitespace on a line, will back-dent
the line and all nested lines to the immediately previous
multiple of `slim-indent-offset' spaces.

Set `slim-backspace-backdents-nesting' to nil to just back-dent
the current line."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
          (bolp)
          (looking-at "^[ \t]+$"))
      (backward-delete-char-untabify arg)
    (save-excursion
      (let ((ci (current-column)))
        (beginning-of-line)
        (if slim-backspace-backdents-nesting
            (slim-mark-sexp-but-not-next-line)
          (set-mark (save-excursion (end-of-line) (point))))
        (slim-reindent-region-by (* (- arg) slim-indent-offset))
        (back-to-indentation)
        (pop-mark)))))

(defun slim-kill-line-and-indent ()
  "Kill the current line, and re-indent all lines nested beneath it."
  (interactive)
  (beginning-of-line)
  (slim-mark-sexp-but-not-next-line)
  (kill-line 1)
  (slim-reindent-region-by (* -1 slim-indent-offset)))

(defun slim-indent-string ()
  "Return the indentation string for `slim-indent-offset'."
  (mapconcat 'identity (make-list slim-indent-offset " ") ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.slim\\'" . slim-mode))

;; Setup/Activation
(provide 'slim-mode)
;;; slim-mode.el ends here
