;;; evil-lisp-state.el --- An evil state to edit Lisp code

;; Copyright (C) 2014, 2015 syl20bnr
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: convenience editing evil smartparens lisp mnemonic
;; Package-Version: 20160403.1948
;; Created: 9 Oct 2014
;; Version: 8.2
;; Package-Requires: ((evil "1.0.9") (bind-map "0") (smartparens "1.6.1"))
;; URL: https://github.com/syl20bnr/evil-lisp-state

;; This file is not part of GNU Emacs.

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

;; Adds a new Evil state called --LISP-- (<L>) with mnemonics key bindings
;; to navigate Lisp code and edit the sexp tree.

;; Principle:
;; ----------

;; To execute a command while in normal state, a leader is used.
;; The leader has to be defined with the function `evil-lisp-state-leader'.
;; By default, any command when executed sets the current state to
;; `lisp state`.
;;
;; For example, to slurp three times while in normal state:
;;     <leader> 3 s
;; Or to wrap a symbol in parenthesis then slurping two times:
;;     <leader> w 2 s
;;
;; Key Binding  | Function
;; -------------|------------------------------------------------------------
;; `leader .'   | switch to `lisp state'
;; `leader %'   | evil jump item
;; `leader :'   | ex command
;; `leader ('   | insert expression before (same level as current one)
;; `leader )'   | insert expression after (same level as current one)
;; `leader $'   | go to the end of current sexp
;; `leader ` k' | hybrid version of kill sexp (can be used in non lisp dialects)
;; `leader ` p' | hybrid version of push sexp (can be used in non lisp dialects)
;; `leader ` s' | hybrid version of slurp sexp (can be used in non lisp dialects)
;; `leader ` t' | hybrid version of transpose sexp (can be used in non lisp dialects)
;; `leader 0'   | go to the beginning of current sexp
;; `leader a'   | absorb expression
;; `leader b'   | forward barf expression
;; `leader B'   | backward barf expression
;; `leader c'   | convolute expression
;; `leader ds'  | delete symbol
;; `leader dw'  | delete word
;; `leader dx'  | delete expression
;; `leader e'   | (splice) unwrap current expression and kill all symbols after point
;; `leader E'   | (splice) unwrap current expression and kill all symbols before point
;; `leader h'   | previous symbol
;; `leader H'   | go to previous sexp
;; `leader i'   | switch to `insert state`
;; `leader I'   | go to beginning of current expression and switch to `insert state`
;; `leader j'   | next closing parenthesis/bracket/brace
;; `leader J'   | join expression
;; `leader k'   | previous opening parenthesis/bracket/brace
;; `leader l'   | next symbol
;; `leader L'   | go to next sexp
;; `leader p'   | paste after
;; `leader P'   | paste before
;; `leader r'   | raise expression (replace parent expression by current one)
;; `leader s'   | forwared slurp expression
;; `leader S'   | backward slurp expression
;; `leader t'   | transpose expression
;; `leader u'   | undo
;; `leader U'   | got to parent sexp backward
;; `leader C-r' | redo
;; `leader v'   | switch to `visual state`
;; `leader V'   | switch to `visual line state`
;; `leader C-v' | switch to `visual block state`
;; `leader w'   | wrap expression with parenthesis
;; `leader W'   | unwrap expression
;; `leader y'   | copy expression

;; Configuration:
;; --------------

;; No default binding comes with the package, you have to explicitly
;; bind the lisp state to a key with the function `evil-lisp-state-leader'
;; For instance: `(evil-lisp-state-leader ", l")'

;; Key bindings are set only for `emacs-lisp-mode' by default.
;; It is possible to add major modes with the variable
;; `evil-lisp-state-major-modes'.

;; It is also possible to define the key bindings globally by
;; setting `evil-lisp-state-global' to t. In this case
;; `evil-lisp-state-major-modes' has no effect.

;; If you don't want commands to enter in `lisp state' by default
;; set the variable `evil-lisp-state-enter-lisp-state-on-command'
;; to nil. Then use the `.' to enter manually in `lisp state'

;;; Code:

(require 'evil)
(require 'smartparens)
(require 'bind-map)

(evil-define-state lisp
  "Lisp state.
 Used to navigate lisp code and manipulate the sexp tree."
  :tag " <L> "
  :suppress-keymap t
  :cursor (bar . 2)
  ;; force smartparens mode
  (if (evil-lisp-state-p) (smartparens-mode)))

(defgroup evil-lisp-state nil
  "Evil lisp state."
  :group 'emulations
  :prefix 'evil-lisp-state-)

(eval-and-compile
  (defcustom evil-lisp-state-global nil
    "If non nil evil-lisp-state is available everywhere."
    :type 'boolean
    :group 'evil-lisp-state)

  (defcustom evil-lisp-state-major-modes '(emacs-lisp-mode)
    "Major modes where evil leader key bindings are defined.
If `evil-lisp-state-global' is non nil then this variable has no effect."
    :type 'sexp
    :group 'evil-lisp-state)

  (defcustom evil-lisp-state-enter-lisp-state-on-command t
    "If non nil, enter evil-lisp-state before executing command."
    :type 'sexp
    :group 'evil-lisp-state))

(defvar evil-lisp-state-default-state 'normal
  "The state to activate when exiting lisp state")

(defmacro evil-lisp-state-enter-command (command)
  "Wrap COMMAND to call evil-lisp-state before executing COMMAND."
  (let ((funcname (if (string-match "lisp-state-"
                                    (symbol-name command))
                      (intern (format "evil-%s" command))
                    (intern (format "evil-lisp-state-%s" command)))))
    `(progn
       (defun ,funcname ()
        (interactive)
        (when evil-lisp-state-enter-lisp-state-on-command
          (evil-lisp-state))
        (call-interactively ',command))
       ',funcname)))

(defun evil-lisp-state-escape-command (command)
  "Wrap COMMAND to escape to normal state before executing COMMAND."
  `(lambda ()
     (interactive)
     (evil-normal-state)
     (call-interactively ',command)))


;; leader maps
(defun evil-lisp-state-leader (leader)
  "Set LEADER."
  (bind-map evil-lisp-state-map
    :evil-use-local t
    :evil-keys (leader)
    :evil-states (normal))
  (eval
   `(bind-map evil-lisp-state-major-mode-map
      :evil-keys (,leader)
      :evil-states (normal)
      :major-modes ,evil-lisp-state-major-modes)))

(defun evil-lisp-state/quit ()
  "Quit lisp state and set state `evil-lisp-state-default-state'."
  (interactive)
  (funcall (intern (format "evil-%S-state" evil-lisp-state-default-state))))

;; escape
(define-key evil-lisp-state-map [escape] 'evil-lisp-state/quit)
;; toggle lisp state
(define-key evil-lisp-state-map "." 'lisp-state-toggle-lisp-state)
;; hjkl
(define-key evil-lisp-state-map "h" 'evil-backward-char)
(define-key evil-lisp-state-map "j" 'evil-next-visual-line)
(define-key evil-lisp-state-map "k" 'evil-previous-visual-line)
(define-key evil-lisp-state-map "l" 'evil-forward-char)

;; auto-switch to lisp state commands
(defconst evil-lisp-state-commands
  `(("%"   . evil-jump-item)
    (":"   . evil-ex)
    ("("   . lisp-state-insert-sexp-before)
    (")"   . lisp-state-insert-sexp-after)
    ("$"   . sp-end-of-sexp)
    ("`k"  . sp-kill-hybrid-sexp)
    ("`p"  . sp-push-hybrid-sexp)
    ("`s"  . sp-slurp-hybrid-sexp)
    ("`t"  . sp-transpose-hybrid-sexp)
    ("0"   . lisp-state-beginning-of-sexp)
    ("1"   . digit-argument)
    ("2"   . digit-argument)
    ("3"   . digit-argument)
    ("4"   . digit-argument)
    ("5"   . digit-argument)
    ("6"   . digit-argument)
    ("7"   . digit-argument)
    ("8"   . digit-argument)
    ("9"   . digit-argument)
    ("a"   . sp-absorb-sexp)
    ("b"   . sp-forward-barf-sexp)
    ("B"   . sp-backward-barf-sexp)
    ("c"   . sp-convolute-sexp)
    ("ds"  . sp-kill-symbol)
    ("Ds"  . sp-backward-kill-symbol)
    ("dw"  . sp-kill-word)
    ("Dw"  . sp-backward-kill-word)
    ("dx"  . sp-kill-sexp)
    ("Dx"  . sp-backward-kill-sexp)
    ("e"   . sp-splice-sexp-killing-forward)
    ("E"   . sp-splice-sexp-killing-backward)
    ("h"   . sp-backward-symbol)
    ("H"   . sp-backward-sexp)
    ("i"   . evil-insert-state)
    ("I"   . evil-insert-line)
    ("j"   . lisp-state-next-closing-paren)
    ("J"   . sp-join-sexp)
    ("k"   . lisp-state-prev-opening-paren)
    ("l"   . lisp-state-forward-symbol)
    ("L"   . sp-forward-sexp)
    ("p"   . evil-paste-after)
    ("P"   . evil-paste-before)
    ("r"   . sp-raise-sexp)
    ("s"   . sp-forward-slurp-sexp)
    ("S"   . sp-backward-slurp-sexp)
    ("t"   . sp-transpose-sexp)
    ("u"   . undo-tree-undo)
    ("U"   . sp-backward-up-sexp)
    ("C-r" . undo-tree-redo)
    ("v"   . evil-visual-char)
    ("V"   . evil-visual-line)
    ("C-v" . evil-visual-block)
    ("w"   . lisp-state-wrap)
    ("W"   . sp-unwrap-sexp)
    ("y"   . sp-copy-sexp))
  "alist of keys and commands in lisp state.")

(defvar evil-lisp-state-major-mode-map (make-sparse-keymap))

(dolist (x evil-lisp-state-commands)
  (let ((key (car x))
        (cmd (cdr x)))
    (eval
     `(progn
        (if evil-lisp-state-global
            (define-key evil-lisp-state-map ,(kbd key)
              (evil-lisp-state-enter-command ,cmd))
          (define-key evil-lisp-state-major-mode-map ,(kbd key)
            (evil-lisp-state-enter-command ,cmd)))))))

(defun lisp-state-toggle-lisp-state ()
  "Toggle the lisp state."
  (interactive)
  (if (eq 'lisp evil-state)
      (progn
        (message "state: lisp -> normal")
        (evil-normal-state))
    (message "state: %s -> lisp" evil-state)
    (evil-lisp-state)))
(defun lisp-state-wrap (&optional arg)
  "Wrap a symbol with parenthesis."
  (interactive "P")
  (sp-wrap-with-pair "("))

(defun evil-lisp-state-next-paren (&optional closing)
  "Go to the next/previous closing/opening parenthesis/bracket/brace."
  (if closing
      (let ((curr (point)))
        (forward-char)
        (unless (eq curr (search-forward-regexp "[])}]"))
          (backward-char)))
    (search-backward-regexp "[[({]")))

(defun lisp-state-prev-opening-paren ()
  "Go to the next closing parenthesis."
  (interactive)
  (evil-lisp-state-next-paren))

(defun lisp-state-next-closing-paren ()
  "Go to the next closing parenthesis."
  (interactive)
  (evil-lisp-state-next-paren 'closing))

(defun lisp-state-forward-symbol (&optional arg)
  "Go to the beginning of the next symbol."
  (interactive "P")
  (let ((n (if (char-equal (char-after) ?\() 1 2)))
    (sp-forward-symbol (+ (if arg arg 0) n))
    (sp-backward-symbol)))

(defun lisp-state-insert-sexp-after ()
  "Insert sexp after the current one."
  (interactive)
  (let ((sp-navigate-consider-symbols nil))
    (if (char-equal (char-after) ?\() (forward-char))
    (sp-up-sexp)
    (evil-insert-state)
    (sp-newline)
    (sp-insert-pair "(")))

(defun lisp-state-insert-sexp-before ()
  "Insert sexp before the current one."
  (interactive)
  (let ((sp-navigate-consider-symbols nil))
    (if (char-equal (char-after) ?\() (forward-char))
    (sp-backward-sexp)
    (evil-insert-state)
    (sp-newline)
    (evil-previous-visual-line)
    (evil-end-of-line)
    (insert " ")
    (sp-insert-pair "(")
    (indent-for-tab-command)))

(defun lisp-state-eval-sexp-end-of-line ()
  "Evaluate the last sexp at the end of the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (eval-last-sexp nil)))

(defun lisp-state-beginning-of-sexp (&optional arg)
  "Go to the beginning of current s-exp"
  (interactive "P")
  (sp-beginning-of-sexp)
  (evil-backward-char))


(provide 'evil-lisp-state)

;;; evil-lisp-state.el ends here
