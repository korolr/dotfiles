;;; evil-search-highlight-persist.el --- Persistent highlights after search
;; Version: 20150107.4
;; Package-Version: 20160912.807
;; X-Original-Version: 20140918

;; Author: Juanjo Alvarez <juanjo@juanjoalvarez.net>
;; Created:  September 18, 2014
;; Package-Requires: ((highlight "0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139,
;; USA.

;;; Commentary:
;;
;; This extension will make isearch and evil-ex-search-incremental to
;; highlight the search term (taken as a regexp) in all the buffer and
;; persistently until you make another search or clear the highlights
;; with the evil-search-highlight-persist-remove-all command (default
;; binding to C-x SPC). This is how Vim search works by default when
;; you enable hlsearch.
;;
;; To enable:
;;
;; (require 'evil-search-highlight-persist)
;; (global-evil-search-highlight-persist t)

;; To only display string whose length is greater than or equal to 3
;; (setq evil-search-highlight-string-min-len 3)


;;; Code:

;;; User Customizable Variables:
(require 'advice)
(require 'highlight)
(require 'evil-search)

(defvar evil-search-highlight-regex-flag t)
(defun hlt-+/--highlight-regexp-region (unhighlightp start end regexp face msgp mousep nth &optional buffers)
  "Helper for `hlt-(un)highlight-regexp-region'.
Non-nil UNHIGHLIGHTP means unhighlight.  Otherwise, highlight.
The other arguments are as for `hlt-highlight-regexp-region'.
If UNHIGHLIGHTP:
 Do not advance to the next face, even if `hlt-auto-faces-flag'.
 If FACE is nil then unhighlight all faces."
  (unless regexp (setq regexp  hlt-last-regexp))
  (unless (stringp regexp)              ; Else re-search-forward gets an error
    (error "HLT-%sHIGHLIGHT-REGEXP-REGION: REGEXP arg is not a string: `%S'"
           (if unhighlightp "UN" "")
           regexp))
  (let ((mbufs  buffers))
    (unless buffers (setq buffers  (list (current-buffer))))
    ;; Advance the face if highlighting (but not unhighlighting) with auto faces.
    (when (and hlt-auto-faces-flag  (not unhighlightp)) (hlt-next-face))
    (if face (setq hlt-last-face  face) (unless unhighlightp (setq face  hlt-last-face)))
    (dolist (buf  buffers)
      (with-current-buffer buf
        ;; Use START and END if provided non-interactively, but not otherwise.
        (unless (and start  end  (or (not (cadr buffers))  (not (interactive-p))))
          (let ((start-end  (hlt-region-or-buffer-limits buf)))
            (setq start  (car start-end)
                  end    (cadr start-end))))
        ;; Do nothing if START or END is a marker for a different buffer.
        (when (and (or (not (markerp start))  (eq buf (marker-buffer start)))
                   (or (not (markerp end))    (eq buf (marker-buffer end))))
          (when (and msgp  (not unhighlightp))
            (let ((reg-size  (abs (- end start))))
              (when (and (> reg-size hlt-max-region-no-warning)
                         (not (progn (and (fboundp 'flash-ding) ; In `frame-fns.el'
                                          (flash-ding 'no-terminate-macros (selected-frame)))
                                     (y-or-n-p (substitute-command-keys
                                                (format "Lots of highlighting slows things down.  Do you \
really want to highlight up to %d chars?  "
                                                        reg-size))))))
                (error "OK, highlighting cancelled"))))
          (when (eq t msgp)
            (message "%sighlighting occurrences of `%s'%s..."
                     (if unhighlightp "UNh" "H")
                     regexp
                     (if mbufs (format " in `%s'"  buf) "")))
          (let ((hits-p               nil)
                (hlt-auto-faces-flag  nil)) ; Prevent advancing - we already advanced.
            (save-excursion
              (goto-char start)
              (if evil-search-highlight-regex-flag
                  (while (and (< start end)  (not (eobp))  (re-search-forward regexp end t)  (setq hits-p  t))
                (condition-case nil
                    (progn (forward-char 1) (setq start  (1+ (point))))
                  (end-of-buffer (setq start  end)))
                (funcall (if unhighlightp #'hlt-unhighlight-region #'hlt-highlight-region)
                         (match-beginning (or nth  0))
                         (match-end (or nth  0))
                         face
                         nil
                         mousep))
                (while (and (< start end)  (not (eobp))  (search-forward regexp end t)  (setq hits-p  t))
                (condition-case nil
                    (progn (forward-char 1) (setq start  (1+ (point))))
                  (end-of-buffer (setq start  end)))
                (funcall (if unhighlightp #'hlt-unhighlight-region #'hlt-highlight-region)
                         (match-beginning (or nth  0))
                         (match-end (or nth  0))
                         face
                         nil
                         mousep)))
              )
            (when (eq t msgp)
              (if hits-p
                  (message "%sighlighting occurrences of `%s'%s done  %s"
                           (if unhighlightp "UNh" "H")
                           regexp
                           (if mbufs (format " in `%s'"  buf) "")
                           (if unhighlightp
                               ""
                             (let ((remove-msg  "\\[hlt-unhighlight-regexp-region]' to remove highlighting"))
                               (when mousep (setq remove-msg  (concat "\\[universal-argument] " remove-msg)))
                               (setq remove-msg  (substitute-command-keys (concat "`" remove-msg)))
                               remove-msg)))
                (message "No occurrences of `%s' in `%s'" regexp buf))))
          (setq hlt-last-regexp  regexp))))))


(defgroup evil-search-highlight-persist nil
  "evil-search-highlight-persist -- Search Highlight Remain, Vim's style"
  :tag "SearchHighlightPersist"
  :group 'environment)


(defface evil-search-highlight-persist-highlight-face
  '((((class color))
     (:background "yellow1")))
  "Face for the highlighted text."
  :group 'evil-search-highlight-persist)


(defun evil-search-highlight-persist-remove-all ()
  (interactive)
  (hlt-unhighlight-region-in-buffers (list (current-buffer))))

(defvar evil-search-highlight-string-min-len 1 "min legth")
(defun evil-search-highlight-persist-mark ()
  (let ((hlt-use-overlays-flag t)
        (hlt-last-face 'evil-search-highlight-persist-highlight-face)
        tmp)
    (if isearch-regexp
        (progn
          (setq tmp (car-safe regexp-search-ring))
          (setq evil-search-highlight-regex-flag t))
      (progn
        (setq tmp (car-safe search-ring))
        (setq evil-search-highlight-regex-flag nil)))
    (if (>= (length tmp) evil-search-highlight-string-min-len)
          (hlt-highlight-regexp-region-in-buffers tmp (list (current-buffer)))
      ))
  (setq evil-search-highlight-regex-flag t)
  )

(make-variable-buffer-local 'evil-search-highlight-persist-enabled)

(defadvice isearch-exit (after isearch--highlight-persist)
  (when evil-search-highlight-persist-enabled
    (evil-search-highlight-persist-remove-all)
    (evil-search-highlight-persist-mark)))

(defadvice evil-flash-search-pattern (after evil-flash-search--highlight-persist)
  (when evil-search-highlight-persist-enabled
    (evil-search-highlight-persist-remove-all)
    (evil-search-highlight-persist-mark)))

(ad-activate 'isearch-exit)
(ad-activate 'evil-flash-search-pattern)

;;;###autoload
(define-minor-mode evil-search-highlight-persist
  "Keep the highlights persist after a search"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-x SPC") 'evil-search-highlight-persist-remove-all)
            map)
  (if evil-search-highlight-persist
      (setq evil-search-highlight-persist-enabled t)
    (evil-search-highlight-persist-remove-all)
    (setq evil-search-highlight-persist-enabled nil)))

;;;###autoload
(defun turn-on-search-highlight-persist ()
  "Enable search-highlight-persist in the current buffer."
  (evil-search-highlight-persist
   (if (eq 'fundamental-mode major-mode) -1 1)))

;;;###autoload
(defun turn-off-search-highlight-persist ()
  "Disable evil-search-highlight-persist in the current buffer."
  (evil-search-highlight-persist -1))


;;;###autoload
(define-globalized-minor-mode global-evil-search-highlight-persist
  evil-search-highlight-persist turn-on-search-highlight-persist)

;; * provide
(provide 'evil-search-highlight-persist)
;;; evil-search-highlight-persist.el ends here
