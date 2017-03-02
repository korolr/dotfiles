;;; yapfify.el --- (automatically) format python buffers using YAPF.

;; Copyright (C) 2016 Joris Engbers

;; Author: Joris Engbers <info@jorisengbers.nl>
;; Homepage: https://github.com/JorisE/yapfify
;; Version: 0.0.4
;; Package-Version: 20161202.1055
;; Package-Requires: ()

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Yapfify uses yapf to format a Python buffer. It can be called explicitly on a
;; certain buffer, but more conveniently, a minor-mode 'yapf-mode' is provided
;; that turns on automatically running YAPF on a buffer before saving.
;;
;; Installation:
;;
;; Add yapfify.el to your load-path.
;;
;; To automatically format all Python buffers before saving, add the function
;; yapf-mode to python-mode-hook:
;;
;; (add-hook 'python-mode-hook 'yapf-mode)
;;
;;; Code:


(defun yapfify-call-bin (input-buffer output-buffer)
  "Call process yapf on INPUT-BUFFER saving the output to OUTPUT-BUFFER.
Return the exit code."
  (with-current-buffer input-buffer
    (call-process-region (point-min) (point-max) "yapf" nil output-buffer)))

;;;###autoload
(defun yapfify-buffer ()
  "Try to yapfify the current buffer.
If yapf exits with an error, the output will be shown in a help-window."
  (interactive)
  (let* ((original-buffer (current-buffer))
         (original-point (point))  ; Because we are replacing text, save-excursion does not always work.
         (tmpbuf (generate-new-buffer "*yapfify*"))
         (exit-code (yapfify-call-bin original-buffer tmpbuf)))

    ;; There are three exit-codes defined for YAPF:
    ;; 0: Exit with success (change or no change on yapf >=0.11)
    ;; 1: Exit with error
    ;; 2: Exit with success and change (Backward compatibility)
    ;; anything else would be very unexpected.
    (cond ((or (eq exit-code 0) (eq exit-code 2))
           (with-current-buffer tmpbuf
             (copy-to-buffer original-buffer (point-min) (point-max)))
           (goto-char original-point))

          ((eq exit-code 1)
           (error "Yapf failed, see *yapfify* buffer for details")))

    ;; Clean up tmpbuf
    (kill-buffer tmpbuf)))

;;;###autoload
(define-minor-mode yapf-mode
  "Automatically run YAPF before saving."
  :lighter " YAPF"
  (if yapf-mode
      (add-hook 'before-save-hook 'yapfify-buffer nil t)
    (remove-hook 'before-save-hook 'yapfify-buffer t)))

(provide 'yapfify)

;;; yapfify.el ends here
