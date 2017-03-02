;;; helm-mode-manager.el --- Select and toggle major and minor modes with helm

;; Copyright (C) 2014 istib

;; Author: istib
;; URL: https://github.com/istib/helm-mode-manager
;; Package-Version: 20151124.138
;; Version: 0.1
;; Package-Requires: ((helm "1.5.3"))
;; Keywords:

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A call to `helm-switch-major-mode' will give you a `helm' selection
;; of all available major modes.  Selecting a target will activate the
;; major mode.  The persistent action is to show help about the
;; selected major mode.
;;
;; A call to `helm-enable-minor-mode' will give you a `helm' selection
;; of all available minor modes.  Selecting a target will activate the
;; minor mode.  The persistent action is to show help about the
;; selected minor mode.

;; A call to `helm-disable-minor-mode' will give you a `helm'
;; selection of active minor modes.  Selecting a target will
;; disactivate the minor mode.  The persistent action is to show help
;; about the selected minor mode.

;;; Code:

(require 'helm)

(defun helm-mode-manager-describe-mode (mode)
  (describe-function (intern mode)))

;;;###autoload
(defun helm-enable-minor-mode ()
  (interactive)
  (helm
   :sources '((name . "Minor modes")
              (candidates . minor-mode-list)
              (action . (lambda (_candidate)
                          (dolist (mode (helm-marked-candidates))
                            (funcall (intern mode)))))
              (persistent-action . helm-mode-manager-describe-mode))))

;;;###autoload
(defun helm-disable-minor-mode ()
  (interactive)
  (let (active-minor-modes)
    (mapc (lambda (mode) (condition-case nil
                        (if (and (symbolp mode) (symbol-value mode))
                            (add-to-list 'active-minor-modes mode))
                      (error nil)))
          minor-mode-list)
    (helm
     :sources '((name . "Active minor modes")
                (candidates . active-minor-modes)
                (action . (lambda (_candidate)
                            (dolist (mode (helm-marked-candidates))
                              (funcall (intern mode) -1))))
                (persistent-action . helm-mode-manager-describe-mode)))))

(defun helm-mode-manager-list-major-modes ()
  "Returns list of potential major mode names.
From Tobias Zawada (http://stackoverflow.com/questions/5536304/emacs-stock-major-modes-list)"
  (interactive)
  (let (l)
    (mapatoms #'(lambda (f) (and
                        (commandp f)
                        (string-match "-mode$" (symbol-name f))
                        ;; auto-loaded
                        (or (and (autoloadp (symbol-function f))
                              (let ((doc (documentation f)))
                                (when doc
                                  (and
                                   (let ((docSplit (help-split-fundoc doc f)))
                                     (and docSplit ;; car is argument list
                                        (null (cdr (read (car docSplit)))))) ;; major mode starters have no arguments
                                   (if (string-match "[mM]inor" doc) ;; If the doc contains "minor"...
                                       (string-match "[mM]ajor" doc) ;; it should also contain "major".
                                     t) ;; else we cannot decide therefrom
                                   ))))
                           (null (help-function-arglist f)))
                        (setq l (cons (symbol-name f) l)))))
    l))

;;;###autoload
(defun helm-switch-major-mode ()
  (interactive)
  (let ((major-modes (helm-mode-manager-list-major-modes)))
    (helm
     :sources '((name . "Major modes")
                (candidates . major-modes)
                (action . (lambda (mode) (funcall (intern mode))))
                (persistent-action . (lambda (mode) (describe-function (intern mode))))))))

(provide 'helm-mode-manager)

;;; helm-mode-manager.el ends here
