;;; helm-pydoc.el --- pydoc with helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-pydoc
;; Version: 0.07
;; Package-Requires: ((helm-core "2.0") (emacs "24.4"))

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

(require 'cl-lib)
(require 'helm)
(require 'python)

(defgroup helm-pydoc nil
  "Pydoc with helm interface"
  :group 'helm)

(defcustom helm-pydoc-virtualenv "venv"
  "Directory name containing virtualenv."
  :type 'string)

(defvar helm-pydoc--collect-command
  (if load-file-name
      (concat (file-name-directory load-file-name) "helm-pydoc.py")
    "helm-pydoc.py"))

(defun helm-pydoc--python ()
  (with-helm-current-buffer
    (let* ((file (or (buffer-file-name) default-directory))
           (venv (locate-dominating-file file helm-pydoc-virtualenv)))
      (if venv
          (concat (expand-file-name (file-name-as-directory venv))
                  helm-pydoc-virtualenv (if (eq system-type 'windows-nt)
                                            "/Scripts/python"
                                          "/bin/python"))
        python-shell-interpreter))))

(defun helm-pydoc--collect-imported-modules ()
  (with-helm-current-buffer
    (save-excursion
      (goto-char (point-min))
      (let ((modules nil))
        (while (re-search-forward "^\\s-*\\(?:import\\|from\\)\\s-+\\([^ \t\r\n]+\\)" nil t)
          (push (match-string-no-properties 1) modules))
        (reverse modules)))))

(defun helm-pydoc--init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (unless (zerop (call-process (helm-pydoc--python) nil t nil
                                 helm-pydoc--collect-command))
      (error "Failed helm-pydoc--init"))))

(defsubst helm-pydoc--pydoc-buffer (module)
  (get-buffer-create (format "*Pydoc %s*" module)))

(defun helm-pydoc--do-pydoc (module)
  (with-current-buffer (helm-pydoc--pydoc-buffer module)
    (view-mode -1)
    (erase-buffer)
    (unless (zerop (call-process (helm-pydoc--python) nil t nil "-m" "pydoc" module))
      (error "Failed: 'pydoc'"))
    (goto-char (point-min))
    (view-mode +1)
    (pop-to-buffer (current-buffer))))

(defun helm-pydoc--module-file (module)
  (with-temp-buffer
    (let ((import-expression (format "import %s;print(%s.__file__)" module module)))
      (unless (zerop (call-process (helm-pydoc--python) nil t nil
                                   "-c" import-expression))
        (error "Not found module '%s' source code" module))
      (goto-char (point-min))
      (let ((modname (buffer-substring (point) (line-end-position))))
        (if (string-match "\\`\\(\.+\\.py\\)c\\'" modname)
            (match-string-no-properties 1 modname)
          modname)))))

(defun helm-pydoc--view-source (candidate)
  (let ((modfile (helm-pydoc--module-file candidate)))
    (find-file-read-only-other-window modfile)))

(defun helm-pydoc--check-imported (module)
  (save-excursion
    (let ((regexp (format "^\\s-*\\(?:from\\|import\\)\\s-+%s\\>" module)))
      (re-search-backward regexp nil t))))

(defun helm-pydoc--collect-import-modules ()
  (cl-loop for module in (helm-marked-candidates)
           when (not (helm-pydoc--check-imported module))
           collect module into modules
           finally return (sort modules 'string<)))

(defun helm-pydoc--construct-import-statement (modules)
  (cond ((null (cdr modules))
         (format "import %s\n" (car modules)))
        (t
         (mapconcat (lambda (m) (concat "import " m)) modules "\n"))))

(defun helm-pydoc--insert-import-statement (inserted)
  (save-excursion
    (goto-char (line-end-position))
    (if (re-search-backward "^\\s-*\\(?:from\\|import\\)\\s-+" nil t)
        (forward-line 1)
      (helm-pydoc--skip-comments))
    (insert inserted)))

(defun helm-pydoc--skip-comments ()
  (goto-char (point-min))
  (cl-loop while (string-match-p "\\`#" (buffer-substring-no-properties
                                         (line-beginning-position)
                                         (line-end-position)))
           do
           (forward-line 1)))

(defun helm-pydoc--import-module (_candidate)
  (let* ((modules (helm-pydoc--collect-import-modules))
         (statements (helm-pydoc--construct-import-statement modules)))
    (helm-pydoc--insert-import-statement statements)))

(defun helm-pydoc--construct-from-import (module imports &optional name)
  (format "from %s import %s%s\n"
          module imports
          (if name
              (format " as name")
            "")))

(defun helm-pydoc--from-import-module (candidate)
  (let* ((imports (read-string (format "Identifiers in %s: " candidate)))
         (statement (helm-pydoc--construct-from-import candidate imports)))
    (helm-pydoc--insert-import-statement statement)))

(defun helm-pydoc--from-import-as-module (candidate)
  (let* ((imports (read-string (format "Identifiers in %s: " candidate)))
         (name (read-string (format "As name [%s]: " candidate)))
         (statement (helm-pydoc--construct-from-import
                     candidate imports name)))
    (helm-pydoc--insert-import-statement statement)))

(defvar helm-pydoc--actions
  '(("Pydoc Module" . helm-pydoc--do-pydoc)
    ("View Source Code" . helm-pydoc--view-source)
    ("Import Module(import module)" . helm-pydoc--import-module)
    ("Import Module(from module import identifiers)"
     . helm-pydoc--from-import-module)
    ("Import Module(from module import identifiers as name)"
     . helm-pydoc--from-import-as-module)))

(defvar helm-pydoc--imported-source
  (helm-build-sync-source "Imported Modules"
    :candidates 'helm-pydoc--collect-imported-modules
    :action helm-pydoc--actions
    :candidate-number-limit 9999))

(defvar helm-pydoc--installed-source
  (helm-build-in-buffer-source "Installed Modules"
    :init 'helm-pydoc--init
    :action helm-pydoc--actions
    :candidate-number-limit 9999))

(defvar helm-pydoc--history nil)

;;;###autoload
(defun helm-pydoc ()
  (interactive)
  (helm :sources '(helm-pydoc--imported-source helm-pydoc--installed-source)
        :buffer "*helm pydoc*" :history 'helm-pydoc--history))

(provide 'helm-pydoc)

;;; helm-pydoc.el ends here
