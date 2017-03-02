;;; pytest.el --- Easy Python test running in Emacs

;; Copyright (C) 2009 Eric Larson

;; Licensed under the same terms as Emacs.

;; Version: 0.2.1
;; Package-Version: 20161014.815
;; Keywords: pytest python testing
;; URL: https://github.com/ionrock/pytest-el
;; Package-Requires: ((s "1.9.0"))
;; Created: 07 Oct 2011

;; This file is NOT part of GNU Emacs.

;; Licensed under the same terms as Emacs.

;;; Commentary:
;; This gives a bunch of functions that handle running pytest on a
;; particular buffer or part of a buffer.  This started as a direct
;; port of nosemacs (https://bitbucket.org/durin42/nosemacs).  A
;; special thanks to Jason Pellerin and Augie Fackler for writing
;; nose.el.

;;; Installation

;; In your Emacs config:
;;
;;   (require 'pytest)
;;
;; If you don't use a global installation of py.test (ie in
;; virtualenv) then add something like the following that points to
;; either the non-global version or a test runner script.:
;;
;;   (add-to-list 'pytest-project-names "my/crazy/runner")
;;
;; You can generate a script with py.test:
;;
;;   py.test --genscript=run-tests.py

;; Another option is if your global pytest isn't called "pytest" is to
;; redefine pytest-global-name to be the command that should be used.

;; By default, the root of a project is found by looking for any of the files
;; 'setup.py', '.hg' and '.git'.  You can add files to check for to the file
;; list:
;;
;; ; (add-to-list 'pytest-project-root-files "something")

;; or you can change the project root test to detect in some other way
;; whether a directory is the project root:
;;
;; ; (setq pytest-project-root-test (lambda (dirname) (equal dirname "foo")))

;; Probably also want some keybindings:
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (local-set-key "\C-ca" 'pytest-all)
;;             (local-set-key "\C-cm" 'pytest-module)
;;             (local-set-key "\C-c." 'pytest-one)
;;             (local-set-key "\C-cd" 'pytest-directory)
;;             (local-set-key "\C-cpa" 'pytest-pdb-all)
;;             (local-set-key "\C-cpm" 'pytest-pdb-module)
;;             (local-set-key "\C-cp." 'pytest-pdb-one)))

;;; Code:
(require 's)
(require 'cl)
(require 'python)

(defgroup pytest nil
  "Easy Python test running in Emacs"
  :group 'python)

(defcustom pytest-project-names '("runtests")
  "The name of the script that starts the tests.")

(defcustom pytest-project-root-files '("setup.py" ".hg" ".git")
  "Names of files or directories that signify the root of a project.")

(defcustom pytest-project-root-test 'pytest-project-root
  "A function used to determine the directory the tests will be run from.")

(defcustom pytest-global-name "py.test"
  "The name of the py.test executable.")
(put 'pytest-global-name 'safe-local-variable 'stringp)

(defcustom pytest-cmd-flags "-x -s"
  "These are the flags passed to the pytest runner.")

(defcustom pytest-cmd-format-string "cd '%s' && %s %s '%s'"
  "Format string used to run the py.test command.")

(defun pytest-cmd-format (format-string working-directory test-runner command-flags test-names)
  "Create the string used for running the py.test command.
FORMAT-STRING is a template string used by (format) to compose
the py.test command invocation.  The string should contain enough
'%s' placeholders to satisfy the remaining arguments to this
function.
WORKING-DIRECTORY is the directory to run py.test in.
TEST-RUNNER is the name of the command to run.
COMMAND-FLAGS are the flags to pass into py.test.
TEST-NAMES are the names of the tests to run.

The function returns a string used to run the py.test command.  Here's an example:
'cd WORKING-DIRECTORY && TEST-RUNNER COMMAND-FLAGS TEST-NAMES'"
  (format format-string working-directory test-runner command-flags test-names))

(defun pytest-check-test-file (path)
  (if (not (file-exists-p path))
      (error (format "'%s' is not an extant file." path))))

(defun pytest-run (&optional tests flags)
  "Run pytest.
Optional argument TESTS Tests to run.
Optional argument FLAGS py.test command line flags."
  (interactive "fTest directory or file: \nspy.test flags: ")
  (let* ((pytest (pytest-find-test-runner))
         (where (if tests
                    (let ((testpath (if (listp tests) (car tests) tests)))
                      (pytest-find-project-root (file-name-directory testpath)))
                  (pytest-find-project-root)))
         (tests (cond ((not tests) (list "."))
                      ((listp tests) tests)
                      ((stringp tests) (split-string tests))))
         (tnames (mapconcat (apply-partially 'format "'%s'") tests " "))
         (cmd-flags (if flags flags pytest-cmd-flags))
         (use-comint (s-contains? "pdb" cmd-flags)))
    (funcall #'(lambda (command)
                 (compilation-start command use-comint
                                    (lambda (mode) (concat "*pytest*"))))
             (pytest-cmd-format pytest-cmd-format-string where pytest cmd-flags tnames))
    (if use-comint
	(with-current-buffer (get-buffer "*pytest*")
	  (inferior-python-mode)))))

;;; Run entire test suite
;;;###autoload
(defun pytest-all (&optional flags)
  "Run all tests.
Optional argument FLAGS py.test command line flags."
  (interactive)
  (pytest-run nil flags))

;;;###autoload
(defun pytest-failed ()
  "Quit test suite on first failed test."
  (interactive)
  (pytest-all "-x "))

;;;###autoload
(defun pytest-pdb-all ()
  "Start pdb on error."
  (interactive)
  (pytest-all (concat "--pdb " pytest-cmd-flags)))

;;; Run all the tests in a directory (and its child directories)
;;;###autoload
(defun pytest-directory (&optional flags)
  "Run pytest on all the files in the current buffer.
Optional argument FLAGS py.test command line flags."
  (interactive)
  (pytest-run (file-name-directory buffer-file-name) flags))

;;;###autoload
(defun pytest-pdb-directory (&optional flags)
  "Run pytest on all the files in the current buffer.
Optional argument FLAGS py.test command line flags."
  (interactive)
  (pytest-directory (concat "--pdb " pytest-cmd-flags)))

;;; Run all the tests in a file
;;;###autoload
(defun pytest-module (&optional flags)
  "Run pytest (via eggs/bin/test) on current buffer.
Optional argument FLAGS py.test command line flags."
  (interactive)
  (pytest-run buffer-file-name flags))

;;;###autoload
(defun pytest-pdb-module ()
  "Run pytest on a module, enter debugger on error."
  (interactive)
  (pytest-module (concat "--pdb " pytest-cmd-flags)))

;;; Run the test surrounding the current point
;;;###autoload
(defun pytest-one (&optional flags)
  "Run pytest (via eggs/bin/test) on testable thing at point in current buffer.
Optional argument FLAGS py.test command line flags."
  (interactive)
  (pytest-run (format "%s" (pytest-py-testable)) flags))

;;;###autoload
(defun pytest-pdb-one ()
  "Run pytest on testable thing at point, enter debugger on error."
  (interactive)
  (pytest-one (concat "--pdb " pytest-cmd-flags)))


;;; Utility functions
(defun pytest-find-test-runner ()
  (let ((result
     (reduce '(lambda (x y) (or x y))
         (mapcar 'pytest-find-test-runner-names pytest-project-names))))
    (if result
    result
      pytest-global-name)))

(defun pytest-find-test-runner-names (runner)
  "Find eggs/bin/test in a parent dir of current buffer's file."
  (pytest-find-test-runner-in-dir-named
   (file-name-directory buffer-file-name) runner))

(defun pytest-find-test-runner-in-dir-named (dn runner)
  (let ((fn (expand-file-name runner dn)))
    (cond ((file-regular-p fn) fn)
      ((equal dn "/") nil)
      (t (pytest-find-test-runner-in-dir-named
          (file-name-directory (directory-file-name dn))
          runner)))))

(defun pytest-py-testable ()
  "Create a path to a test.
This uses the `::` delimiter between the
filename, class and method in order to find the specific test
case.  This requires pytest >= 1.2."
  (let* ((inner-obj (pytest-inner-testable))
         (outer (pytest-outer-testable))
         ;; elisp can't return multiple values
         (outer-def (car outer))
         (outer-obj (cdr outer)))
    (concat
     (buffer-file-name)
     (cond ((equal outer-def "def") (format "::%s" outer-obj))
       ((equal inner-obj outer-obj) (format "::%s" outer-obj))
       (t (format "::%s::%s" outer-obj inner-obj))))))

(defun pytest-inner-testable ()
  "Find the function name for `pytest-one'."
  (save-excursion
    (re-search-backward
     "^[ \t]\\{0,4\\}\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (buffer-substring-no-properties (match-beginning 2) (match-end 2))))

(defun pytest-outer-testable ()
  "Find the class for the `pytest-one'."
  (save-excursion
    (re-search-backward
     "^\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (let ((result
            (buffer-substring-no-properties (match-beginning 2) (match-end 2))))
      (cons
       (buffer-substring-no-properties (match-beginning 1) (match-end 1))
       result))))

(defun pytest-find-project-root (&optional dirname)
  (let ((dn
         (if dirname
             dirname
           (file-name-directory buffer-file-name))))
    (cond ((funcall pytest-project-root-test dn) (expand-file-name dn))
          ((equal (expand-file-name dn) "/") nil)
        (t (pytest-find-project-root
             (file-name-directory (directory-file-name dn)))))))

(defun pytest-project-root (dirname)
  (reduce '(lambda (x y) (or x y))
          (mapcar (lambda (d) (member d (directory-files dirname)))
                  pytest-project-root-files)))

(defun pytest-current-root ()
  (if (not (buffer-file-name))
      (expand-file-name default-directory)
    (file-name-directory (expand-file-name (buffer-file-name)))))

(provide 'pytest)

;;; pytest.el ends here
