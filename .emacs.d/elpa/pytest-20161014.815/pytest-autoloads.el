;;; pytest-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "pytest" "pytest.el" (22688 13738 543974 539000))
;;; Generated autoloads from pytest.el

(autoload 'pytest-all "pytest" "\
Run all tests.
Optional argument FLAGS py.test command line flags.

\(fn &optional FLAGS)" t nil)

(autoload 'pytest-failed "pytest" "\
Quit test suite on first failed test.

\(fn)" t nil)

(autoload 'pytest-pdb-all "pytest" "\
Start pdb on error.

\(fn)" t nil)

(autoload 'pytest-directory "pytest" "\
Run pytest on all the files in the current buffer.
Optional argument FLAGS py.test command line flags.

\(fn &optional FLAGS)" t nil)

(autoload 'pytest-pdb-directory "pytest" "\
Run pytest on all the files in the current buffer.
Optional argument FLAGS py.test command line flags.

\(fn &optional FLAGS)" t nil)

(autoload 'pytest-module "pytest" "\
Run pytest (via eggs/bin/test) on current buffer.
Optional argument FLAGS py.test command line flags.

\(fn &optional FLAGS)" t nil)

(autoload 'pytest-pdb-module "pytest" "\
Run pytest on a module, enter debugger on error.

\(fn)" t nil)

(autoload 'pytest-one "pytest" "\
Run pytest (via eggs/bin/test) on testable thing at point in current buffer.
Optional argument FLAGS py.test command line flags.

\(fn &optional FLAGS)" t nil)

(autoload 'pytest-pdb-one "pytest" "\
Run pytest on testable thing at point, enter debugger on error.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; pytest-autoloads.el ends here
