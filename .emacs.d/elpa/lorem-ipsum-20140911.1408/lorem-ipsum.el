;;; lorem-ipsum.el --- Insert dummy pseudo Latin text.

;; Copyright (c) 2003 Jean-Philippe Theberge

;; Author: Jean-Philippe Theberge (jphil21@sourceforge.net)
;; Maintainer: Joe Schafer (joe@jschaf.com)
;; Package-Version: 20140911.1408
;; Package-X-Original-Version: 0.2
;; Keywords: tools, language, convenience

;; Special Thanks: The emacswiki users, the #emacs@freenode.net citizens
;;                 and Marcus Tullius Cicero

;; This file is not part of GNU Emacs.

;; Contains code from GNU Emacs <https://www.gnu.org/software/emacs/>,
;; released under the GNU General Public License version 3 or later.

;; lorem-ipsum.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; lorem-ipsum.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with lorem-ipsum.el.  If not, see <http://www.gnu.org/licenses/>.

;;; History:

;; Version 0.1 released by Jean-Philippe Theberge in 2003.  After
;; attempting to contact Jean-Philippe, Joe Schafer took over as
;; maintainer and published to Github.

;;; Commentary:

;; This package provides convenience functions to insert dummy Latin
;; text into a buffer.
;;
;; To install manually, add this file to your `load-path'.  Use the
;; default keybindings by adding the following to your .emacs file:
;;
;; (lorem-ipsum-use-default-bindings)
;;
;; This will setup the folling keybindings:
;;
;; C-c l p: lorem-ipsum-insert-paragraphs
;; C-c l s: lorem-ipsum-insert-sentences
;; C-c l l: lorem-ipsum-insert-list
;;
;; If you want a different keybinding, say you want the prefix C-c C-l, use a variation of the
;; following:
;;
;; (global-set-key (kbd "C-c C-l s") 'lorem-ipsum-insert-sentences)
;; (global-set-key (kbd "C-c C-l p") 'lorem-ipsum-insert-paragraphs)
;; (global-set-key (kbd "C-c C-l l") 'lorem-ipsum-insert-list)


;;; Code:

(defconst lorem-ipsum-version "0.2")

(defgroup lorem-ipsum nil
  "Insert filler text."
  :group 'tools
  :group 'convenience)

;;;###autoload
(defun lorem-ipsum-use-default-bindings ()
  "Use the default keybindings of C-c l [spl]."
  (interactive)
  (global-set-key (kbd "C-c l s") 'lorem-ipsum-insert-sentences)
  (global-set-key (kbd "C-c l p") 'lorem-ipsum-insert-paragraphs)
  (global-set-key (kbd "C-c l l") 'lorem-ipsum-insert-list))

(defconst lorem-ipsum-text
  '(("Lorem ipsum dolor sit amet, consectetuer adipiscing elit."
     "Donec hendrerit tempor tellus."
     "Donec pretium posuere tellus."
     "Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus."
     "Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus."
     "Nulla posuere."
     "Donec vitae dolor."
     "Nullam tristique diam non turpis."
     "Cras placerat accumsan nulla."
     "Nullam rutrum."
     "Nam vestibulum accumsan nisl.")

    ("Pellentesque dapibus suscipit ligula."
     "Donec posuere augue in quam."
     "Etiam vel tortor sodales tellus ultricies commodo."
     "Suspendisse potenti."
     "Aenean in sem ac leo mollis blandit."
     "Donec neque quam, dignissim in, mollis nec, sagittis eu, wisi."
     "Phasellus lacus."
     "Etiam laoreet quam sed arcu."
     "Phasellus at dui in ligula mollis ultricies."
     "Integer placerat tristique nisl."
     "Praesent augue."
     "Fusce commodo."
     "Vestibulum convallis, lorem a tempus semper, dui dui euismod elit, vitae placerat urna tortor vitae lacus."
     "Nullam libero mauris, consequat quis, varius et, dictum id, arcu."
     "Mauris mollis tincidunt felis."
     "Aliquam feugiat tellus ut neque."
     "Nulla facilisis, risus a rhoncus fermentum, tellus tellus lacinia purus, et dictum nunc justo sit amet elit.")

    ("Aliquam erat volutpat."
     "Nunc eleifend leo vitae magna."
     "In id erat non orci commodo lobortis."
     "Proin neque massa, cursus ut, gravida ut, lobortis eget, lacus."
     "Sed diam."
     "Praesent fermentum tempor tellus."
     "Nullam tempus."
     "Mauris ac felis vel velit tristique imperdiet."
     "Donec at pede."
     "Etiam vel neque nec dui dignissim bibendum."
     "Vivamus id enim."
     "Phasellus neque orci, porta a, aliquet quis, semper a, massa."
     "Phasellus purus."
     "Pellentesque tristique imperdiet tortor."
     "Nam euismod tellus id erat.")

    ("Nullam eu ante vel est convallis dignissim."
     "Fusce suscipit, wisi nec facilisis facilisis, est dui fermentum leo, quis tempor ligula erat quis odio."
     "Nunc porta vulputate tellus."
     "Nunc rutrum turpis sed pede."
     "Sed bibendum."
     "Aliquam posuere."
     "Nunc aliquet, augue nec adipiscing interdum, lacus tellus malesuada massa, quis varius mi purus non odio."
     "Pellentesque condimentum, magna ut suscipit hendrerit, ipsum augue ornare nulla, non luctus diam neque sit amet urna."
     "Curabitur vulputate vestibulum lorem."
     "Fusce sagittis, libero non molestie mollis, magna orci ultrices dolor, at vulputate neque nulla lacinia eros."
     "Sed id ligula quis est convallis tempor."
     "Curabitur lacinia pulvinar nibh."
     "Nam a sapien.")))

(defvar lorem-ipsum-paragraph-separator "\n\n")
(defvar lorem-ipsum-sentence-separator "  ")
(defvar lorem-ipsum-list-beginning "")
(defvar lorem-ipsum-list-bullet "* ")
(defvar lorem-ipsum-list-item-end "\n")
(defvar lorem-ipsum-list-end "")

(make-variable-buffer-local 'lorem-ipsum-paragraph-separator)
(make-variable-buffer-local 'lorem-ipsum-sentence-separator)
(make-variable-buffer-local 'lorem-ipsum-list-beginning)
(make-variable-buffer-local 'lorem-ipsum-list-bullet)
(make-variable-buffer-local 'lorem-ipsum-list-item-end)
(make-variable-buffer-local 'lorem-ipsum-list-end)

(add-hook 'sgml-mode-hook (lambda ()
			    (setq lorem-ipsum-paragraph-separator "<br><br>\n"
				  lorem-ipsum-sentence-separator "&nbsp;&nbsp;"
				  lorem-ipsum-list-beginning "<ul>\n"
				  lorem-ipsum-list-bullet "<li>"
				  lorem-ipsum-list-item-end "</li>\n"
				  lorem-ipsum-list-end "</ul>\n")))

;;;###autoload
(defun lorem-ipsum-insert-paragraphs (&optional num)
  "Insert lorem ipsum paragraphs into buffer.
If NUM is non-nil, insert NUM paragraphs."
  (interactive "p")
  (if (not num)(setq num 1))
  (if (> num 0)
      (progn
	(insert (concat
		 (mapconcat 'identity
			    (nth (random (length lorem-ipsum-text))
				 lorem-ipsum-text) lorem-ipsum-sentence-separator) lorem-ipsum-paragraph-separator))
	(lorem-ipsum-insert-paragraphs (- num 1)))))

;;;###autoload
(defalias 'Lorem-ipsum-insert-paragraphs 'lorem-ipsum-insert-paragraphs)

;;;###autoload
(defun lorem-ipsum-insert-sentences (&optional num)
  "Insert lorem ipsum sentences into buffer.
If NUM is non-nil, insert NUM sentences."
  (interactive "p")
  (if (not num)(setq num 1))
  (if (> num 0)
      (progn
	(let ((para
	       (nth (random (length lorem-ipsum-text)) lorem-ipsum-text)))
	  (insert (concat (nth (random (length para)) para) lorem-ipsum-sentence-separator)))
	(lorem-ipsum-insert-sentences (- num 1)))))

;;;###autoload
(defalias 'Lorem-ipsum-insert-sentences 'lorem-ipsum-insert-sentences)

;;;###autoload
(defun lorem-ipsum-insert-list (&optional num)
  "Insert lorem ipsum list items into buffer.
If NUM is non-nil, insert NUM list items."
  (interactive "p")
  (if (not num)(setq num 1))
  (if (> num 0)
      (progn
	(let ((para (nth (random (length lorem-ipsum-text)) lorem-ipsum-text)))
	  (insert (concat lorem-ipsum-list-bullet
			  (nth (random (length para)) para)
			  lorem-ipsum-list-item-end)))
	(lorem-ipsum-insert-list (- num 1)))
    (insert lorem-ipsum-list-end)))

;;;###autoload
(defalias 'Lorem-ipsum-insert-list 'lorem-ipsum-insert-list)

(provide 'lorem-ipsum)

;;; lorem-ipsum.el ends here
