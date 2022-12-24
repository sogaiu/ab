;;; ab.el --- Ascertain Bounds -*- lexical-binding: t; -*-

;; Author: sogaiu
;; Version: 20190907
;; Package-Requires: ((emacs "26.2"))
;; Keywords: bounds

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Code related to ascertaining the bounds of expressions.

;;;; Installation

;;;;; Manual

;; Put this file in your load-path, and put this in your init
;; file:

;; (require 'ab)

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'seq)
(require 'thingatpt)

;;;; Functions

;; XXX: consider regular expression matching...
;; XXX: factoring bits lead to problems, hence this is verbose atm
(defun ab-sexp-bounds (targets)
  "Find bounds of closest ancestor sexp having initial symbol in TARGETS.

After placing point within a def in Clojure, try:

  (ab-sexp-bounds (list \\='def))

or within a defn or defn-, try:

  (ab-sexp-bounds (list \\='defn \\='defn-))

or within a defun in Emacs Lisp, try:

  (ab-sexp-bounds (list \\='defun))

Successful detection of the bounds should yield a list with 2 elements
representing the start and end."
  (letrec ((here (point))
           ;; have a monkey climb up the tree...
           (monkey (lambda ()
                     (backward-up-list 1 (point) (point))
                     (if (/= 40 (char-after)) ; 40 is open paren
                         (funcall monkey)
                       (forward-char)
                       (let ((sym-here (symbol-at-point)))
                         (if (seq-find (lambda (a-target)
                                         (eq a-target sym-here))
                                       targets)
                           (progn
                             (backward-char)
                             t)
                           (backward-char)
                           (funcall monkey)))))))
    (condition-case nil
	(if (and (= 40 (char-after)) ; point is at open paren
		 (save-excursion
		     (forward-char)
		     (let ((sym-here (symbol-at-point)))
		       (if (seq-find (lambda (a-target)
				       (eq a-target sym-here))
				     targets)
			   t
			 nil))))
	    (let ((start (point))
		  (end (save-excursion
			 (forward-sexp)
			 (point))))
	      (goto-char here) ; not actually necessary
	      (list start end))
          (when (funcall monkey) ; point is not at open paren
            (let ((start (point))
                  (end (save-excursion
			 (forward-sexp)
			 (point))))
              (goto-char here)
              (list start end))))
      (error (goto-char here)
             nil))))

;;;; Footer

(provide 'ab)

;;; ab.el ends here
