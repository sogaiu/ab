;;; ab-janet.el --- Ascertain Bounds for Janet -*- lexical-binding: t; -*-

;; Author: sogaiu
;; Version: 20200805
;; Package-Requires: ((emacs "26.2"))
;; Keywords: bounds janet

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Code related to ascertaining the bounds of expressions.

;;;; Installation

;;;;; Manual

;; Ensure this file and the following dependencies are in your
;; load path:
;;
;;   ab
;;
;; and put this in your relevant init file:
;;
;; (require 'ab-janet)

;;;; Usage

;; Run one of these commands:

;; `ab-select-janet-def*': try to select various Janet def* expressions
;; `ab-set-frame-height-to-janet-def*': match frame height to clj def* expr

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

(require 'ab)

;;;; Variables

;; XXX: var, var-, varglobal, default, dyn?
(defvar ab-janet-def-forms
  (list 'def
        'def-
        'defn
        'defn-
        'defglobal
        'defmacro
        'defmacro-))

;;;;; Commands

;;;###autoload
(defun ab-select-janet-def* ()
  "Guess the bounds of some Janet def-form around point."
  (interactive)
  (condition-case err
      (cl-destructuring-bind (start end) (ab-sexp-bounds ab-janet-def-forms)
	(set-mark start)
	(goto-char end)
	(activate-mark))
    (wrong-number-of-arguments
     (message "Failed to find containing def* form."))
    (error
     (message "Error: %s %s" (car err) (cdr err)))))

;;;###autoload
(defun ab-set-frame-height-to-janet-def* ()
  "Set the frame height to the bounds of some janet def-form around point."
  (interactive)
  (condition-case err
      (cl-destructuring-bind (start end) (ab-sexp-bounds ab-janet-def-forms)
	(set-frame-height (selected-frame)
                          (+ (- (line-number-at-pos end)
                                (line-number-at-pos start))
                             3))
        (let ((here (point)))
          (delete-other-windows)
          (goto-char start)
          (recenter-top-bottom 0)
          (goto-char here)))
    (wrong-number-of-arguments
     (message "Failed to find containing defn form."))
    (error
     (message "Error: %s %s" (car err) (cdr err)))))

;;;###autoload
(defun ab-match-frame-to-janet-def* ()
  "Match the frame to the bounds of some janet def-form around point."
  (interactive)
  (condition-case err
      (cl-destructuring-bind (start end) (ab-sexp-bounds ab-janet-def-forms)
        (set-frame-parameter (selected-frame) 'menu-bar-lines 0)
	(set-frame-height (selected-frame)
                          (+ (- (line-number-at-pos end)
                                (line-number-at-pos start))
                             3))
        (let ((here (point)))
          (delete-other-windows)
          (goto-char start)
          (recenter-top-bottom 0)
          (goto-char here)))
    (wrong-number-of-arguments
     (message "Failed to find containing defn form."))
    (error
     (message "Error: %s %s" (car err) (cdr err)))))

;;;; Footer

(provide 'ab-janet)

;;; ab-janet.el ends here
