;;; ab-elisp.el --- Ascertain Bounds for Emacs Lisp -*- lexical-binding: t; -*-

;; Author: sogaiu
;; Version: 20200805
;; Package-Requires: ((emacs "26.2"))
;; Keywords: bounds elisp

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
;; (require 'ab-elisp)

;;;; Usage

;; Run one of these commands:

;; `ab-select-elisp-def*': try to select various Emacs Lisp def* expressions
;; `ab-set-frame-height-to-elisp-def*': match frame height to elisp def* expr

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

(defvar ab-elisp-def-forms
  (list 'cl-defgeneric
        'cl-defmethod
        'defun
        'defvar
        'defalias
        'defcustom
        'defconst
        'defsubst
        'defadvice
        'defgroup
        'defmacro
        'defface
        'define-derived-mode
        'define-minor-mode))

;;;;; Commands

;;;###autoload
(defun ab-select-elisp-def* ()
  "Guess the bounds of some elisp def-form around point."
  (interactive)
  (condition-case err
      (cl-destructuring-bind (start end) (ab-sexp-bounds ab-elisp-def-forms)
	(set-mark start)
	(goto-char end)
	(activate-mark))
    (wrong-number-of-arguments
     (message "Failed to find containing defn form."))
    (error
     (message "Error: %s %s" (car err) (cdr err)))))

;;;###autoload
(defun ab-set-frame-height-to-elisp-def* ()
  "Set the frame height to the bounds of some elisp def-form around point."
  (interactive)
  (condition-case err
      (cl-destructuring-bind (start end) (ab-sexp-bounds ab-elisp-def-forms)
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
(defun ab-match-frame-to-elisp-def* ()
  "Match the frame to the bounds of some elisp def-form around point."
  (interactive)
  (condition-case err
      (cl-destructuring-bind (start end) (ab-sexp-bounds ab-elisp-def-forms)
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

(provide 'ab-elisp)

;;; ab.el ends here
