;;; ab-clojure.el --- Ascertain Bounds for Clojure -*- lexical-binding: t; -*-

;; Author: sogaiu
;; Version: 20200805
;; Package-Requires: ((emacs "26.2"))
;; Keywords: bounds clojure

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
;; (require 'ab-clojure)

;;;; Usage

;; Run one of these commands:

;; `ab-select-clojure-def*': try to select various Clojure def* expressions
;; `ab-set-frame-height-to-clojure-def*': match frame height to clj def* expr

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

(defvar ab-clojure-def-forms
  (list 'def
        'defn
        'defn-
        'defmacro
        'defmethod
        'defmulti
        'defrecord
        'deftype
        'defprotocol
        'extend-protocol))

;;;;; Commands

;;;###autoload
(defun ab-select-clojure-def* ()
  "Guess the bounds of some Clojure def-form around point."
  (interactive)
  (condition-case err
      (cl-destructuring-bind (start end) (ab-sexp-bounds ab-clojure-def-forms)
	(set-mark start)
	(goto-char end)
	(activate-mark))
    (wrong-number-of-arguments
     (message "Failed to find containing def* form."))
    (error
     (message "Error: %s %s" (car err) (cdr err)))))

;;;###autoload
(defun ab-set-frame-height-to-clojure-def* ()
  "Set the frame height to the bounds of some clojure def-form around point."
  (interactive)
  (condition-case err
      (cl-destructuring-bind (start end) (ab-sexp-bounds ab-clojure-def-forms)
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
(defun ab-match-frame-to-clojure-def* ()
  "Match the frame to the bounds of some clojure def-form around point."
  (interactive)
  (condition-case err
      (cl-destructuring-bind (start end) (ab-sexp-bounds ab-clojure-def-forms)
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

(provide 'ab-clojure)

;;; ab-clojure.el ends here
