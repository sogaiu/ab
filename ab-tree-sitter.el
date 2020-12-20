;;; ab-tree-sitter.el --- A.B. using Tree Sitter -*- lexical-binding: t; -*-

;; Author: sogaiu
;; Version: 20200820
;; Package-Requires: ((emacs "26.2"))
;; Keywords: bounds tree-sitter

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Code related to ascertaining the bounds of expressions.

;;;; Installation

;;;;; Manual

;; Put this file in your load-path, and put this in your init
;; file:

;; (require 'ab-tree-sitter)

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

;; XXX: what is it that we actually need?
(require 'tree-sitter)

;;;; Functions

(defun ab-top-level-node-for (&optional position)
  "Determine the top level node containing POSITION."
  (if (not tree-sitter-mode)
      (message "tree-sitter-mode must be enabled")
    (let* ((position (or position (point)))
           (root (tsc-root-node tree-sitter-tree))
           (node
            (tsc-get-descendant-for-position-range root
                                                   (position-bytes position)
                                                   (position-bytes position)))
           (target-node nil)
           (last-node nil))
      (while node
        (setq target-node last-node)
        (setq last-node node)
        (setq node (tsc-get-parent node)))
      (or target-node last-node))))

(defun ab-top-level-bounds-for (&optional position)
  "Determine the bounds for the top level node containing POSITION."
  (if (not tree-sitter-mode)
      (message "tree-sitter-mode must be enabled")
    (let ((node (ab-top-level-node-for (or position (point)))))
      (list (tsc-node-start-position node)
            (tsc-node-end-position node)))))

;;;###autoload
(defun ab-select-top-level-for (&optional position)
  "Select the top level form containing POSITION."
  (interactive)
  (if (not tree-sitter-mode)
      (message "tree-sitter-mode must be enabled")
    (condition-case err
        (cl-destructuring-bind (start end) (ab-top-level-bounds-for
                                            (or position (point)))
	  (set-mark start)
	  (goto-char end)
	  (activate-mark))
      (wrong-number-of-arguments
       (message "Failed to find containing top-level form."))
      (error
       (message "Error: %s %s" (car err) (cdr err))))))

;; XXX: try to match width as well
;;;###autoload
(defun ab-match-frame-to-top-level-for (&optional position)
  "Match the frame to the bounds of top level form containing POSITION."
  (interactive)
  (if (not tree-sitter-mode)
      (message "tree-sitter-mode must be enabled")
    (condition-case err
        (cl-destructuring-bind (start end) (ab-top-level-bounds-for
                                            (or position (point)))
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
       (message "Error: %s %s" (car err) (cdr err))))))

;;;; Footer

(provide 'ab-tree-sitter)

;;; ab-tree-sitter.el ends here
