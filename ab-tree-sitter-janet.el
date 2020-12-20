;;; ab-tree-sitter-janet.el --- tree-sitter-janet -*- lexical-binding: t; -*-

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

;; (require 'ab-tree-sitter-janet)

;;;; Usage

;; Run one of the commands:

;; `ab-insert-repl-form': insert a repl form within certain defining forms

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
(require 'ab-tree-sitter)

;;;; Functions

;; XXX: modification of tree-sitter-node-at-point
(defun ab-tree-sitter-node-at-pos (&optional position node-type)
  "Return the smallest syntax node at POSITION whose type is NODE-TYPE.
If NODE-TYPE is nil, return the smallest syntax node at point."
  (let* ((root (tsc-root-node tree-sitter-tree))
         (p (or position (point)))
         (node (tsc-get-descendant-for-position-range root p p)))
    (if node-type
        (let ((this node) result)
          (while this
            (if (equal node-type (tsc-node-type this))
                (setq result this
                      this nil)
              (setq this (tsc-get-parent this))))
          result)
      node)))

(defun ab-params-for-janet-def (&optional position)
  "Determine params for janet def* form containing POSITION."
  (if (not tree-sitter-mode)
      (message "tree-sitter-mode must be enabled")
    (let* ((position (or position (point)))
           (tl-node (ab-top-level-node-for
                     (ts-node-start-position
                      (ab-tree-sitter-node-at-pos position)))))
      (when (and tl-node
                 (eq (ts-node-type tl-node)
                     'extra_defs))
        (when-let ((params-node
                    (tsc-get-child-by-field tl-node :parameters)))
          (let ((node (tsc-get-nth-named-child params-node 0))
                (pnode-names '()))
            (while node
              ;; XXX: don't save & and a few other things?
              (setq pnode-names
                    (cons (ts-node-text node)
                          pnode-names))
              (setq node (tsc-get-next-named-sibling node)))
            pnode-names))))))

(defun ab-make-janet-repl-form (sym-names)
  "Create repl form using SYM-NAMES."
  (format "(repl nil nil (merge-into (make-env)\n@{%s}))"
          (mapconcat (lambda (sname)
                       (format "'%s @{:value %s}" sname sname))
                     sym-names
                     "\n")))

;;;###autoload
(defun ab-insert-repl-form (&optional position)
  "Insert a repl form near POSITION."
  (interactive)
  (if (not tree-sitter-mode)
      (message "tree-sitter-mode must be enabled")
    (let ((position (or position (point))))
      ;; XXX: determine relevant local names too?
      (insert (ab-make-janet-repl-form (ab-params-for-janet-def position)))
      (indent-region position (point)))))

;;;; Footer

(provide 'ab-tree-sitter-janet)

;;; ab-tree-sitter-janet.el ends here
