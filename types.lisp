;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package nkeymaps/types
  (:use #:common-lisp)
  (:export #:list-of)
  (:documentation "Package for types.
It's useful to have a separate package because some types may need to generate
functions for the `satisfies' type condition."))
(in-package :nkeymaps/types)

;; trivial-types:proper-list doesn't check its element type.

(defun list-of-p (list type)
  "Return non-nil if LIST contains only elements of the given TYPE."
  (and (listp list)
       (every (lambda (x) (typep x type)) list)))

(deftype list-of (type)
  "The empty list or a proper list of TYPE elements.
Unlike `(cons TYPE *)', it checks all the elements.
`(cons TYPE *)' does not accept the empty list."
  (let ((predicate-name (intern (uiop:strcat "LIST-OF-" (symbol-name type) "-P")
                                (find-package :nkeymaps/types))))
    (unless (fboundp predicate-name)
      (setf (fdefinition predicate-name)
            (lambda (object)
              (list-of-p object type))))
    `(and list (satisfies ,predicate-name))))
