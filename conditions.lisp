;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package nkeymaps/conditions
  (:use #:common-lisp)
  (:export
   #:cycle
   #:duplicate-modifiers
   #:override-existing-binding
   #:bad-modifier
   #:make-key-required-arg
   #:empty-keyspec
   #:empty-value
   #:empty-modifiers
   #:bad-keyspec)
  (:documentation "Package listing conditions."))
(in-package :nkeymaps/conditions)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :nkeymaps/conditions))

(define-condition cycle (warning)
  ((message :initarg :message :accessor message :initform "Cycle detect in keymap")
   (keymap :initarg :keymap :accessor keymap :initform (alex:required-argument 'keymap)))
  (:report (lambda (c stream)
             (format stream "~a ~a" (message c) (keymap c))))
  (:documentation "Warning raised when keymap has cycles.
This is possible if a bound value is a keymap that occured before."))

(define-condition duplicate-modifiers (warning)
  ((message :initarg :message :accessor message :initform "Duplicate modifiers")
   (modifiers :initarg :modifiers :accessor modifiers :initform (alex:required-argument 'modifiers)))
  (:report (lambda (c stream)
             (format stream "~a: ~a" (message c) (modifiers c))))
  (:documentation "Warning raised when a keyspec contains multiple occurences of the same
modifiers."))

(define-condition override-existing-binding (warning)
  ((message :initarg :message :accessor message :initform "Key was bound to")
   (existing-binding-value
    :initarg :existing-binding-value
    :accessor existing-binding-value
    :initform (alex:required-argument 'existing-binding-value)))
  (:report (lambda (c stream)
             (format stream "~a ~a" (message c) (existing-binding-value c))))
  (:documentation "Warning raised overriding an existing binding."))

(define-condition bad-modifier (error)
  ((message :initarg :message :accessor message :initform ""))
  (:report (lambda (c stream)
             (format stream "~a" (message c))))
  (:documentation "Condition raised when we didn't get a modifier as expected."))

(define-condition make-key-required-arg (error)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "~a" "One of CODE or VALUE must be given.")))
  (:documentation "Error raised when `nkeymaps::make-key' is called without CODE or VALUE."))

(define-condition empty-keyspec (error)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "~a" "Empty keyspec.")))
  (:documentation "Error raised when a `keyspec' is empty, since, it's not allowed."))

(define-condition empty-value (error)
  ((keyspec
    :initarg :keyspec
    :accessor keyspec
    :initform (alex:required-argument 'keyspec)))
  (:report (lambda (c stream)
             (format stream "Empty key code or value in keyspec ~s" (keyspec c))))
  (:documentation "Error raised when keyspec has no CODE nor VALUE."))

(define-condition empty-modifiers (error)
  ((keyspec
    :initarg :keyspec
    :accessor keyspec
    :initform (alex:required-argument 'keyspec)))
  (:report (lambda (c stream)
             (format stream "Empty modifier(s) in keyspec ~s." (keyspec c))))
  (:documentation "Error raised when modifier is the empty string in a keyspec."))

(define-condition bad-keyspec (warning)
  ((message :initarg :message :accessor message :initform "Illegal keyspec")
   (error-condition
    :initarg :error-condition
    :accessor error-condition
    :initform (alex:required-argument 'error-condition)))
  (:report (lambda (c stream)
             (format stream "~a: ~a" (message c) (error-condition c))))
  (:documentation "Warning raised when trying to derive a key from an illegal keyspec."))
