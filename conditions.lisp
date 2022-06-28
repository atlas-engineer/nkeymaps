;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nkeymaps)

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
             (format stream "~a: ~a" (message c) (mapcar #'modifier-string (modifiers c)))))
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
             (format stream "~a" "One of CODE or VALUE must be given."))))

(define-condition empty-keyspec (error)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "~a" "Empty keyspec."))))

(define-condition empty-value (error)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "~a" "Empty key code or value."))))

(define-condition empty-modifiers (error)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "~a" "Empty modifier(s)."))))
