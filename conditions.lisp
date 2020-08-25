;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :keymap)

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
