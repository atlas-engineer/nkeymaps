;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(uiop:define-package :nkeymaps/tests
  (:use :common-lisp :lisp-unit2)
  (:import-from :nkeymaps))

(in-package :nkeymaps/tests)

(defun with-emacs-keyspec-context (body-fn)
  "A context that locally binds `nkeymaps:*print-keyspec-style*' to \"emacs\"."
  (let ((nkeymaps:*print-keyspec-style* "emacs"))
    (funcall body-fn)))
