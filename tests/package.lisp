;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-user)

(uiop:define-package :nkeymaps/tests
  (:use :common-lisp :lisp-unit2)
  (:import-from :nkeymaps))

(in-package :nkeymaps/tests)

(defun cua-modifiers (body-fn)
  "A context that locally binds `nkeymaps:*print-shortcut*' to \"cua\"."
  (let ((nkeymaps:*print-shortcut* "cua"))
    (funcall body-fn)))
