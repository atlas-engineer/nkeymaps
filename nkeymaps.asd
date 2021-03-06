;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem "nkeymaps"
  :version "1.0.0"
  :description "General-purpose keymap management à-la Emacs."
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/nkeymaps"
  :license "BSD 3-Clause"
  :depends-on (alexandria fset trivial-package-local-nicknames uiop)
  :serial t
  :components ((:file "types")
               (:file "conditions")
               (:file "package")
               (:file "keymap")
               (:file "modifiers")
               (:file "translators")
               (:file "keyscheme-map")
               (:file "keyschemes"))
  :in-order-to ((test-op (test-op "nkeymaps/tests"))))

(defsystem "nkeymaps/tests"
  :depends-on (alexandria fset nkeymaps lisp-unit2)
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "tests")
               (:file "keyscheme-tests"))
  :perform (test-op (op c)
                    (symbol-call :lisp-unit2 :run-tests :package :nkeymaps/tests
                                 :run-contexts (find-symbol "WITH-SUMMARY-CONTEXT" :lisp-unit2))))
