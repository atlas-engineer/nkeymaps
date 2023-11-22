;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem "nkeymaps"
  :version "1.1.0"
  :description "General-purpose keymap management à-la Emacs."
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/nkeymaps"
  :bug-tracker "https://github.com/atlas-engineer/nkeymaps/issues"
  :source-control (:git "https://github.com/atlas-engineer/nkeymaps.git")
  :license "BSD 3-Clause"
  :depends-on ("alexandria" "fset" "trivial-package-local-nicknames" "uiop" "str")
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
  :depends-on ("nkeymaps" "lisp-unit2")
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests")
               (:file "keyscheme-tests"))
  :perform (test-op (op c)
                    (eval-input
                     "(lisp-unit2:run-tests
                       :package :nkeymaps/tests
                       :run-contexts #'lisp-unit2:with-summary-context)")))
