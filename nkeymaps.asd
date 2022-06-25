;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem "nkeymaps"
  :version "1"
  :description "General-purpose keymap management à-la Emacs."
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/nkeymaps"
  :license "BSD 3-Clause"
  :depends-on (alexandria fset str trivial-package-local-nicknames)
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "conditions")
               (:file "keymap")
               (:file "scheme")
               (:file "scheme-names"))
  :in-order-to ((test-op (test-op "nkeymaps/tests"))))

(defsystem "nkeymaps/tests"
  :depends-on (alexandria fset nkeymaps prove)
  :components ((:file "test-package"))
  :perform (test-op (op c)
                    (symbol-call :prove :run (system-relative-pathname c "tests/"))))
