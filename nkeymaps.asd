;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem "nkeymaps"
  :version "1.0.0"
  :description "General-purpose keymap management à-la Emacs."
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/nkeymaps"
  :license "BSD 3-Clause"
  :depends-on ("alexandria" "fset" "trivial-package-local-nicknames" "uiop")
  :serial t
  :components ((:file "types")
               (:file "conditions")
               (:file "package")
               (:file "keymap")
               (:file "modifiers")
               (:file "translators")
               (:file "keyscheme-map")
               (:file "keyschemes"))
  :in-order-to ((test-op (test-op "nkeymaps/tests")
                         (test-op "nkeymaps/tests/compilation"))))


(defsystem "nkeymaps/submodules"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-submodule-system)

(defsystem "nkeymaps/tests"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-test-system
  :depends-on ("nkeymaps")
  :targets (:package :nkeymaps/tests)
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests")
               (:file "keyscheme-tests")))

(defsystem "nkeymaps/tests/compilation"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-compilation-test-system
  :depends-on ("nkeymaps")
  :packages (:nkeymaps :nkeymaps/translator :nkeymaps/modifier :nkeymaps/keyscheme :nkeymaps/core))
