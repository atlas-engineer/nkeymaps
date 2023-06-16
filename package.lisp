;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nkeymaps/core
  (:use :common-lisp
        :nkeymaps/types
        :nkeymaps/conditions)
  (:import-from :fset)
  (:import-from :alexandria
                #:curry
                #:rcurry)
  (:export
   #:define-modifier
   #:modifier=

   #:key
   #:make-key
   #:copy-key
   #:key-code
   #:key-value
   #:key-modifiers
   #:key-status
   #:key=

   #:keymap
   #:make-keymap
   #:keymap-p
   #:define-key
   #:lookup-key
   #:parents
   #:name
   #:bound-type
   #:modifiers

   #:*translator*

   #:*print-keyspec-style*
   #:keys->keyspecs

   #:keymap->map
   #:keymap-with-parents->map
   #:binding-keys

   #:compose

   ;; scheme
   #:keyscheme
   #:make-keyscheme
   #:keyscheme-p
   #:keyscheme-map
   #:keyscheme-map-p
   #:define-keyscheme-map
   #:get-keymap
   #:make-keyscheme-map)
  (:documentation "See the `nkeymaps' package documentation."))

(uiop:define-package :nkeymaps/keyscheme
  (:use :common-lisp)
  (:import-from :nkeymaps/core #:make-keyscheme)
  (:export
   #:cua
   #:default
   #:emacs
   #:vi-normal
   #:vi-insert)
  (:documentation "Package holding the list of well-known keyschemes.
We use a dedicated package so that keyschemes can easily be listed and completed."))

(uiop:define-package :nkeymaps/modifier
  (:use :common-lisp)
  (:import-from :nkeymaps/core #:define-modifier)
  (:export
   #:+control+
   #:+meta+
   #:+shift+
   #:+super+
   #:+hyper+)
  (:documentation "Package holding the list of predefined modifiers.
We use a dedicated package so that modifiers can easily be listed and completed.
See `nkeymaps:define-modifier'."))

(uiop:define-package :nkeymaps/translator
  (:use :common-lisp :nkeymaps/core :nkeymaps/modifier)
  (:export
   #:translate-remove-shift-toggle-case
   #:translate-remove-shift
   #:translate-remove-but-first-control
   #:translate-remove-shift-but-first-control
   #:translate-remove-shift-but-first-control-toggle-case
   #:translate-shift-control-combinations)
  (:documentation "Package holding the list of predefined translators.
We use a dedicated package so that modifiers can easily be listed and completed.
See `nkeymaps:*translator*'."))

(uiop:define-package :nkeymaps
  (:use :common-lisp)
  (:use-reexport
   :nkeymaps/types
   :nkeymaps/conditions
   :nkeymaps/core
   :nkeymaps/keyscheme
   :nkeymaps/modifier
   :nkeymaps/translator)
  (:documentation "
The workflow goes as follows:
- Make a keymap with `nkeymaps:make-keymap'.
- Define a binding on it with `nkeymaps:define-key'.
- Lookup this binding with `nkeymaps:lookup-key'.

Example:

\(let* ((parent-keymap (nkeymaps:make-keymap \"parent-keymap\"))
       (my-keymap (nkeymaps:make-keymap \"my-keymap\" parent-keymap)))
  (nkeymaps:define-key parent-keymap
    \"C-c\" 'copy
    \"C-v\" 'paste)
  (nkeymaps:define-key my-keymap
    \"C-x\" 'cut)
  (values
   (nkeymaps:lookup-key \"C-x\" parent-keymap)
   (nkeymaps:lookup-key \"C-x\" my-keymap)
   (nkeymaps:lookup-key \"C-c\" my-keymap)
   (nkeymaps:binding-keys 'cut parent-keymap)
   (nkeymaps:binding-keys 'copy my-keymap)
   (nkeymaps:binding-keys 'paste my-keymap)))
;; => NIL, CUT, COPY;; => NIL, CUT, COPY, NIL, (\"C-c\"), (\"C-v\")

Notice that the inverse of `nkeymaps:lookup-key' is `nkeymaps:binding-keys'.

Another workflow is to use `nkeymaps:keyscheme's, which allows to compose
different binding styles.

Example:

\(nkeymaps:define-keyscheme-map \"test\" ()
                               nkeymaps:cua '(\"C-c\" copy
                                              \"C-v\" paste)
                               nkeymaps:emacs '(\"C-x\" cut))

The default keyschemes can be listed from the `nkeymaps/keyscheme:' package
exported symbols.

New keyschemes can be created with `nkeymaps:make-keyscheme'.

Keys can be created with `nkeymaps:make-key', which gives you more fine-tuning
compared to the \"keyspecs\" above:

  (nkeymaps:make-key :code 38 :value \"a\" :modifiers '(\"C\"))

You can also specify key codes from the keyspec directly.  For instance,
\"C-#10\" corresponds to keycode 10 with the `nkeymaps:+control+'.

Keymaps can be composed with `nkeymaps:compose'.

New modifiers can be defined with `nkeymaps:define-modifier'.

   (nkeymaps:define-modifier :string \"duper\" :shortcut \"D\")

Some globals can be tweaked to customize the library to your needs:

- `nkeymaps:*translator*': The function to infer the right binding when the
  exact binding hits nothing.
- `nkeymaps:*print-keyspec-style*': Dictates how modifiers are printed.
  Possible values are `:no-shortcuts', `:emacs' and `:cua'.
  E.g. `nkeymaps/modifier:+control+' may be printed as \"control\", \"C\", or
  \"Ctrl\", respectively for the values above."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :nkeymaps/core)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :nkeymaps))
