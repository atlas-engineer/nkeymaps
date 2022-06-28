;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package nkeymaps/core
  (:use #:common-lisp
        #:nkeymaps/types
        #:nkeymaps/conditions)
  (:import-from #:fset)
  (:export
   #:define-modifier
   #:modifier=
   #:+control+
   #:+meta+
   #:+shift+
   #:+super+
   #:+hyper+



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

   #:translate-remove-shift-toggle-case
   #:translate-remove-shift
   #:translate-remove-but-first-control
   #:translate-remove-shift-but-first-control
   #:translate-remove-shift-but-first-control-toggle-case
   #:translate-shifted-control-combinations
   #:*translator*

   #:*print-shortcut*
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

(uiop:define-package nkeymaps/keyscheme
  (:use #:common-lisp)
  (:import-from #:nkeymaps/core #:make-keyscheme)
  (:export
   #:cua
   #:default
   #:emacs
   #:vi-normal
   #:vi-insert)
  (:documentation "Package holding the list of well-known keyschemes.
We use a dedicated package so that keyschemes can easily be listed and completed."))

(uiop:define-package nkeymaps
  (:use #:common-lisp)
  (:use-reexport
   #:nkeymaps/types
   #:nkeymaps/conditions
   #:nkeymaps/core
   #:nkeymaps/keyscheme)
  (:documentation "
The workflow goes as follows:
- Make a keymap with `make-keymap'.
- Define a binding on it with `define-key'.
- Lookup this binding with `lookup-key'.

Some globals can be tweaked to customize the library to your needs:

- `*translator*': The function to infer the right binding when
  the exact binding hits nothing.
- `*print-shortcuts*': Print modifiers using their short form instead of the
  full name, e.g. \"C\" instead of \"control\"."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :nkeymaps/core)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :nkeymaps))
