;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; TODO: Reexport nkeymaps/keyscheme.
(uiop:define-package nkeymaps
  (:use #:common-lisp #:nkeymaps/types)
  (:import-from #:fset)
  (:export
   #:modifier=
   #:+control+
   #:+meta+
   #:+shift+
   #:+super+
   #:+hyper+

   #:*modifier-list*

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

   ;; conditions
   #:cycle
   #:duplicate-modifiers
   #:override-existing-binding
   #:bad-modifier
   #:make-key-required-arg
   #:empty-keyspec
   #:empty-value
   #:empty-modifiers

   ;; scheme
   #:keyscheme
   #:make-keyscheme
   #:keyscheme-p
   #:keyscheme-map
   #:keyscheme-map-p
   #:define-keyscheme-map
   #:get-keymap
   #:make-keyscheme-map)
  (:documentation "
The workflow goes as follows:
- Make a keymap with `make-keymap'.
- Define a binding on it with `define-key'.
- Lookup this binding with `lookup-key'.

Some globals can be tweaked to customize the library to your needs:

- `*modifier-list*': List of known keyboard modifiers like `+control+'.
- `*translator*': The function to infer the right binding when
  the exact binding hits nothing.
- `*print-shortcuts*': Print modifiers using their short form instead of the
  full name, e.g. \"C\" instead of \"control\".
- `*default-bound-type*': The allowed type for bound values; default to T (everything)."))

(uiop:define-package nkeymaps/keyscheme
  (:use #:common-lisp)
  (:import-from #:nkeymaps #:make-keyscheme)
  (:export
   #:cua
   #:emacs
   #:vi-normal
   #:vi-insert)
  (:documentation "Package holding the list of well-known keyschemes.
We use a dedicated package so that keyschemes can easily be listed and completed."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :nkeymaps)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :nkeymaps/keyscheme))
