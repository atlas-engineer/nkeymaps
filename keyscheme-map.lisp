;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nkeymaps/core)

(defclass keyscheme ()
  ((name :initarg :name
         :accessor name
         :type string
         :documentation "A scheme name.")
   ;; TODO: Remove `parents' and use superclasses instead.
   (parents :initarg :parents
            :accessor parents
            :initform '()
            :type (list-of keyscheme)
            :documentation "The list of parents.  When a scheme is defined, the
keymap parents are automatically set to the keymaps corresponding to the given
keyschemes.  See `define-keyscheme-map'.")
   (bound-type :accessor bound-type
               :initarg :bound-type
               :initform *default-bound-type*
               :documentation
               "Type of the bound-value.
The type is enforced in `define-keyscheme-map' at macro-expansion time.
Type should allow `keymap's, so it should probably be in the form
\(or keymap NON-KEYMAP-BOUND-TYPE).")
   (modifiers :accessor modifiers
              :initarg :modifiers
              :initform (fset:convert 'fset:set  *modifier-list*)
              :type fset:wb-set
              :documentation "
Accepted modifiers for this `keyscheme'."))
  (:documentation "A keyscheme is best understood as a conventional family of bindings.
See `nkeymaps/keyscheme:cua' for an example.
Keyschemes can be associated to `keymap's with `keyscheme-map'."))

(defmethod print-object ((object keyscheme) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a" (name object))))

(declaim (ftype (function (string &rest keyscheme) (values keyscheme &optional))
                make-keyscheme))
(defun make-keyscheme (name &rest parents)
  "Return a new `keyscheme' object.
The scheme name inherits from the optional PARENTS, ordered by priority.

Example:

  (defvar emacs (make-keyscheme \"emacs\" cua))

In the above, we define a new scheme name called `emacs` which inherits from the
existing keyscheme `cua`."
  (the (values keyscheme &optional)
       (make-instance 'keyscheme
                      :name name
                      :parents parents)))

(defun keyscheme-p (object)
  "Return non-nil if OBJECT is a `keyscheme'."
  (typep object 'keyscheme))

(declaim (ftype (function (hash-table) boolean) scheme-p))
(defun keyscheme-map-p (object)
  "Return non-nil if OBJECT is a `keyscheme-map'."
  (loop :for name :being :the hash-keys :in object :using (:hash-value keymap)
        :always (and (keyscheme-p name)
                     (keymap-p keymap))))

(deftype keyscheme-map ()
  "A `hash-table' mapping `keyscheme's to `keymap's."
  `(and hash-table
        (satisfies keyscheme-map-p)))

(defun copy-keyscheme-map (keyscheme-map)
  (let ((new-keyscheme-map (make-hash-table :test #'equal)))
    (maphash (lambda (keyscheme keymap)
               (setf (gethash keyscheme new-keyscheme-map)
                     (copy-keymap keymap)))
             keyscheme-map)
    new-keyscheme-map))

(declaim (ftype (function (string list
                                  keyscheme list
                                  &rest (or keyscheme list))
                          keyscheme-map)
                define-keyscheme-map))
(defun define-keyscheme-map (name-prefix options keyscheme bindings
                             &rest more-keyschemes+bindings-pairs)
  "Return a keyscheme-map, a hash table with `keyscheme's as key and `keymap's
holding BINDINGS as value.

The keymap names are prefixed with NAME-PREFIX and suffixed with \"-map\".

OPTIONS is list of keyword arguments.
For now the only supported option is IMPORT.

When given a `keyscheme-map' to IMPORT, it is used as initial values for the new
keyscheme-map.  The content is copied.  Further alteration to the imported
keyscheme-map won't reflect on this newly define keyscheme-map.

This is a macro like `define-key' so that it can type-check the BINDINGS
keyspecs at compile-time.

Example:

  (define-keyscheme-map \"my-mode\" '()
    nkeymaps/keyscheme:cua (list
                            \"C-c\" 'copy
                            \"C-v\" 'paste)

    nkeymaps/keyscheme:emacs '(\"M-w\" copy
                               \"M-y\" paste))

`nkeymaps/keyscheme:cua' and `nkeymaps/keyscheme:emacs' are pre-defined keyschemes.
To define a new keyscheme, see `make-keyscheme'.

`nkeymaps/keyscheme:cua' is a parent of `nkeymaps/keyscheme:emacs'; thus, in the
above example, the Emacs keymap will have the CUA keymap as parent.
The keyscheme-map keymaps are named \"my-mode-cua-map\" and
\"my-mode-emacs-map\"."
  (destructuring-bind (&key import) options
    (let ((name+bindings-pairs (append (list keyscheme bindings) more-keyschemes+bindings-pairs))
          (keyscheme-map (if import
                             (copy-keyscheme-map import)
                             (make-hash-table :test #'equal))))
      (unless import
        (alex:doplist (keyscheme _ name+bindings-pairs)
          (setf (gethash keyscheme keyscheme-map)
                (let ((new-keymap (make-keymap (format nil "~a-~a-map" name-prefix (name keyscheme)))))
                  (setf (modifiers new-keymap) (modifiers keyscheme))
                  (setf (bound-type new-keymap) (bound-type keyscheme))
                  new-keymap))))
      ;; Set parents now that all keymaps exist.
      (maphash (lambda (keyscheme keymap)
                 (setf (parents keymap)
                       (delete nil
                               (mapcar (lambda (parent-keyscheme) (gethash parent-keyscheme keyscheme-map))
                                       (parents keyscheme)))))
               keyscheme-map)
      ;; Set bindings.
      (alex:doplist (keyscheme bindings name+bindings-pairs)
        (let ((keymap (gethash keyscheme keyscheme-map)))
          (alex:doplist (keyspecs bound-value bindings)
            (define-key keymap keyspecs bound-value))))
      keyscheme-map)))

(defun quoted-symbol-p (arg)
  (and (listp arg)
       (eq (first arg) 'quote)
       (= 2 (length arg))))

(define-compiler-macro define-keyscheme-map (&whole form
                                                    name-prefix options
                                                    keyscheme bindings
                                                    &rest more-keyscheme+bindings-pairs)
  "See the `define-key' compiler-macro for why we need one here too."
  (declare (ignore name-prefix options))
  (let ((keyscheme+bindings-pairs (append (list keyscheme bindings) more-keyscheme+bindings-pairs)))
    (alex:doplist (keyscheme quoted-bindings keyscheme+bindings-pairs)
      (when (and (symbolp keyscheme)
                 (boundp keyscheme))
        (check-type (symbol-value keyscheme) keyscheme))
      ;; (log:info quoted-bindings)
      (let ((bindings (if (eq 'quote (first quoted-bindings))
                          (first (rest quoted-bindings))
                          (rest quoted-bindings)) ))
        ;; (log:info bindings)
        ;; (check-type bindings list)
        (alex:doplist (keyspecs bound-value bindings)
          (when (stringp keyspecs)
            (check-type keyspecs (or keyspecs-type list)))
          (when (and (boundp keyscheme) (quoted-symbol-p bound-value))
            (assert (typep (second bound-value) (bound-type (symbol-value keyscheme))) (bound-value)
                    'type-error :datum (second bound-value) :expected-type (bound-type (symbol-value keyscheme))))))))
  form)

(declaim (ftype (function (keyscheme keyscheme-map) (or keymap null)) get-keymap))
(defun get-keymap (keyscheme keyscheme-map)
  "Return keymap corresponding to KEYSCHEME in KEYSCHEME-MAP.
If no keymap is found, try with KEYSCHEME's `parents'.
For instance, if KEYSCHEME has a `nkeymaps/keyscheme:cua' keymap and no
`nkeymaps/keyscheme:emacs' keymap, this function returns the
`nkeymaps/keyscheme:cua' keymap when NAME is `nkeymaps/keyscheme:emacs'.
Return nil if nothing is found."
  (or (gethash keyscheme keyscheme-map)
      (when (parents keyscheme)
        (some (alexandria:rcurry #'get-keymap keyscheme-map) (parents keyscheme)))))

(declaim (ftype (function (keyscheme keymap &rest (or keyscheme keymap)) keyscheme-map) make-keyscheme-map))
(defun make-keyscheme-map (keyscheme keymap &rest more-keyscheme+keymap-pairs)
  "Return a new scheme associating KEYSCHEME to KEYMAP.
With MORE-KEYSCHEME+KEYMAP-PAIRS, include those names and keymaps as well.  This is
useful in complement to `define-keyscheme-map' to make a scheme with pre-existing
keymaps."
  (let ((keyscheme-map (make-hash-table :test #'equal))
        (keyscheme+keymap-pairs (append (list keyscheme keymap) more-keyscheme+keymap-pairs)))
    (alex:doplist (keyscheme keymap keyscheme+keymap-pairs)
      (setf (gethash keyscheme keyscheme-map) keymap))
    keyscheme-map))
