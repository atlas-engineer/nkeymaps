;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nkeymaps)

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
\(or keymap NON-KEYMAP-BOUND-TYPE).")))

(defmethod print-object ((object keyscheme) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a" (name object))))

(declaim (ftype (function (string &rest keyscheme) keyscheme)
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

(defun keyscheme-p (name)
  (typep name 'keyscheme))

(declaim (ftype (function (hash-table) boolean) scheme-p))
(defun keyscheme-map-p (keyscheme-map)
  (loop :for name :being :the hash-keys :in keyscheme-map :using (:hash-value keymap)
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

(declaim (ftype (function (string (or null keyscheme-map) keyscheme list &rest (or keyscheme list))
                          keyscheme-map)
                define-scheme*))
(defun define-keyscheme-map* (name-prefix imported-keyscheme-map keyscheme bindings &rest more-keyschemes+bindings-pairs)
  "Define `keyscheme-map'.
See `define-keyscheme-map' for the user-facing function."
  (let ((name+bindings-pairs (append (list keyscheme bindings) more-keyschemes+bindings-pairs))
        (keyscheme-map (if imported-keyscheme-map
                           (copy-keyscheme-map imported-keyscheme-map)
                           (make-hash-table :test #'equal))))
    (unless imported-keyscheme-map
      (loop :for (keyscheme nil . nil) :on name+bindings-pairs :by #'cddr
            :do (setf (gethash keyscheme keyscheme-map)
                      (let ((new-keymap (make-keymap (format nil "~a-~a-map" name-prefix (name keyscheme)))))
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
    (loop :for (keyscheme bindings . nil) :on name+bindings-pairs :by #'cddr
          :for keymap = (gethash keyscheme keyscheme-map)
          :do (loop :for (keyspecs bound-value . nil) :on bindings :by #'cddr
                    :do (define-key* keymap keyspecs bound-value))) ; TODO: Can we use define-key?
    keyscheme-map))

(defun check-plist (plist &rest keys)
  "Raise error if PLIST has keys not in KEYS."
  (let ((extra-keys)
        (all-keys))
    (alexandria:doplist (k v plist)
      (push k all-keys)
      (unless (member k keys)
        (push k extra-keys)))
    (if extra-keys
        (error "Allowed keys are ~a, got ~a." keys all-keys)
        t)))

(defun quoted-symbol-p (arg)
  (and (listp arg)
       (eq (first arg) 'quote)
       (= 2 (length arg))))

;; TODO: Replace with compiler-macro.  Then test type errors.
;; TODO: Replace keyscheme-specifier with name followed by options.
(defmacro define-keyscheme-map (keyscheme-specifier keyscheme bindings &rest more-keyscheme+bindings-pairs)
  "Return a keyscheme-map, a hash table with `keyscheme's as key and `keymap's
holding BINDINGS as value.

KEYSCHEME-SPECIFIER is either a string or a plist in the form

  (:name-prefix NAME-PREFIX :import IMPORTED-SCHEME)

The keymap names are prefixed with NAME-PREFIX or KEYSCHEME-SPECIFIER (if a
string) and suffixed with \"-map\".

This is a macro like `define-key' so that it can type-check the BINDINGS
keyspecs at compile-time.

Example:

  (define-keyscheme-map \"my-mode\"
    nkeymaps/keyscheme:cua (list
                   \"C-c\" 'copy
                   \"C-v\" 'paste)

    nkeymaps/keyscheme:emacs '(\"M-w\" copy
                      \"M-y\" paste))

`nkeymaps/keyscheme:cua' and `nkeymaps/keyscheme:emacs' are pre-defined keyschemes.  To define a new
keyscheme, see `make-keyscheme'.

`nkeymaps/keyscheme:cua' is a parent of `nkeymaps/keyscheme:emacs'; thus, in the above example,
the Emacs keymap will have the CUA keymap as parent.
The keyscheme-map keymaps are named \"my-mode-cua-map\" and \"my-mode-emacs-map\"."
  (let ((keyscheme+bindings-pairs (append (list keyscheme bindings) more-keyscheme+bindings-pairs))
        (name-prefix (if (stringp keyscheme-specifier)
                         keyscheme-specifier
                         (getf keyscheme-specifier :name-prefix)))
        (imported-keyscheme-map (unless (stringp keyscheme-specifier)
                                  (getf keyscheme-specifier :import))))
    (unless (stringp keyscheme-specifier)
      (check-plist keyscheme-specifier :name-prefix :import))
    (loop :for (keyscheme quoted-bindings . nil) :on keyscheme+bindings-pairs :by #'cddr
          :for bindings = (rest quoted-bindings)
          :do (check-type (symbol-value keyscheme) keyscheme)
          :do (check-type bindings list)
          :do (loop :for (keyspecs bound-value . nil) :on bindings :by #'cddr
                    :do (check-type keyspecs (or keyspecs-type list))
                    :when (quoted-symbol-p bound-value)
                      :do (assert (typep (second bound-value) (bound-type (symbol-value keyscheme))) (bound-value)
                                  'type-error :datum (second bound-value) :expected-type (bound-type (symbol-value keyscheme)))))
    `(progn
       (define-keyscheme-map* ,name-prefix ,imported-keyscheme-map ,keyscheme ,bindings ,@more-keyscheme+bindings-pairs))))

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
    ;; TODO: Use `alex:doplist'.
    (loop :for (keyscheme keymap) :on keyscheme+keymap-pairs :by #'cddr
          :do (setf (gethash keyscheme keyscheme-map) keymap))
    keyscheme-map))
