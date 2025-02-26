;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nkeymaps/core)

(defstruct modifier
  (string "" :type string)
  (shortcut "" :type string))

(declaim (ftype (function ((or string modifier) (or string modifier)) boolean) modifier=))
(defun modifier= (string-or-modifier1 string-or-modifier2)
  "Return non-nil if STRING-OR-MODIFIER1 and STRING-OR-MODIFIER2 represent the same modifier."
  (unless (or (modifier-p string-or-modifier1)
              (modifier-p string-or-modifier2))
    (error 'bad-modifier :message "At least one of the arguments must be a modifier."))
  (flet ((match-modifier (modifier string)
           (or (string= (modifier-string modifier) string)
               (string= (modifier-shortcut modifier) string))))
    (cond
      ((stringp string-or-modifier1)
       (match-modifier string-or-modifier2 string-or-modifier1))
      ((stringp string-or-modifier2)
       (match-modifier string-or-modifier1 string-or-modifier2))
      (t
       (or (string= (modifier-string string-or-modifier1)
                    (modifier-string string-or-modifier2))
           (string= (modifier-shortcut string-or-modifier1)
                    (modifier-shortcut string-or-modifier2)))))))

(defmethod fset:compare ((x modifier) (y modifier))
  "Modifier sets need this comparison function to be ordered, so that (\"C\"
\"M\") is the same as (\"M\" \"C\")."
  (fset:compare-lexicographically (modifier-string x) (modifier-string y)))

(defparameter *modifier-list* (list)
  "List of known modifiers.
`make-key' and `define-key' raise an error when setting a modifier that is not
in this list.")

(defun define-modifier (&rest args &key string shortcut)
  "Return a new modifier.
It is registered globally and can be used from any new keymap, unless the keymap
filters out the modifier in its `modifiers' slot."
  (declare (ignore string shortcut))
  (let ((result (apply #'make-modifier args)))
    (setf *modifier-list* (delete result *modifier-list*
                                  :test #'modifier=))
    (push result *modifier-list*)
    result))

(deftype key-status-type ()
  `(member :pressed :released))

(deftype modifier-list ()
  "List of modifiers are sets: they are ordered and have no duplicates."
  `fset:wb-set)

(defstruct (key (:constructor %make-key (code value modifiers status))
                (:copier %copy-key))
  "The fundamental type to represent any input, be it keyboard, mouse or others."
  (code 0 :type integer) ; TODO: Can a keycode be 0?  I think not, so 0 might be a good non-value.
  (value "" :type string)
  (modifiers (fset:set) :type modifier-list)
  (status :pressed :type key-status-type))

(defun key= (key1 key2)
  "Two keys are equal if the have the same modifiers, status and key code.
If codes don't match, the values are compared instead.  This way, code-matching
keys match before the value which is usually what the users want when they
specify a key-code binding."
  (and (or (and (not (zerop (key-code key1)))
                (= (key-code key1)
                   (key-code key2)))
           (and
            (not (uiop:emptyp (key-value key1)))
            (string= (key-value key1)
                     (key-value key2))))
       (fset:equal? (key-modifiers key1)
                    (key-modifiers key2))
       (eq (key-status key1)
           (key-status key2))))

(declaim (ftype (function ((or string modifier)) modifier) modspec->modifier))
(defun modspec->modifier (string-or-modifier)
  "Return the `modifier' corresponding to STRING-OR-MODIFIER."
  (if (modifier-p string-or-modifier)
      string-or-modifier
      (let ((modifier (find-if (alex:curry #'modifier= string-or-modifier) *modifier-list*)))
        (or modifier
            (error 'bad-modifier
                   :message (format nil "Unknown modifier ~a" string-or-modifier))))))

(declaim (ftype (function ((or (list-of string)
                               (list-of modifier)
                               modifier-list))
                          modifier-list)
                modspecs->modifiers))
(defun modspecs->modifiers (strings-or-modifiers)
  "Return the list of `modifier's corresponding to STRINGS-OR-MODIFIERS."
  (flet ((list-difference (list1 list2)
           (dolist (list2-elt list2 list1)
             (setf list1 (delete list2-elt list1 :count 1)))))
    (if (typep strings-or-modifiers 'modifier-list)
        strings-or-modifiers
        (the (values modifier-list &optional)
             (fset:convert 'fset:set
                           (let* ((mods (mapcar #'modspec->modifier strings-or-modifiers))
                                  (no-dups-mods (delete-duplicates mods :test #'modifier=)))
                             (when (/= (length mods) (length no-dups-mods))
                               (warn 'duplicate-modifiers
                                     :modifiers (mapcar #'modifier-string (list-difference mods no-dups-mods))))
                             no-dups-mods))))))

(declaim (ftype (function (&key (:code integer) (:value string)
                                (:modifiers list) (:status keyword))
                          key)
                make-key))
(defun make-key (&key (code 0 explicit-code) (value "" explicit-value)
                      modifiers
                      (status :pressed))
  "Return new `key'.
Modifiers can be either a list of `modifier' types or of strings that will be
looked up in `*modifier-list*'."
  (unless (or explicit-code explicit-value)
    (error 'make-key-required-arg))
  (%make-key
   code
   value
   (modspecs->modifiers modifiers)
   status))

(declaim (ftype (function (key &key (:code integer) (:value string)
                               (:modifiers modifier-list) (:status keyword))
                          key)
                copy-key))
(defun copy-key (key &key (code (key-code key)) (value (key-value key))
                        (modifiers (key-modifiers key))
                        (status (key-status key)))
  "Return a new copy of KEY.
This is useful as a low-level function to translate keys without modifying existing keys."
  (let ((new-key (%copy-key key)))
    (setf (key-value new-key) value
          (key-code new-key) code
          (key-status new-key) status
          (key-modifiers new-key) (modspecs->modifiers modifiers))
    new-key))

(defmethod fset:compare ((x key) (y key))
  "Needed to use the KEY structure as keys in Fset maps.
Would we use the default comparison function, case-sensitivity would be lost on
key values because `fset:equal?' folds case."
  (if (key= x y)
      :equal
      :unequal))

(declaim (ftype (function (string &optional boolean) (or key null)) keyspec->key))
(defun keyspec->key (string &optional error-p)
  "Parse STRING and return a new `key'.
STRING must be passed in one of two forms:

  MODIFIERS-#<code>
  MODIFIERS-<value>

MODIFIERS are hyphen-separated modifiers as per `*modifier-list*'.
E.g. control-#1 or control-a.

Note that '-' or '#' as a last character is supported, e.g. 'control--' and
'control-#' are valid."
  (handler-bind ((error (if error-p
                            #'identity
                            (lambda (c)
                              (warn 'bad-keyspec :error-condition c)
                              (return-from keyspec->key nil)))))
    (when (string= string "")
      (error 'empty-keyspec))
    (let* ((last-nonval-hyphen (or (position #\- string :from-end t
                                                        :end (1- (length string)))
                                   -1))
           (code 0)
           (value "")
           (code-or-value (subseq string (1+ last-nonval-hyphen)))
           (rest (subseq string 0 (1+ last-nonval-hyphen)))
           (modifiers (butlast (uiop:split-string rest :separator "-"))))
      (when (find "" modifiers :test #'string=)
        (error 'empty-modifiers :keyspec string))
      (when (and (<= 2 (length code-or-value))
                 (string= (subseq code-or-value (1- (length code-or-value)))
                          "-"))
        (error 'empty-value :keyspec string))
      (if (and (<= 2 (length code-or-value))
               (string= "#" (subseq code-or-value 0 1)))
          (setf code (or (parse-integer code-or-value :start 1 :junk-allowed t)
                         code))
          (setf value code-or-value))
      (let ((unshifted-value (nkeymaps/translator:unshift value)))
        (make-key :code code
                  :value (or unshifted-value value)
                  :modifiers (if unshifted-value
                                 ;; Duplicates are not an issue.
                                 (pushnew "shift" modifiers :test #'string=)
                                 modifiers))))))

(declaim (ftype (function (string &optional boolean) (list-of key)) keyspecs->keys))
(defun keyspecs->keys (spec &optional error-p)
  "Parse SPEC and return corresponding list of keys."
  ;; TODO: Return nil if SPEC is invalid?
  (let* ((result (delete "" (uiop:split-string spec) :test #'string=)))
    (mapcar (rcurry #'keyspec->key error-p) result)))

(defparameter *translator* (lambda (keys) (list keys))
  "Key translator to use in `keymap' objects.
When no binding is found, call this function to
generate new bindings to lookup.  The function takes a list of `key' objects and
returns a list of list of keys.

This parameter can be let-bound around `lookup-key' calls.")

(defparameter *default-bound-type* '(or keymap t)
  "Default value for the `bound-type' slot of `keymap'.
Do not change this, instead create new a `keyscheme' or subclass `keymap'.")

(defclass keymap ()
  ((name :accessor name
         :initarg :name
         :initform "anonymous"
         :type string
         :documentation
         "Name of the keymap.
Used for documentation purposes, e.g. referring to a keymap by a well known name.")
   (entries :accessor entries
            :initarg :entries
            ;; We cannot use the standard (make-hash-table :test #'equalp)
            ;; because then (set "a") and (set "A") would be the same thing.
            :initform (fset:empty-map)
            :type fset:wb-map
            :documentation
            "Hash table of which the keys are key-chords and the values are a
symbol or a keymap.")
   (bound-type :accessor bound-type
               :initarg :bound-type
               :initform *default-bound-type*
               :documentation
               "Type of the bound-value.
The type is enforced in `define-key' at macro-expansion time.
Type should allow `keymap's, so it should probably be in the form
\(or keymap NON-KEYMAP-BOUND-TYPE).")
   (parents :accessor parents
            :initarg :parents
            :initform nil
            :type (list-of keymap)
            :documentation "List of parent keymaps.
Parents are ordered by priority, the first parent has highest priority.")
   (modifiers :accessor modifiers
              :initarg :modifiers
              :initform (fset:convert 'fset:set  *modifier-list*)
              :type modifier-list
              :documentation "
Accepted modifiers for this `keymap'."))
  (:documentation "A map of `key' bindings to values.
Values must be of type `bound-type'.
The bindings from the parents are inherited, with the children having precedence."))

(defmethod print-object ((keymap keymap) stream)
  (print-unreadable-object (keymap stream :type t :identity t)
    (format stream "~a" (name keymap))))

(declaim (ftype (function (string &rest keymap)
                          (values keymap &optional))
                make-keymap))
(defun make-keymap (name &rest parents)
  "Return a new `keymap'."
  ;; We use (values keymap &optional) type because of an SBCL limitation.
  ;; See http://www.sbcl.org/manual/#Implementation-Limitations.
  (the (values keymap &optional)
       (make-instance 'keymap
                      :name name
                      :parents parents)))
(defun keymap-p (object)
  "Return non-nil if OBJECT is a `keymap'."
  (typep object 'keymap))

(defun copy-keymap (keymap)
  (let ((new-keymap (apply #'make-keymap (name keymap)
                           (parents keymap))))
    (setf (bound-type new-keymap)
          (bound-type keymap))
    (setf (entries new-keymap)
          (entries keymap))
    new-keymap))

;; "keyspec" is better then "keydesc" since the strings are well specified,
;; while a "description" could be anything.
(deftype keyspecs-type ()
  `(and string (satisfies keyspecs->keys)))

(declaim (ftype (function (keymap (or keyspecs-type list) (or keymap t)
                                  &rest (or keyspecs-type list keymap t)))
                define-key))
(defun define-key (keymap keyspecs bound-value &rest more-keyspecs-value-pairs)
  "Bind KEYS to BOUND-VALUE in KEYMAP.
Return KEYMAP.

KEYSPECS is either a `keyspecs-type', or a list of arguments passed to invocations
of `make-key's, or (:REMAP OTHER-VALUE &OPTIONAL OTHER-KEYMAP).

BOUND-VALUE can be anything.  If NIL, the binding is removed.

With (:REMAP OTHER-VALUE &OPTIONAL OTHER-KEYMAP), define-key maps the binding of
OTHER-VALUE in OTHER-KEYMAP (default to KEYMAP) to BOUND-VALUE.
In other words, it remaps OTHER-VALUE to VALUE.

Examples:

  (define-key foo-map \"C-x C-f\" 'open-file)

  (define-key foo-map
              \"C-x C-f\" 'open-file
              \"C-h k\" 'describe-key)

\"C-M-1 x\" on a QWERTY:

  (define-key foo-map '((:code 10 :modifiers (\"C\" \"M\") (:value \"x\"))) 'open-file)

or the shorter:

  (define-key foo-map \"C-M-#1\" 'open-file)

Remapping keys:

  (define-key foo-map '(:remap foo-a) 'foo-value)
  (define-key foo-map `(:remap foo-a ,bar-map) 'new-value)"
  (let ((keyspecs-value-pairs (append (list keyspecs bound-value) more-keyspecs-value-pairs)))
    (alex:doplist (keyspecs bound-value keyspecs-value-pairs)
      (unless (typep bound-value (bound-type keymap))
        ;; (error 'type-error "Bound value ~a not of type ~a"
        ;;        bound-value (bound-type keymap))
        (assert (typep bound-value (bound-type keymap)) (bound-value)
                'type-error :datum bound-value :expected-type (bound-type keymap)))
      (let ((keys (cond
                    ((and (listp keyspecs) (eq (first keyspecs) :remap))
                     (let ((other-value (second keyspecs))
                           (other-keymap (or (third keyspecs) keymap)))
                       (keyspecs->keys (first (binding-keys other-value other-keymap))
                                       t)))
                    ((listp keyspecs)
                     (mapcar (alex:curry #'apply #'make-key) keyspecs))
                    (t (keyspecs->keys keyspecs t)))))
        (bind-key keymap keys bound-value)))
    keymap))

(define-compiler-macro define-key (&whole form
                                          keymap
                                          keyspecs bound-value
                                          &rest more-keyspecs-value-pairs)
  "We need a compiler macro to check that bindings are valid at compile time.
This is because most Common Lisp implementations are not capable of checking
types that use `satisfies' for non-top-level symbols.
We can verify this with:

\(compile nil (lambda () (nkeymaps::define-key keymap \"C-x C-f\" 'open-file)))"
  (declare (ignore keymap))
  (let ((keyspecs-value-pairs (append (list keyspecs bound-value) more-keyspecs-value-pairs)))
    (alex:doplist (keyspecs _ keyspecs-value-pairs)
      (when (stringp keyspecs)
        (check-type keyspecs (or keyspecs-type list))))
    form))

(defun legal-modifiers-p (key keymap)
  "Whether KEY's modifiers are allowed in KEYMAP."
  (fset:empty? (fset:set-difference (key-modifiers key)
                                    (modifiers keymap))))

(declaim (ftype (function (keymap (list-of key) (or keymap t)) (values keymap &optional))
                bind-key))
(defun bind-key (keymap keys bound-value)
  "Recursively bind the KEYS to keymaps starting from KEYMAP.
The last key is bound to BOUND-VALUE.
If BOUND-VALUE is nil, the key is unbound.
If KEYS has modifiers that are not allowed in KEYMAP, do nothing.

Return KEYMAP."
  (cond
    ((not (legal-modifiers-p (first keys) keymap))
     (error 'bad-modifier
            :message (format nil "Keymap ~a only accepts modifiers ~a, got ~a"
                             keymap
                             (modifiers keymap)
                             (key-modifiers (first keys)))))
    ((= (length keys) 1)
     (when (fset:@ (entries keymap) (first keys))
       ;; TODO: Notify caller properly?
       (warn 'override-existing-binding
             :existing-binding-value (fset:@ (entries keymap) (first keys))))
     (if bound-value
         (setf (fset:@ (entries keymap) (first keys)) bound-value)
         (setf (entries keymap) (fset:less (entries keymap) (first keys)))))
    (t
     (let ((submap (fset:@ (entries keymap) (first keys))))
       (when (and (not (keymap-p submap))
                  bound-value)
         (setf submap (make-keymap "anonymous"))
         (setf (fset:@ (entries keymap) (first keys)) submap))
       (bind-key submap (rest keys) bound-value)
       (unless bound-value
         (when (fset:equal? (fset:empty-map) (entries submap))
           (setf (entries keymap) (fset:less (entries keymap) (first keys))))))))
  keymap)

(declaim (ftype (function (keymap
                           (list-of key)
                           (list-of keymap))
                          (or keymap t))
                lookup-keys-in-keymap))
(defun lookup-keys-in-keymap (keymap keys visited)
  "Return bound value or keymap for KEYS.
Return nil when KEYS is not found in KEYMAP.
VISITED is used to detect cycles."
  (when keys
    (if (legal-modifiers-p (first keys) keymap)
        (let ((hit (fset:@ (entries keymap) (first keys))))
          (when hit
            (cond ((and (keymap-p hit) (rest keys))
                   (lookup-key* hit (rest keys) visited))
                  ;; Skip when a binding is found instead of a prefix keymap.
                  ((and (not (keymap-p hit)) (rest keys))
                   nil)
                  (t hit))))
        (error 'bad-modifier
               :message (format nil "Keymap ~a only accepts modifiers ~a, got ~a"
                                keymap
                                (modifiers keymap)
                                (key-modifiers (first keys)))))))

(defun keymap-tree->list (keymaps visited)
  "Flatten the KEYMAPS into a list.
It traverses KEYMAPS first, then traverses the each keymap `parent' layer after layer.

Example:
- keymap1 has parents (k1a k1b)
- k1a has parents (k1ap)
- keymap2 has parents (k2a)

Return (keymap1 keymap2 k1a k1b k2a k1ap)."
  (when keymaps
    (append keymaps
            (keymap-tree->list
             (delete-if (lambda (keymap)
                          (when (find keymap visited)
                            (warn 'cycle :keymap keymap)
                            t))
                        (alex:mappend #'parents keymaps))
             (append keymaps visited)))))

(declaim (ftype (function ((or keymap (list-of keymap))
                           (list-of key)
                           (list-of keymap))
                          (values (or keymap t) (or keymap null)))
                lookup-key*))
(defun lookup-key* (keymap-or-keymaps keys visited)
  "Internal function, see `lookup-key' for the user-facing function.
VISITED is used to detect cycles.
As a second value, return the matching keymap."
  (let* ((matching-keymap nil)
         (result
          (some (lambda (keymap)
                  (if (find keymap visited)
                      (warn 'cycle :keymap keymap)
                      (let ((hit (lookup-keys-in-keymap
                                  keymap
                                  keys
                                  (cons keymap visited))))
                        (when hit
                          (setf matching-keymap keymap)
                          hit))))
                (keymap-tree->list (uiop:ensure-list keymap-or-keymaps) '()))))
    (values result matching-keymap)))

(declaim (ftype (function ((or (list-of key) keyspecs-type)
                           (or keymap (list-of keymap)))
                          (values (or keymap t) (or keymap null) (list-of key)))
                lookup-key))
(defun lookup-key (keys-or-keyspecs keymap-or-keymaps)
  ;; We name this user-facing function using the singular form to be consistent
  ;; with `define-key'.
  "Return the value bound to KEYS-OR-KEYSPECS in KEYMAP-OR-KEYMAPS.
As a second value, return the matching keymap.
As a third value, return the possibly translated KEYS.

Return NIL if no value is found.

The successive keymaps from KEYMAP-OR-KEYMAPS (if a list) are looked up one
after the other.
If no binding is found, the direct parents are looked up in the same order.
And so on if the binding is still not found."
  (let* ((keys (if (stringp keys-or-keyspecs)
                   (keyspecs->keys keys-or-keyspecs t)
                   keys-or-keyspecs))
         (matching-keymap nil)
         (matching-key nil)
         (result
           (some (lambda (keys)
                   (multiple-value-bind (hit hit-keymap)
                       (lookup-key* keymap-or-keymaps keys '())
                     (when hit
                       (setf matching-keymap hit-keymap)
                       (setf matching-key keys)
                       hit)))
                 (cons keys (funcall (or *translator*
                                         (constantly nil))
                                     keys)))))
    (values result matching-keymap matching-key)))

(defparameter *print-shortcut* t
  "Whether to print the short form of the modifiers.")

(defun string-join (strings separator &key end)
  "Adapted from `serapeum:string-join'."
  (with-output-to-string (s)
    (if strings
        (progn
          (write-string (string (first strings)) s)
          (dolist (string (cdr strings))
            (write-string (string separator) s)
            (write-string (string string) s))
          (when end
            (write-string (string separator) s)))
        (make-string 0))))

(declaim (ftype (function (key) keyspecs-type) key->keyspec))
(defun key->keyspec (key)
  "Return the keyspec of KEY.
If the key has a code, return it prefixed with '#'.
The status is not encoded in the keyspec, but this may change in the future."
  (let ((value (if (zerop (key-code key))
                   (key-value key)
                   (format nil "#~a" (key-code key))))
        (modifiers (fset:reduce (lambda (&rest mods) (string-join mods "-"))
                                (key-modifiers key)
                                :key (if *print-shortcut*
                                         #'modifier-shortcut
                                         #'modifier-string))))
    (the (values keyspecs-type &optional)
         (uiop:strcat (if (uiop:emptyp modifiers) "" (uiop:strcat modifiers "-"))
                      value))))

(declaim (ftype (function ((list-of key)) keyspecs-type) keys->keyspecs))
(defun keys->keyspecs (keys)
  "Return the keyspecs (a list of `keyspec') for KEYS.
See `key->keyspec' for the details."
  (the (values keyspecs-type &optional)
       (string-join (mapcar #'key->keyspec keys) " ")))

(declaim (ftype (function (keymap &optional (list-of keymap)) fset:map) keymap->map*))
(defun keymap->map* (keymap &optional visited)
  "Return a map of (KEYSPEC SYM) from KEYMAP."
  (flet ((fold-keymap (result key sym)
           (let ((keyspec (key->keyspec key)))
             (if (keymap-p sym)
                 (cond
                   ((find sym visited)
                    (warn 'cycle :keymap keymap)
                    result)
                   (t
                    (fset:map-union result
                                    (fset:image (lambda (subkey subsym)
                                                  (values (format nil "~a ~a" keyspec subkey)
                                                          subsym))
                                                (keymap->map* sym (cons sym visited))))))
                 (fset:with result keyspec sym)))))
    (the (values fset:map &optional)
         (fset:reduce #'fold-keymap (entries keymap)
                      :initial-value (fset:empty-map)))))

(declaim (ftype (function (keymap &rest keymap) hash-table) keymap->map))
(defun keymap->map (keymap &rest more-keymaps)
  "Return a hash-table of (KEYSPEC BOUND-VALUE) from KEYMAP.
Parent bindings are not listed; see `keymap-with-parents->map' instead.
This is convenient if the caller wants to list all the bindings.
When multiple keymaps are provided, return the union of the `fset:map' of each arguments.
Keymaps are ordered by precedence, highest precedence comes first."
  (let ((keymaps (reverse (cons keymap more-keymaps))))
    (the (values hash-table &optional)
         (fset:convert 'hash-table
                       (reduce #'fset:map-union
                               (mapcar #'keymap->map* keymaps))))))

(defun keymap-with-parents (keymap)
  "Return the list of keymap and all its parents."
  (labels ((list-keymaps (keymap visited)
             (cond
               ((null keymap)
                '())
               ((find keymap visited)
                (warn 'cycle  :keymap keymap)
                '())
               (t
                (cons keymap
                      (alex:mappend (alex:rcurry #'list-keymaps (cons keymap visited))
                                    (parents keymap)))))))
    (list-keymaps keymap '())))

(declaim (ftype (function (keymap) hash-table) keymap-with-parents->map))
(defun keymap-with-parents->map (keymap)
  "List bindings in KEYMAP and all its parents.
See `keymap->map'."
  (apply #'keymap->map (keymap-with-parents keymap)))

(declaim (ftype (function (keymap &rest keymap) (or keymap null)) compose))
(defun compose (keymap &rest more-keymaps)
  "Return a new keymap that's the composition of all given KEYMAPS.
KEYMAPS are composed by order of precedence, first keymap being the one with
highest precedence."
  (flet ((stable-union (list1 list2)
           (delete-duplicates (append list1 list2)
                              :from-end t)))
    (let ((keymaps (cons keymap more-keymaps)))
      (cond
        ((uiop:emptyp keymaps)
         nil)
        ((= 1 (length keymaps))
         ;; Copy the map, don't just refer to it or all modifying the source
         ;; will alter the target.
         (let ((keymap (apply #'make-keymap (name (first keymaps)) (parents (first keymaps)))))
           (setf (entries keymap) (fset:convert 'fset:map (entries (first keymaps))))
           keymap))
        (t
         (let* ((keymap1 (first keymaps))
                (keymap2 (second keymaps))
                (merge (make-keymap (format nil "~a+~a" (name keymap1) (name keymap2)))))
           (setf (parents merge) (stable-union (parents keymap1) (parents keymap2)))
           (setf (entries merge) (fset:map-union (entries keymap2) (entries keymap1)))
           (apply #'compose merge (rest (rest keymaps)))))))))

(declaim (ftype (function (t keymap &key (:test function)) (list-of string)) binding-keys*))
(defun binding-keys* (binding keymap &key (test #'eql))
  "Return a the list of `keyspec's bound to BINDING in KEYMAP.
The list is sorted alphabetically to ensure reproducible results.
Comparison against BINDING is done with TEST."
  ;; This code is a bit redundant with `keymap->map*' but it's necessary to
  ;; avoid calling key->keyspec thousands of times.
  (labels ((scan-keymap (keymap visited)
             (let ((result '()))
               (fset:do-map (key sym (entries keymap))
                 (cond
                   ((funcall test binding sym)
                    (push (list key) result))
                   ((and (keymap-p sym) (find sym visited))
                    (warn 'cycle :keymap keymap))
                   ((keymap-p sym)
                    (dolist (hit (scan-keymap sym (cons keymap visited)))
                      (push (cons key hit) result)))))
               result)))
    (sort (mapcar #'keys->keyspecs (scan-keymap keymap '())) #'string<)))

(declaim (ftype (function (t (or keymap (list-of keymap)) &key (:test function))
                          (values list list))
                binding-keys))
(defun binding-keys (bound-value keymap-or-keymaps &key (test #'eql))
  "Return the list of `keyspec's bound to BINDING in KEYMAP-OR-KEYMAPS.

The order of the result is dictated by the order of KEYMAP-OR-KEYMAPS.

As a second value, return an alist of (keyspec keymap) for all the
`keyspec's bound to BINDING in KEYMAP.

Comparison against BINDING is done with TEST.

Duplicates and shadowed bindings are removed.

For instance, to list all keymaps that have a binding, call:

  (mapcar #'second (nth-value 1 (binding-keys ...)))"
  (let ((alist (alex:mappend (lambda (keymap)
                               (let ((hit (binding-keys* bound-value keymap :test test)))
                                 (when hit
                                   (mapcar (alex:rcurry #'list keymap) hit))))
                             (delete-duplicates
                              (alex:mappend #'keymap-with-parents
                                            (uiop:ensure-list keymap-or-keymaps))))))
    (setf alist (delete-duplicates alist :key #'first :test #'string= :from-end t))
    (setf alist (delete-if (lambda (keyspec)
                             (not
                              ;; TODO: Which comparison operator should we use?
                              (eq bound-value (lookup-key keyspec keymap-or-keymaps))))
                           alist :key #'first))
    (values
     (mapcar #'first alist)
     alist)))

(defun pretty-binding-keys (bound-value keymap-or-keymaps &key (test #'eql) (print-style "cua"))
  "Print the binding keys in a pretty manner.

This violates the keyspec <-> key functional relationship, and should not be
used for anything but rendering to the user."
  (cond ((equal print-style "cua")
         (let ((*print-shortcut* t))
           (mapcar
            (lambda (i)
              (str:replace-all
               "M-" #-(or darwin win32) "Alt+" #+darwin "Option+" #+win32 "Alt+"
               (str:replace-all
                "S-" #-(or darwin win32) "Super+" #+darwin "Command+" #+win32 "Win+"
                (str:replace-all
                 "s-" #-(or darwin win32) "Shift+" #+darwin "Shift+" #+win32 "Shift+"
                 (str:replace-all
                  "C-" #-(or darwin win32) "Ctrl+" #+darwin "Ctrl+" #+win32 "Ctrl+"
                  i)))))
            (binding-keys bound-value keymap-or-keymaps :test test))))
        (t
         (binding-keys bound-value keymap-or-keymaps :test test))))
