;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nkeymaps/tests)

(defun empty-keymap (&rest parents)
  (apply #'nkeymaps:make-keymap "anonymous" parents))

(define-test make-key ()
  "Make key."
  (let* ((key (nkeymaps:make-key :code 38 :value "a" :modifiers '("C")))
         (mod (first (fset:convert 'list (nkeymaps:key-modifiers key)))))
    (assert-equal 38
                  (nkeymaps:key-code key))
    (assert-equal "a"
                  (nkeymaps:key-value key))
    (assert-equality #'nkeymaps:modifier= "C" mod)
    (assert-equality #'nkeymaps:modifier= "control" mod)
    (assert-equality #'nkeymaps:modifier= nkeymaps:+control+ mod)
    (assert-false (nkeymaps:modifier= "" mod))
    (assert-false (nkeymaps:modifier= "M" mod))
    (assert-false (nkeymaps:modifier= "meta" mod))))

(define-test make-bad-key ()
  "Make bad key."
  (assert-error 'type-error
                (nkeymaps:make-key :value "a" :status :dummy))
  (assert-error 'nkeymaps:bad-modifier
                (nkeymaps:make-key :value "a" :modifiers '("Z")))
  (assert-error 'nkeymaps:make-key-required-arg
                (nkeymaps:make-key :status :pressed)))

(define-test define-key-type-catching ()
  "Catch bad keyspecs."
  (let ((form '(lambda ()
                (let ((keymap (nkeymaps:make-keymap "anonymous")))
                  (nkeymaps:define-key keymap "z---" 'open-file)))))
    (assert-true (nth-value 2 (compile nil form)))
    (assert-warning 'warning (compile nil form))))

(define-test make-same-key ()
  "Make same key."
  (assert-equality #'nkeymaps:key=
                   (nkeymaps:make-key :value "a" :modifiers '("M" "C"))
                   (nkeymaps:make-key :value "a" :modifiers '("C" "M")))
  (assert-equality #'nkeymaps:key=
                   (nkeymaps:make-key :value "a" :modifiers '("control"))
                   (nkeymaps:make-key :value "a" :modifiers '("C"))))

(define-test make-key-with-duplicate-modifiers ()
  "Make key with duplicate modifiers (trigger warning)."
  (assert-equality #'nkeymaps:key=
                   (nkeymaps:make-key :value "a" :modifiers '("C"))
                   (nkeymaps:make-key :value "a" :modifiers '("C" "control"))))

(define-test make-different-key ()
  "Make different key."
  (assert-false (nkeymaps:key= (nkeymaps:make-key :value "a")
                               (nkeymaps:make-key :value "A"))))

(define-test keyspec->key ()
  "Keyspec->key."
  (assert-equality #'nkeymaps:key=
                   (nkeymaps:make-key :value "a")
                   (nkeymaps/core::keyspec->key "a"))
  (assert-equality #'nkeymaps:key=
                   (nkeymaps:make-key :value "a" :modifiers '("C"))
                   (nkeymaps/core::keyspec->key "C-a"))
  (assert-equality #'nkeymaps:key=
                   (nkeymaps:make-key :value "a" :modifiers '("C" "M"))
                   (nkeymaps/core::keyspec->key "C-M-a"))
  (assert-equality #'nkeymaps:key=
                   (nkeymaps:make-key :value "-" :modifiers '("C"))
                   (nkeymaps/core::keyspec->key "C--"))
  (assert-equality #'nkeymaps:key=
                   (nkeymaps:make-key :value "-" :modifiers '("C" "M"))
                   (nkeymaps/core::keyspec->key "C-M--"))
  (assert-equality #'nkeymaps:key=
                   (nkeymaps:make-key :value "#" :modifiers '("C"))
                   (nkeymaps/core::keyspec->key "C-#"))
  (assert-equality #'nkeymaps:key=
                   (nkeymaps:make-key :value "#")
                   (nkeymaps/core::keyspec->key "#"))
  (assert-equality #'nkeymaps:key=
                   (nkeymaps:make-key :value "-")
                   (nkeymaps/core::keyspec->key "-"))
  (assert-equality #'nkeymaps:key=
                   (nkeymaps:make-key :code 10 :modifiers '("C"))
                   (nkeymaps/core::keyspec->key "C-#10"))
  (assert-error 'nkeymaps:empty-keyspec
                (nkeymaps/core::keyspec->key "" t))
  (assert-error 'nkeymaps:empty-value
                (nkeymaps/core::keyspec->key "C-" t))
  (assert-error 'nkeymaps:empty-modifiers
                (nkeymaps/core::keyspec->key "C---" t)))

(defun binding= (keys1 keys2)
  (not (position nil (mapcar #'nkeymaps:key= keys1 keys2))))

(define-test keyspecs->keys ()
  "Keyspecs->keys."
  (assert-equality #'binding=
                   (list (nkeymaps:make-key :value "x" :modifiers '("C"))
                         (nkeymaps:make-key :value "f" :modifiers '("C")))
                   (nkeymaps/core::keyspecs->keys "C-x C-f"))
  (assert-equality #'binding=
                   (list (nkeymaps:make-key :value "x" :modifiers '("C"))
                         (nkeymaps:make-key :value "f" :modifiers '("C")))
                   (nkeymaps/core::keyspecs->keys "  C-x   C-f  ")))

(define-test define-key-lookup-key ()
  "define-key & lookup-key."
  (let ((keymap (empty-keymap)))
    (nkeymaps:define-key keymap "C-x" 'foo)
    (assert-eql 'foo
                (nkeymaps:lookup-key "C-x" keymap))
    (nkeymaps:define-key keymap "C-x" 'foo2)
    (assert-eql 'foo2
                (nkeymaps:lookup-key "C-x" keymap))
    (nkeymaps:define-key keymap "C-c C-f" 'bar)
    (assert-eql 'bar
                (nkeymaps:lookup-key "C-c C-f" keymap))
    (nkeymaps:define-key keymap "C-c C-h" 'bar2)
    (assert-eql 'bar2
                (nkeymaps:lookup-key "C-c C-h" keymap))))

(define-test define-key-type-error ()
  "define-key type error."
  (let ((keymap (empty-keymap)))
    (setf (nkeymaps:bound-type keymap) '(or nkeymaps::keymap function))
    (assert-equal (nkeymaps:define-key keymap "C-x" #'format)
                  keymap)
    (assert-error 'type-error
                  (nkeymaps:define-key keymap "C-c" 'append))))

(define-test define-key-multiple-bindings ()
  "define-key & multiple bindings."
  (let ((keymap (empty-keymap)))
    (nkeymaps:define-key keymap
      "C-x" 'foo
      "C-c" 'bar)
    (assert-eql 'foo
                (nkeymaps:lookup-key "C-x" keymap))
    (assert-eql 'bar
                (nkeymaps:lookup-key "C-c" keymap))))

(define-test define-key-lookup-key-parents ()
  "define-key & lookup-key with parents."
  (let* ((parent1 (empty-keymap))
         (parent2 (empty-keymap))
         (keymap (empty-keymap parent1 parent2)))
    (nkeymaps:define-key parent1 "x" 'parent1-x)
    (nkeymaps:define-key parent1 "a" 'parent1-a)
    (nkeymaps:define-key parent2 "x" 'parent2-x)
    (nkeymaps:define-key parent2 "b" 'parent2-b)
    (assert-eql 'parent1-x
                (nkeymaps:lookup-key "x" keymap))
    (assert-eql 'parent1-a
                (nkeymaps:lookup-key "a" keymap))
    (assert-eql 'parent2-b
                (nkeymaps:lookup-key "b" keymap))))

(define-test define-key-lookup-key-prefix-keymap ()
  "define-key & lookup-key with prefix keymap."
  (let ((keymap (empty-keymap))
        (prefix (empty-keymap)))
    (nkeymaps:define-key keymap "C-c" prefix)
    (nkeymaps:define-key prefix "x" 'prefix-sym)
    (assert-eql 'prefix-sym
                (nkeymaps:lookup-key "C-c x" keymap))))

(define-test define-key-lookup-key-cycle ()
  "define-key & lookup-key with cycle."
  (let ((keymap (empty-keymap))
        (parent1 (empty-keymap))
        (parent2 (empty-keymap)))
    (push parent1 (nkeymaps:parents keymap))
    (push parent2 (nkeymaps:parents parent1))
    (push keymap (nkeymaps:parents parent2))
    (assert-warning 'nkeymaps:cycle
                    (nkeymaps:lookup-key "x" keymap))))

(define-test translator ()
  "Translator."
  (let ((keymap (empty-keymap)))
    (nkeymaps:define-key keymap "A b" 'foo)
    (assert-eql 'foo
                (nkeymaps:lookup-key "shift-a shift-B" keymap))
    (nkeymaps:define-key keymap "c" 'bar)
    (assert-eql 'bar
                (nkeymaps:lookup-key "shift-c" keymap))
    (nkeymaps:define-key keymap "C-x c" 'baz)
    (assert-eql 'baz
                (nkeymaps:lookup-key "C-x C-c" keymap))
    (nkeymaps:define-key keymap "C-c F" 'qux)
    (assert-eql 'qux
                (nkeymaps:lookup-key "C-shift-c C-shift-F" keymap))
    (nkeymaps:define-key keymap "1" 'quux)
    (assert-eql 'quux
                (nkeymaps:lookup-key "shift-1" keymap))
    (nkeymaps:define-key keymap "return" 'ret)
    (assert-eql 'ret
                (nkeymaps:lookup-key "shift-return" keymap))))

(define-test translator-priority ()
  "Translator: Ensure other keymaps have priority over translations."
  (let ((keymap (empty-keymap))
        (keymap2 (empty-keymap)))
    (nkeymaps:define-key keymap "g g" 'prefix-g)
    (nkeymaps:define-key keymap2 "G" 'up-g)
    (assert-eql 'up-g
                (nkeymaps:lookup-key "s-G" (list keymap keymap2)))))

(define-test keys->keyspecs ()
  "keys->keyspecs."
  (assert-equal "#10"
                (nkeymaps:keys->keyspecs (list (nkeymaps:make-key :code 10 :value "a"))))
  (assert-equal "a b"
                (nkeymaps:keys->keyspecs (list (nkeymaps:make-key :value "a")
                                               (nkeymaps:make-key :value "b"))))
  (assert-equal "C-a"
                (nkeymaps:keys->keyspecs (list (nkeymaps:make-key :value "a" :modifiers '("C")))))
  (assert-equal "C-M-a"
                (nkeymaps:keys->keyspecs (list (nkeymaps:make-key :value "a" :modifiers '("C" "M")))))
  (assert-equal "C-M-a"
                (nkeymaps:keys->keyspecs (list (nkeymaps:make-key :value "a" :modifiers '("M" "C")))))
  (assert-equal "C-M-a H-S-x"
                (nkeymaps:keys->keyspecs (list (nkeymaps:make-key :value "a" :modifiers '("C" "M"))
                                               (nkeymaps:make-key :value "x" :modifiers '("super" "hyper")))))
  (let ((nkeymaps:*print-shortcut* nil))
    (assert-equal "control-a"
                  (nkeymaps:keys->keyspecs (list (nkeymaps:make-key :value "a" :modifiers '("C")))))))

(define-test keymap->map ()
  "keymap->map."
  (let ((keymap (empty-keymap))
        (keymap2 (empty-keymap)))
    (nkeymaps:define-key keymap "a" 'foo-a)
    (nkeymaps:define-key keymap "b" 'foo-b)
    (nkeymaps:define-key keymap "k" keymap2)
    (nkeymaps:define-key keymap2 "a" 'bar-a)
    (nkeymaps:define-key keymap2 "c" 'bar-c)
    (assert-equality #'fset:equal?
                     (fset:map ("a" 'foo-a)
                               ("b" 'foo-b)
                               ("k a" 'bar-a)
                               ("k c" 'bar-c))
                     (fset:convert 'fset:map (nkeymaps:keymap->map keymap)))
    (assert-equality #'fset:equal?
                     (fset:map ("a" 'foo-a)
                               ("b" 'foo-b)
                               ("c" 'bar-c)
                               ("k a" 'bar-a)
                               ("k c" 'bar-c))
                     (fset:convert 'fset:map (nkeymaps:keymap->map keymap keymap2)))
    (assert-equality #'fset:equal?
                     (fset:map ("a" 'bar-a)
                               ("b" 'foo-b)
                               ("c" 'bar-c)
                               ("k a" 'bar-a)
                               ("k c" 'bar-c))
                     (fset:convert 'fset:map (nkeymaps:keymap->map keymap2 keymap)))))

(define-test keymap->map-cycles ()      ; TODO: Can we check warnings?
  "keymap->map with cycles."
  (let ((keymap (empty-keymap))
        (keymap2 (empty-keymap)))
    (nkeymaps:define-key keymap "k" keymap2)
    (nkeymaps:define-key keymap2 "a" keymap)
    (assert-equality #'fset:equal?
                     (fset:empty-map)
                     (fset:convert 'fset:map (nkeymaps:keymap->map keymap))))
  (let ((keymap (empty-keymap))
        (keymap2 (empty-keymap))
        (keymap3 (empty-keymap)))
    (nkeymaps:define-key keymap "k" keymap2)
    (nkeymaps:define-key keymap2 "a" keymap3)
    (nkeymaps:define-key keymap3 "b" keymap)
    (assert-equality #'fset:equal?
                     (fset:empty-map)
                     (fset:convert 'fset:map (nkeymaps:keymap->map keymap)))))

(define-test keymap-with-parents->map ()
  "keymap-with-parents->map."
  (let* ((grand-parent (empty-keymap))
         (parent1 (empty-keymap))
         (parent2 (empty-keymap grand-parent))
         (keymap (empty-keymap parent1 parent2)))
    (nkeymaps:define-key keymap "a" 'foo-a)
    (nkeymaps:define-key parent1 "b" 'bar-b)
    (nkeymaps:define-key parent2 "c" 'qux-c)
    (nkeymaps:define-key grand-parent "d" 'quux-d)
    (assert-equality #'fset:equal?
                     (fset:map ("a" 'foo-a)
                               ("b" 'bar-b)
                               ("c" 'qux-c)
                               ("d" 'quux-d))
                     (fset:convert 'fset:map (nkeymaps:keymap-with-parents->map keymap)))
    (nkeymaps:define-key parent2 "d" 'qux-d)
    (assert-equality #'fset:equal?
                     (fset:map ("a" 'foo-a)
                               ("b" 'bar-b)
                               ("c" 'qux-c)
                               ("d" 'qux-d))
                     (fset:convert 'fset:map (nkeymaps:keymap-with-parents->map keymap)))
    (nkeymaps:define-key parent1 "c" 'bar-c)
    (assert-equality #'fset:equal?
                     (fset:map ("a" 'foo-a)
                               ("b" 'bar-b)
                               ("c" 'bar-c)
                               ("d" 'qux-d))
                     (fset:convert 'fset:map (nkeymaps:keymap-with-parents->map keymap)))
    (nkeymaps:define-key parent1 "b" 'foo-b)
    (assert-equality #'fset:equal?
                     (fset:map ("a" 'foo-a)
                               ("b" 'foo-b)
                               ("c" 'bar-c)
                               ("d" 'qux-d))
                     (fset:convert 'fset:map (nkeymaps:keymap-with-parents->map keymap)))))

(define-test keymap-with-parents->map-cycles () ; TODO: Can we check warnings?
  "keymap-with-parents->map with cycles."
  (let ((keymap1 (empty-keymap))
        (keymap2 (empty-keymap)))
    (push keymap1 (nkeymaps:parents keymap2))
    (push keymap2 (nkeymaps:parents keymap1))
    (assert-equality #'fset:equal?
                     (fset:empty-map)
                     (fset:convert 'fset:map (nkeymaps:keymap-with-parents->map keymap1))))
  (let ((keymap1 (empty-keymap))
        (keymap2 (empty-keymap))
        (keymap3 (empty-keymap)))
    (push keymap1 (nkeymaps:parents keymap2))
    (push keymap2 (nkeymaps:parents keymap3))
    (push keymap3 (nkeymaps:parents keymap1))
    (assert-equality #'fset:equal?
                     (fset:empty-map)
                     (fset:convert 'fset:map (nkeymaps:keymap-with-parents->map keymap1)))))

(define-test compose-keymaps ()
  "compose-keymaps."
  (let* ((parent1 (empty-keymap))
         (keymap1 (nkeymaps:make-keymap "1" parent1))
         (parent2 (empty-keymap))
         (keymap2 (nkeymaps:make-keymap "2" parent2))
         (keymap3 (empty-keymap)))
    (nkeymaps:define-key keymap1 "a" 'foo-a)
    (nkeymaps:define-key keymap1 "b" 'foo-b)
    (nkeymaps:define-key keymap2 "b" 'bar-b)
    (nkeymaps:define-key keymap2 "c" 'bar-c)
    (nkeymaps:define-key keymap3 "c" 'qux-c)
    (nkeymaps:define-key keymap3 "d" 'qux-d)
    (let ((composition (nkeymaps:compose keymap1 keymap2 keymap3)))
      (assert-equal "1+2+anonymous"
                    (nkeymaps:name composition))
      (assert-equality #'fset:equal?
                       (fset:map
                        ("a" 'foo-a)
                        ("b" 'foo-b)
                        ("c" 'bar-c)
                        ("d" 'qux-d))
                       (fset:convert 'fset:map (nkeymaps:keymap->map composition)))
      (assert-equal (list parent1 parent2)
                    (nkeymaps:parents composition)))))

(define-test compose ()
  "compose: Altering source does not impact composition."
  (let* ((keymap1 (nkeymaps:make-keymap "1"))
         (keymap2 nil))
    (nkeymaps:define-key keymap1 "a" 'foo-a)
    (setf keymap2 (nkeymaps:compose keymap1))
    (nkeymaps:define-key keymap1 "a" 'foo-a-alt)
    (nkeymaps:define-key keymap1 "b" 'new-foo)
    (assert-equality #'fset:equal?
                     (fset:map
                      ("a" 'foo-a))
                     (fset:convert 'fset:map (nkeymaps:keymap->map keymap2)))))

(define-test binding-keys ()
  "binding-keys."
  (let* ((keymap1 (empty-keymap))
         (keymap2 (empty-keymap))
         (keymap3 (empty-keymap keymap1)))
    (nkeymaps:define-key keymap1 "a" 'foo-a)
    (nkeymaps:define-key keymap1 "b" 'foo-b)
    (nkeymaps:define-key keymap1 "C-c a" 'foo-a)
    (assert-equal (values '("C-c a" "a")
                          `(("C-c a" ,keymap1)
                            ("a" ,keymap1)))
                  (nkeymaps:binding-keys 'foo-a keymap1))
    (assert-equal (values '("b")
                          `(("b" ,keymap1)))
                  (nkeymaps:binding-keys 'foo-b keymap1))
    (assert-false (nkeymaps:binding-keys 'missing keymap1))
    (nkeymaps:define-key keymap2 "a" 'foo-a)
    (nkeymaps:define-key keymap2 "c" 'foo-a)
    (assert-equal (values '("C-c a" "a" "c")
                          `(("C-c a" ,keymap1)
                            ("a" ,keymap1)
                            ("c" ,keymap2)))
                  (nkeymaps:binding-keys 'foo-a (list keymap1 keymap2)))

    ;; Ordering:
    (nkeymaps:define-key keymap1 "E" 'bar-e)
    (nkeymaps:define-key keymap1 "F" 'bar-f)
    (nkeymaps:define-key keymap2 "D" 'bar-e)
    (nkeymaps:define-key keymap2 "G" 'bar-f)
    (assert-equal (values '("E" "D")
                          `(("E" ,keymap1)
                            ("D" ,keymap2)))
                  (nkeymaps:binding-keys 'bar-e (list keymap1 keymap2)))
    (assert-equal (values '("D" "E")
                          `(("D" ,keymap2)
                            ("E" ,keymap1)))
                  (nkeymaps:binding-keys 'bar-e (list keymap2 keymap1)))
    (assert-equal (values '("F" "G")
                          `(("F" ,keymap1)
                            ("G" ,keymap2)))
                  (nkeymaps:binding-keys 'bar-f (list keymap1 keymap2)))
    (assert-equal (values '("G" "F")
                          `(("G" ,keymap2)
                            ("F" ,keymap1)))
                  (nkeymaps:binding-keys 'bar-f (list keymap2 keymap1)))

    ;; Inheritance:
    (assert-equal (values '("C-c a" "a")
                          `(("C-c a" ,keymap1)
                            ("a" ,keymap1)))
                  (nkeymaps:binding-keys 'foo-a keymap3))

    ;; Shadowing:
    (nkeymaps:define-key keymap3 "a" 'shadowed-a)
    (assert-equal (values '("C-c a")
                          `(("C-c a" ,keymap1)))
                  (nkeymaps:binding-keys 'foo-a keymap3))))

(define-test undefine ()
  "undefine."
  (let* ((keymap (empty-keymap)))
    (nkeymaps:define-key keymap "a" 'foo-a)
    (nkeymaps:define-key keymap "a" nil)
    (assert-equality #'fset:equal?
                     (fset:empty-map)
                     (nkeymaps::entries keymap))
    (nkeymaps:define-key keymap "C-c b" 'foo-b)
    (nkeymaps:define-key keymap "C-c b" nil)
    (assert-equality #'fset:equal?
                     (fset:empty-map)
                     (nkeymaps::entries keymap))))

(define-test remap ()
  "remap."
  (let* ((keymap (empty-keymap))
         (keymap2 (empty-keymap)))
    (nkeymaps:define-key keymap "a" 'foo-a)
    (nkeymaps:define-key keymap '(:remap foo-a) 'foo-b)
    (assert-eql 'foo-b
                (nkeymaps:lookup-key "a" keymap))
    (nkeymaps:define-key keymap2 "b" 'bar-1)
    (nkeymaps:define-key keymap `(:remap bar-1 ,keymap2) 'bar-2)
    (assert-eql 'bar-2
                (nkeymaps:lookup-key "b" keymap))))

(define-test retrieve-translated-key ()
  "retrieve translated key."
  (let* ((keymap (empty-keymap)))
    (nkeymaps:define-key keymap "a" 'foo-a)
    (multiple-value-bind (hit km key)
        (nkeymaps:lookup-key "s-A" keymap)
      (assert-eql 'foo-a hit)
      (assert-eql keymap km)
      (assert-equal "a" (nkeymaps:keys->keyspecs key)))))

(define-test do-not-shadow-prefix-keymap ()
  "Don't shadow a prefix keymap."
  (let* ((parent (empty-keymap))
         (keymap (empty-keymap parent)))
    (nkeymaps:define-key parent "C-x" 'parent-x)
    (nkeymaps:define-key keymap "C-x C-f" 'keymap-x)
    (assert-eql 'keymap-x
                (nkeymaps:lookup-key "C-x C-f" keymap))
    (assert-false (nkeymaps:lookup-key "C-x C-f" parent))
    (assert-eql 'parent-x
                (nkeymaps:lookup-key "C-x" parent))))
