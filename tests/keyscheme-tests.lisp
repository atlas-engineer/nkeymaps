;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nkeymaps/tests)

(define-test make-keyscheme ()
  "Make keyscheme."
  (let* ((keyscheme-map (nkeymaps:define-keyscheme-map "test" ()
                          nkeymaps:cua '("C-c" copy
                                         "C-v" paste)))
         (keymap (nkeymaps:make-keymap "test-cua-map")))
    (nkeymaps:define-key keymap "C-c" 'copy)
    (nkeymaps:define-key keymap "C-v" 'paste)
    (assert-equality #'fset:equal?
                     (fset:convert 'fset:map (nkeymaps:keymap->map keymap))
                     (fset:convert 'fset:map (nkeymaps:keymap->map (gethash nkeymaps:cua keyscheme-map))))
    (assert-equal (nkeymaps:name keymap)
                  (nkeymaps:name (gethash nkeymaps:cua keyscheme-map)))))

(define-test make-keyscheme-map ()
  "Make keyscheme-map with `list'."
  (let* ((keyscheme-map (nkeymaps:define-keyscheme-map "test" ()
                          nkeymaps:cua (list "C-c" 'copy
                                             "C-v" 'paste)))
         (keymap (nkeymaps:make-keymap "test-cua-map")))
    (nkeymaps:define-key keymap
      "C-c" 'copy
      "C-v" 'paste)
    (assert-equality #'fset:equal?
                     (fset:convert 'fset:map (nkeymaps:keymap->map keymap))
                     (fset:convert 'fset:map (nkeymaps:keymap->map (gethash nkeymaps:cua keyscheme-map))))))

(define-test make-schsme-with-multiple-names ()
  "Make scheme with multiple names"
  (let* ((keyscheme-map (nkeymaps:define-keyscheme-map "test" ()
                          nkeymaps:cua (list "C-c" 'copy
                                             "C-v" 'paste)
                          nkeymaps:emacs (list "M-w" 'copy
                                               "M-y" 'paste)))
         (cua-keymap (nkeymaps:make-keymap "test-cua-map"))
         (emacs-keymap (nkeymaps:make-keymap "test-emacs-map")))
    (nkeymaps:define-key cua-keymap
      "C-c" 'copy
      "C-v" 'paste)
    (nkeymaps:define-key emacs-keymap
      "M-w" 'copy
      "M-y" 'paste)
    (assert-equality #'fset:equal?
                     (fset:convert 'fset:map (nkeymaps:keymap->map cua-keymap))
                     (fset:convert 'fset:map (nkeymaps:keymap->map (gethash nkeymaps:cua keyscheme-map))))
    (assert-equality #'fset:equal?
                     (fset:convert 'fset:map (nkeymaps:keymap->map emacs-keymap))
                     (fset:convert 'fset:map (nkeymaps:keymap->map (gethash nkeymaps:emacs keyscheme-map))))))

(define-test inheritance ()
  "Test inheritance."
  (let* ((keyscheme-map (nkeymaps:define-keyscheme-map "test" ()
                          nkeymaps:default (list "C-c" 'copy
                                                 "C-v" 'paste)
                          nkeymaps:emacs (list "M-w" 'copy
                                               "M-y" 'paste)))
         (default-keymap (nkeymaps:make-keymap "test-deefault-map"))
         (emacs-keymap (nkeymaps:make-keymap "test-emacs-map")))
    (nkeymaps:define-key default-keymap
      "C-c" 'copy
      "C-v" 'paste)
    (nkeymaps:define-key emacs-keymap
      "M-w" 'copy
      "M-y" 'paste)
    (assert-equal (nkeymaps:parents (gethash nkeymaps:emacs keyscheme-map))
                  (list (gethash nkeymaps:default keyscheme-map)))))

(define-test get-keymap ()
  "Get keymap."
  (let* ((keyscheme-map (nkeymaps:define-keyscheme-map "test" ()
                          nkeymaps:default (list "C-c" 'copy
                                             "C-v" 'paste)
                          nkeymaps:emacs (list "M-w" 'copy
                                               "M-y" 'paste))))
    (assert-true (nkeymaps:get-keymap nkeymaps:emacs keyscheme-map))
    (assert-true (nkeymaps:get-keymap nkeymaps:default keyscheme-map))
    (assert-false (equal (nkeymaps:get-keymap nkeymaps:default keyscheme-map)
                         (nkeymaps:get-keymap nkeymaps:emacs keyscheme-map)))
    (assert-equal (nkeymaps:get-keymap nkeymaps:default keyscheme-map)
                  (nkeymaps:get-keymap nkeymaps:vi-normal keyscheme-map))))

(define-test prioritize-scheme-over-parent ()
  "Prioritize scheme over parent."
  (let* ((keyscheme-map1 (nkeymaps:define-keyscheme-map "test1" ()
                           nkeymaps:cua (list "C-c" 'do-not-hit-me)
                           nkeymaps:emacs (list "M-w" 'copy)))
         (keyscheme-map2 (nkeymaps:define-keyscheme-map "test2" ()
                           nkeymaps:emacs (list "C-c" 'hit-me))))
    (let ((keymaps (mapcar (lambda (scheme)
                             (nkeymaps:get-keymap nkeymaps:emacs scheme))
                           (list keyscheme-map1 keyscheme-map2))))
      (assert-eql 'hit-me
                  (nkeymaps:lookup-key "C-c" keymaps)))))

(define-test custom-modifiers ()
  "Define scheme with custom modifiers."
  (let* ((+custom+ (make-instance 'nkeymaps:keyscheme
                                  :name "custom"
                                  :modifiers (fset:set
                                              nkeymaps:+control+
                                              (nkeymaps:define-modifier
                                                  :string "duper"
                                                  :shortcut-emacs "D"))))
         (keyscheme-map (nkeymaps:define-keyscheme-map "test" ()
                          +custom+ (list
                                    "D-c" 'hit-me
                                    "C-c" 'hit-me-again))))

    (let ((keymap (nkeymaps:get-keymap +custom+ keyscheme-map)))
      (assert-eql 'hit-me
                  (nkeymaps:lookup-key "D-c" keymap))
      (assert-eql 'hit-me-again
                  (nkeymaps:lookup-key "C-c" keymap))
      (assert-error 'nkeymaps/conditions:bad-modifier
                    (nkeymaps:lookup-key "M-c" keymap)))))

(define-test imported-keyscheme-map ()
  "Define scheme with custom modifiers."
  (let* ((imported-map (nkeymaps:define-keyscheme-map "imported" ()
                         nkeymaps:default (list
                                           "M-c" 'hit-me
                                           "C-c" 'hit-me-again)))
         (new-map (nkeymaps:define-keyscheme-map "imported" `(:import ,imported-map)
                    nkeymaps:default (list
                                      "M-a" 'back
                                      "C-c" 'hit-me-differently))))

    (let ((new-keymap (nkeymaps:get-keymap nkeymaps:default new-map))
          (imported-keymap (nkeymaps:get-keymap nkeymaps:default imported-map)))
      (assert-eql 'hit-me (nkeymaps:lookup-key "M-c" new-keymap))
      (assert-eql 'hit-me-differently (nkeymaps:lookup-key "C-c" new-keymap))
      (assert-eql 'back (nkeymaps:lookup-key "M-a" new-keymap))

      (assert-eql 'hit-me (nkeymaps:lookup-key "M-c" imported-keymap))
      (assert-eql 'hit-me-again (nkeymaps:lookup-key "C-c" imported-keymap))
      (assert-false (nkeymaps:lookup-key "M-a" imported-keymap))

      (nkeymaps:define-key imported-keymap "M-c" 'do-not-forward-me)
      (assert-eql 'do-not-forward-me (nkeymaps:lookup-key "M-c" imported-keymap))
      (assert-eql 'hit-me (nkeymaps:lookup-key "M-c" new-keymap)))))

(define-test define-keyscheme-type-catching ()
  "Catch bad keyspecs."
  (let ((form '(lambda ()
                (nkeymaps:define-keyscheme-map "foo" ()
                    nkeymaps:cua (list "C-" 'copy)))))
    (assert-true (nth-value 2 (compile nil form)))
    (assert-warning 'warning (compile nil form))))
