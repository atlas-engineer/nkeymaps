;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; TODO: Switch to a better testing framework.

(in-package :nkeymaps/tests)

(prove:plan nil)

(prove:subtest "Make keyscheme"
  (let* ((keyscheme-map (nkeymaps:define-keyscheme-map "test"
                          nkeymaps/keyscheme:cua '("C-c" copy
                                                   "C-v" paste)))
         (keymap (nkeymaps:make-keymap "test-cua-map")))
    (nkeymaps:define-key keymap "C-c" 'copy)
    (nkeymaps:define-key keymap "C-v" 'paste)
    (prove:is (fset:convert 'fset:map (nkeymaps:keymap->map (gethash nkeymaps/keyscheme:cua keyscheme-map)))
              (fset:convert 'fset:map (nkeymaps:keymap->map keymap))
              :test #'fset:equal?)
    (prove:is (nkeymaps:name (gethash nkeymaps/keyscheme:cua keyscheme-map))
              (nkeymaps:name keymap))))

(prove:subtest "Make keyscheme-map with `list'"
  (let* ((keyscheme-map (nkeymaps:define-keyscheme-map "test"
                     nkeymaps/keyscheme:cua (list "C-c" 'copy
                                        "C-v" 'paste)))
         (keymap (nkeymaps:make-keymap "test-cua-map")))
    (nkeymaps:define-key keymap
      "C-c" 'copy
      "C-v" 'paste)
    (prove:is (fset:convert 'fset:map (nkeymaps:keymap->map (gethash nkeymaps/keyscheme:cua keyscheme-map)))
              (fset:convert 'fset:map (nkeymaps:keymap->map keymap))
              :test #'fset:equal?)))

(prove:subtest "Make scheme with multiple names"
  (let* ((keyscheme-map (nkeymaps:define-keyscheme-map "test"
                     nkeymaps/keyscheme:cua (list "C-c" 'copy
                                      "C-v" 'paste)
                   nkeymaps/keyscheme:emacs (list "M-w" 'copy
                                      "M-y" 'paste)))
         (cua-keymap (nkeymaps:make-keymap "test-cua-map"))
         (emacs-keymap (nkeymaps:make-keymap "test-emacs-map")))
    (nkeymaps:define-key cua-keymap
      "C-c" 'copy
      "C-v" 'paste)
    (nkeymaps:define-key emacs-keymap
      "M-w" 'copy
      "M-y" 'paste)
    (prove:is (fset:convert 'fset:map (nkeymaps:keymap->map (gethash nkeymaps/keyscheme:cua keyscheme-map)))
              (fset:convert 'fset:map (nkeymaps:keymap->map cua-keymap))
              :test #'fset:equal?)
    (prove:is (fset:convert 'fset:map (nkeymaps:keymap->map (gethash nkeymaps/keyscheme:emacs keyscheme-map)))
              (fset:convert 'fset:map (nkeymaps:keymap->map emacs-keymap))
              :test #'fset:equal?)))

(prove:subtest "Test inheritance"
  (let* ((keyscheme-map (nkeymaps:define-keyscheme-map "test"
                     nkeymaps/keyscheme:cua (list "C-c" 'copy
                                      "C-v" 'paste)
                   nkeymaps/keyscheme:emacs (list "M-w" 'copy
                                      "M-y" 'paste)))
         (cua-keymap (nkeymaps:make-keymap "test-cua-map"))
         (emacs-keymap (nkeymaps:make-keymap "test-emacs-map")))
    (nkeymaps:define-key cua-keymap
      "C-c" 'copy
      "C-v" 'paste)
    (nkeymaps:define-key emacs-keymap
      "M-w" 'copy
      "M-y" 'paste)
    (prove:is (list (gethash nkeymaps/keyscheme:cua keyscheme-map))
              (nkeymaps:parents (gethash nkeymaps/keyscheme:emacs keyscheme-map)))))

(prove:subtest "Get keymap"
  (let* ((keyscheme-map (nkeymaps:define-keyscheme-map "test"
                   nkeymaps/keyscheme:cua (list "C-c" 'copy
                                    "C-v" 'paste)
                   nkeymaps/keyscheme:emacs (list "M-w" 'copy
                                      "M-y" 'paste))))
    (prove:ok (nkeymaps:get-keymap nkeymaps/keyscheme:emacs keyscheme-map))
    (prove:ok (nkeymaps:get-keymap nkeymaps/keyscheme:cua keyscheme-map))
    (prove:isnt (nkeymaps:get-keymap nkeymaps/keyscheme:cua keyscheme-map)
                (nkeymaps:get-keymap nkeymaps/keyscheme:emacs keyscheme-map))
    (prove:is (nkeymaps:get-keymap nkeymaps/keyscheme:cua keyscheme-map)
              (nkeymaps:get-keymap nkeymaps/keyscheme:vi-normal keyscheme-map))))

(prove:subtest "Prioritize scheme over parent."
  (let* ((keyscheme-map1 (nkeymaps:define-keyscheme-map "test1"
                           nkeymaps/keyscheme:cua (list "C-c" 'do-not-hit-me)
                           nkeymaps/keyscheme:emacs (list "M-w" 'copy)))
         (keyscheme-map2 (nkeymaps:define-keyscheme-map "test2"
                           nkeymaps/keyscheme:emacs (list "C-c" 'hit-me))))
    (let ((keymaps (mapcar (lambda (scheme) (nkeymaps:get-keymap nkeymaps/keyscheme:emacs scheme))
                           (list keyscheme-map1 keyscheme-map2))))
      (prove:is (nkeymaps:lookup-key "C-c" keymaps)
                'hit-me))))

;; (prove:subtest "Make scheme with type errors" ; TODO: How do we test macro-expansion-time error?
;;   (prove:is-error (nkeymaps:define-keyscheme-map
;;                       nkeymaps/keyscheme:cua (list "C-" 'copy))
;;                   'type-error))

(prove:finalize)
