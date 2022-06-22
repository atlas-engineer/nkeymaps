;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nkeymaps/tests)

(prove:plan nil)

(prove:subtest "Make scheme"
  (let* ((scheme (nkeymaps:define-scheme "test"
                   nkeymaps/scheme:cua '("C-c" copy
                                "C-v" paste)))
         (keymap (nkeymaps:make-keymap "test-cua-map")))
    (nkeymaps:define-key keymap "C-c" 'copy)
    (nkeymaps:define-key keymap "C-v" 'paste)
    (prove:is (fset:convert 'fset:map (nkeymaps:keymap->map (gethash nkeymaps/scheme:cua scheme)))
              (fset:convert 'fset:map (nkeymaps:keymap->map keymap))
              :test #'fset:equal?)
    (prove:is (nkeymaps:name (gethash nkeymaps/scheme:cua scheme))
              (nkeymaps:name keymap))))

(prove:subtest "Make scheme with LIST"
  (let* ((scheme (nkeymaps:define-scheme "test"
                     nkeymaps/scheme:cua (list "C-c" 'copy
                                      "C-v" 'paste)))
         (keymap (nkeymaps:make-keymap "test-cua-map")))
    (nkeymaps:define-key keymap
      "C-c" 'copy
      "C-v" 'paste)
    (prove:is (fset:convert 'fset:map (nkeymaps:keymap->map (gethash nkeymaps/scheme:cua scheme)))
              (fset:convert 'fset:map (nkeymaps:keymap->map keymap))
              :test #'fset:equal?)))

(prove:subtest "Make scheme with multiple names"
  (let* ((scheme (nkeymaps:define-scheme "test"
                     nkeymaps/scheme:cua (list "C-c" 'copy
                                      "C-v" 'paste)
                   nkeymaps/scheme:emacs (list "M-w" 'copy
                                      "M-y" 'paste)))
         (cua-keymap (nkeymaps:make-keymap "test-cua-map"))
         (emacs-keymap (nkeymaps:make-keymap "test-emacs-map")))
    (nkeymaps:define-key cua-keymap
      "C-c" 'copy
      "C-v" 'paste)
    (nkeymaps:define-key emacs-keymap
      "M-w" 'copy
      "M-y" 'paste)
    (prove:is (fset:convert 'fset:map (nkeymaps:keymap->map (gethash nkeymaps/scheme:cua scheme)))
              (fset:convert 'fset:map (nkeymaps:keymap->map cua-keymap))
              :test #'fset:equal?)
    (prove:is (fset:convert 'fset:map (nkeymaps:keymap->map (gethash nkeymaps/scheme:emacs scheme)))
              (fset:convert 'fset:map (nkeymaps:keymap->map emacs-keymap))
              :test #'fset:equal?)))

(prove:subtest "Test inheritance"
  (let* ((scheme (nkeymaps:define-scheme "test"
                     nkeymaps/scheme:cua (list "C-c" 'copy
                                      "C-v" 'paste)
                   nkeymaps/scheme:emacs (list "M-w" 'copy
                                      "M-y" 'paste)))
         (cua-keymap (nkeymaps:make-keymap "test-cua-map"))
         (emacs-keymap (nkeymaps:make-keymap "test-emacs-map")))
    (nkeymaps:define-key cua-keymap
      "C-c" 'copy
      "C-v" 'paste)
    (nkeymaps:define-key emacs-keymap
      "M-w" 'copy
      "M-y" 'paste)
    (prove:is (list (gethash nkeymaps/scheme:cua scheme))
              (nkeymaps:parents (gethash nkeymaps/scheme:emacs scheme)))))

(prove:subtest "Get keymap"
  (let* ((scheme (nkeymaps:define-scheme "test"
                   nkeymaps/scheme:cua (list "C-c" 'copy
                                    "C-v" 'paste)
                   nkeymaps/scheme:emacs (list "M-w" 'copy
                                      "M-y" 'paste))))
    (prove:ok (nkeymaps:get-keymap nkeymaps/scheme:emacs scheme))
    (prove:ok (nkeymaps:get-keymap nkeymaps/scheme:cua scheme))
    (prove:isnt (nkeymaps:get-keymap nkeymaps/scheme:cua scheme)
                (nkeymaps:get-keymap nkeymaps/scheme:emacs scheme))
    (prove:is (nkeymaps:get-keymap nkeymaps/scheme:cua scheme)
              (nkeymaps:get-keymap nkeymaps/scheme:vi-normal scheme))))

(prove:subtest "Prioritize scheme over parent."
  (let* ((scheme1 (nkeymaps:define-scheme "test1"
                    nkeymaps/scheme:cua (list "C-c" 'do-not-hit-me)
                    nkeymaps/scheme:emacs (list "M-w" 'copy)))
         (scheme2 (nkeymaps:define-scheme "test2"
                    nkeymaps/scheme:emacs (list "C-c" 'hit-me))))
    (let ((keymaps (mapcar (lambda (scheme) (nkeymaps:get-keymap nkeymaps/scheme:emacs  scheme))
                           (list scheme1 scheme2))                   ))
      (prove:is (nkeymaps:lookup-key  "C-c" keymaps)
                'hit-me))))

;; (prove:subtest "Make scheme with type errors" ; TODO: How do we test macro-expansion-time error?
;;   (prove:is-error (nkeymaps:define-scheme
;;                       nkeymaps/scheme:cua (list "C-" 'copy))
;;                   'type-error))

(prove:finalize)
