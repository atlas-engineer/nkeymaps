;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nkeymaps/translator)

(declaim (ftype (function (string) string) toggle-case))
(defun toggle-case (string)
  "Return the input with reversed case if it has only one character."
  (if (= 1 (length string))
      (let ((down (string-downcase string)))
        (if (string= down string)
            (string-upcase string)
            down))
      string))

(defun translate-remove-shift-toggle-case (keys)
  "With shift, keys without shift and with their key value case reversed:
'shift-a shift-B' -> 'A b'."
  (let ((shift? (find +shift+ keys :key #'key-modifiers :test #'fset:find)))
    (when shift?
      (mapcar (lambda (key)
                (copy-key key :modifiers (fset:less (key-modifiers key) +shift+)
                              :value (toggle-case (key-value key))))
              keys))))

(defun translate-remove-shift (keys)
  "With shift, keys without shift: 'shift-a' -> 'a'."
  (let ((shift? (find +shift+ keys :key #'key-modifiers :test #'fset:find)))
    (when shift?
      (mapcar (lambda (key)
                (copy-key key :modifiers (fset:less (key-modifiers key) +shift+)))
              keys))))

(defun translate-remove-but-first-control (keys)
  "With control, keys without control except for the first key:
'C-x C-c' -> 'C-x c'."
  (let ((control? (find +control+ (rest keys) :key #'key-modifiers :test #'fset:find)))
    (when control?
      (cons (first keys)
            (mapcar (lambda (key)
                      (copy-key key :modifiers (fset:less (key-modifiers key) +control+)))
                    (rest keys))))))

(defun translate-remove-shift-but-first-control (keys)
  "With control and shift, keys without control except for the first key and
without shift everywhere: 'C-shift-C C-shift-f' -> 'C-C f. "
  (let ((shift? (find +shift+ keys :key #'key-modifiers :test #'fset:find))
        (control? (find +control+ (rest keys) :key #'key-modifiers :test #'fset:find)))
    (when (and control? shift?)
               (cons (copy-key (first keys)
                               :modifiers (fset:less (key-modifiers (first keys)) +shift+))
                     (mapcar (lambda (key)
                               (copy-key key :modifiers (fset:set-difference (key-modifiers key)
                                                                             (fset:set +control+ +shift+))))
                             (rest keys))))))

(defun translate-remove-shift-but-first-control-toggle-case (keys)
  "With control and shift, keys without control except for the first key and
without shift everywhere: 'C-shift-C C-shift-f' -> 'C-c F. "
  (let ((control? (find +control+ (rest keys) :key #'key-modifiers :test #'fset:find))
        (shift? (find +shift+ keys :key #'key-modifiers :test #'fset:find)))
    (when (and control? shift?)
               (cons (copy-key (first keys)
                               :value (toggle-case (key-value (first keys)))
                               :modifiers (fset:less (key-modifiers (first keys)) +shift+))
                     (mapcar (lambda (key)
                               (copy-key key
                                         :value (toggle-case (key-value key))
                                         :modifiers (fset:set-difference (key-modifiers key)
                                                                         (fset:set +control+ +shift+))))
                             (rest keys))))))

(defun translate-shift-control-combinations (keys)
  "Return the successive translations of
- `translate-remove-shift',
- `translate-remove-shift-toggle-case',
- `translate-remove-but-first-control',
- `translate-remove-shift-but-first-control',
- `translate-remove-shift-but-first-control-toggle-case'.

We first remove shift before toggle the case because we want 's-A' to match an
'A' binding before matching 'a'."
  (delete nil
          (mapcar (lambda (translator) (funcall translator keys))
                  (list #'translate-remove-shift
                        #'translate-remove-shift-toggle-case
                        #'translate-remove-but-first-control
                        #'translate-remove-shift-but-first-control
                        #'translate-remove-shift-but-first-control-toggle-case))))

(setf nkeymaps/core:*translator* #'translate-shift-control-combinations)
