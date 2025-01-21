;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nkeymaps/translator)

(defparameter +shift-layer-alist+
  `(("`" . "~")
    ("1" . "!")
    ("2" . "@")
    ("3" . "#")
    ("4" . "$")
    ("5" . "%")
    ("6" . "^")
    ("7" . "&")
    ("8" . "*")
    ("9" . "(")
    ("0" . ")")
    ("-" . "_")
    ("=" . "+")
    ("[" . "{")
    ("]" . "}")
    ("\\" . "|")
    (";" . ":")
    ("'" . "\"")
    ("," . "<")
    ("." . ">")
    ("/" . "?")
    ,@(loop for char across "qwertyuiopasdfghjklzxcvbnm"
            for char-string = (string char)
            collect (cons char-string (string-upcase char-string)))))

(declaim (ftype (function (string)) shift))
(defun shift (string)
  (alexandria:assoc-value +shift-layer-alist+ string :test 'string=))

(declaim (ftype (function (string)) unshift))
(defun unshift (string)
  (alexandria:rassoc-value +shift-layer-alist+ string :test 'string=))

(defun normalize-shift-modifier-state (keys)
  (mapcar (lambda (key)
            (or (alexandria:when-let ((unshifted-key (unshift (key-value key))))
                  (copy-key key :modifiers (fset:with (key-modifiers key) +shift+)
                                :value unshifted-key))
                key))
          keys))

(setf nkeymaps/core:*translator*
      (alexandria:compose #'list #'normalize-shift-modifier-state))
