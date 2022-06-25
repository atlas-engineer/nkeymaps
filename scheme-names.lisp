;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; TODO: Rename to keyschemes.lisp.

(in-package :nkeymaps/keyscheme)

(defvar cua (make-keyscheme "cua"))
(defvar emacs (make-keyscheme "emacs" cua))
(defvar vi-normal (make-keyscheme "vi-normal" cua emacs))
(defvar vi-insert (make-keyscheme "vi-insert" cua))
