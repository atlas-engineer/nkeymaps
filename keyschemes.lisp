;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nkeymaps/keyscheme)

(defvar default (make-keyscheme "default"))
(defvar cua (make-keyscheme "cua" default))
(defvar emacs (make-keyscheme "emacs" default))
(defvar vi-normal (make-keyscheme "vi-normal" default))
(defvar vi-insert (make-keyscheme "vi-insert"))
