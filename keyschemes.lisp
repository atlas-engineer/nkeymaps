;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nkeymaps/keyscheme)

(defvar default (make-keyscheme "default")
  "The keyscheme parent to all other default keyschemes.")
(defvar cua (make-keyscheme "cua" default)
  "The CUA keyscheme as popularly used by GTK, Qt, Windows, Cocoa, etc.")
(defvar emacs (make-keyscheme "emacs" default)
  "The Emacs editor keyscheme.")
(defvar vi-normal (make-keyscheme "vi-normal" default)
  "The VI editor 'normal mode' keyscheme.")
(defvar vi-insert (make-keyscheme "vi-insert")
  "The VI editor 'insert mode' keyscheme.")
