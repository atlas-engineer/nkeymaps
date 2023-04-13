;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nkeymaps/modifier)

(defparameter +control+ (nkeymaps:define-modifier :string "control" :shortcut "C")
  "The key modifier commonly known as 'control'.")
(defparameter +meta+ (define-modifier :string "meta" :shortcut "M")
  "The key modifier commonly known as 'meta' or 'alt'.")
(defparameter +shift+ (define-modifier :string "shift" :shortcut "s")
  "The key modifier commonly known as 'shift'.")
(defparameter +super+ (define-modifier :string "super" :shortcut "S")
  "The key modifier commonly known as 'shift' or 'windows'.")
(defparameter +hyper+ (define-modifier :string "hyper" :shortcut "H")
  "The key modifier commonly known as 'hyper'.")
