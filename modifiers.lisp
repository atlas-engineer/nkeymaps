;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nkeymaps/modifier)

(defparameter +control+ (define-modifier :string "control" :shortcut "C")
  "The key modifier commonly known as 'control'.")
(defparameter +meta+ (define-modifier :string "meta" :shortcut "M")
  "The key modifier commonly known as 'alt', 'option' or 'meta'.")
(defparameter +shift+ (define-modifier :string "shift" :shortcut "s")
  "The key modifier commonly known as 'shift'.")
(defparameter +super+ (define-modifier :string "super" :shortcut "S")
  "The key modifier commonly known as 'windows', 'command' or 'super'.")
(defparameter +hyper+ (define-modifier :string "hyper" :shortcut "H")
  "The key modifier commonly known as 'hyper'.")
