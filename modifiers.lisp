;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nkeymaps/modifier)

(defparameter +control+ (nkeymaps:define-modifier :string "control" :shortcut "C"))
(defparameter +meta+ (define-modifier :string "meta" :shortcut "M"))
(defparameter +shift+ (define-modifier :string "shift" :shortcut "s"))
(defparameter +super+ (define-modifier :string "super" :shortcut "S"))
(defparameter +hyper+ (define-modifier :string "hyper" :shortcut "H"))
