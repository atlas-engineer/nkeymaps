;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nkeymaps/modifier)

(defparameter +control+
  (define-modifier :string "control" :shortcut "C"
    :system-name #+darwin "control" #-darwin "Ctrl")
  "The key modifier commonly known as 'control'.")
(defparameter +meta+
  (define-modifier :string "meta" :shortcut "M"
    :system-name #+darwin "option" #-darwin "Alt")
  "The key modifier commonly known as 'alt', 'option' or 'meta'.")
(defparameter +shift+
  (define-modifier :string "shift" :shortcut "s"
    :system-name #+darwin "shift" #-darwin "Shift")
  "The key modifier commonly known as 'shift'.")
(defparameter +super+
  (define-modifier :string "super" :shortcut "S"
    :system-name #+darwin "command" #+win32 "Win" #+linux "Super")
  "The key modifier commonly known as 'windows', 'command' or 'super'.")
(defparameter +hyper+
  (define-modifier :string "hyper" :shortcut "H"
    :system-name "Hyper")
  "The key modifier commonly known as 'hyper'.")
