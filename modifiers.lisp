;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nkeymaps/modifier)

(defparameter +control+
  (define-modifier :string "control" :shortcut-emacs "C"
    :shortcut-cua #+darwin "control" #-darwin "Ctrl")
  "The key modifier commonly known as 'control'.")
(defparameter +meta+
  (define-modifier :string "meta" :shortcut-emacs "M"
    :shortcut-cua #+darwin "option" #-darwin "Alt")
  "The key modifier commonly known as 'alt', 'option' or 'meta'.")
(defparameter +shift+
  (define-modifier :string "shift" :shortcut-emacs "s"
    :shortcut-cua #+darwin "shift" #-darwin "Shift")
  "The key modifier commonly known as 'shift'.")
(defparameter +super+
  (define-modifier :string "super" :shortcut-emacs "S"
    :shortcut-cua #+darwin "command" #+win32 "Win" #+linux "Super")
  "The key modifier commonly known as 'windows', 'command' or 'super'.")
(defparameter +hyper+
  (define-modifier :string "hyper" :shortcut-emacs "H"
    :shortcut-cua "Hyper")
  "The key modifier commonly known as 'hyper'.")
