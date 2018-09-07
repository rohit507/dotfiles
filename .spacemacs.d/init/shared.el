;; -*- mode: emacs-lisp -*-

;; -- Contact: rohit507@gmail.com --
;; -- Original Author: ekaschalk@gmail.com --
;; -- MIT License --
;; -- Emacs 25.3.2 ~ Spacemacs Dev Branch 0.300.0.x ~ pkgs updated: 05/06/18 --
;;
;; All configuration is housed in personal layers - see README.
;; `init.el' configures spacemacs, defining required `dotspacemacs/...' functions.

(defvar linux? (eq system-type 'gnu/linux)
  "Are we on a gnu/linux machine?")

(defun os-path (path)
  "Prepend drive label to PATH if on windows machine."
  (if linux?
      path
    (expand-file-name path "c:")))

;; dotspacemacs/init and all the default configuration variables are stored in
;; this file. This is where you change things like fonts, display settings, and
;; the like
(load "~/.spacemacs.d/init/config")

;; dotspacemacs/layers declarations are all in this file
(load "~/.spacemacs.d/init/layers")

;; dotspacemacs/user-config is defined here
(load "~/.spacemacs.d/init/user-config")


(defun dotspacemacs/user-init ()
  "Initialization function for user code.
   It is called immediately after `dotspacemacs/init', before layer configuration
   executes.
   This function is mostly useful for variables that need to be set
   before packages are loaded. If you are unsure, you should try in setting them in
   `dotspacemacs/user-config' first.
   Package independent settings to run before `dotspacemacs/user-config'."
  )
