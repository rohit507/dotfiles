;; -*- mode: emacs-lisp -*-

;; -- Contact: rohit507@gmail.com --
;; -- Original Author: ekaschalk@gmail.com --
;;
(defun dotspacemacs/layers ()
  "Instantiate Spacemacs layers declarations and package configurations."
  (dotspacemacs/layers/config)
  (dotspacemacs/layers/packages))


(defun dotspacemacs/layers/config ()
  (setq-default
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for cofacebooknfiguration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path `("~/.spacemacs.d/layers/")

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers `(,@dotspacemacs/layers/local
                                       ,@dotspacemacs/layers/core
                                       ,@dotspacemacs/layers/langs
                                       ,@dotspacemacs/layers/extra))

  ;; Base distribution to use. This is a layer contained in the directory
  ;; `+distribution'. For now available distributions are `spacemacs-base'
  ;; or `spacemacs'. (default 'spacemacs)
  dotspacemacs-distribution 'spacemacs

  ;; Lazy installation of layers (i.e. layers are installed only when a file
  ;; with a supported type is opened). Possible values are `all', `unused'
  ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
  ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
  ;; lazy install any layer that support lazy installation even the layers
  ;; in `dotspacemacs-configuration-layers'. `nil' disable the lazy
  ;; installation feature and you have to explicitly list a layer in the
  ;; variable `dotspacemacs-configuration-layers' to install it.
  ;; (default 'unused)
  dotspacemacs-enable-lazy-installation 'unused)

;;;; Layers/packages

(defun dotspacemacs/layers/packages ()
  (setq-default

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; (default '())
   dotspacemacs-additional-packages '(solarized-theme
                                      nord-theme
                                      faceup
                                      protobuf-mode
                                      ;;(lsp-haskell :location (recipe :fetcher github :repo "emacs-lsp/lsp-haskell"))
                                      )

   ;; A list of packages that will not be installed and loaded.
   ;; (default '())
   dotspacemacs-excluded-packages '(fringe
                          importmagic
                          hide-comnt
                          help-fns+
                          info+)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))



;;; Spacemacs/Layers
;;;; Local

(defvar dotspacemacs/layers/local
  '((macros :location local)    ; All local layers depend on this layer
    (display :location local)   ; Pretty-eshell/code/outlines... pkgs
    )
  "Local layers housed in `~/.spacemacs.d/layers'.")

;;;; Core

(defvar dotspacemacs/layers/core
  '(better-defaults
    git
    github

    lsp ;; Language Server Protocol

    syntax-checking
    spell-checking

    (auto-completion :variables
                     auto-completion-return-key-behavior 'complete
                     auto-completion-tab-key-behavior 'cycle
                     auto-completion-enable-snippets-in-popup t)

    (auto-completion
         (haskell :variables haskell-completion-backend 'intero))

    (ivy :variables
         ivy-extra-directories nil)
         ;;ivy-re-builders-alist 'ivy--regex-fuzzy)

    (org :variables
         org-want-todo-bindings t)

    (shell :variables
           shell-default-term-shell "/user/bin/fish"
           shell-default-position 'bottom
	         shell-default-full-span nil
           )


    (version-control :variables
                     version-control-global-margin t
                     version-control-diff-tool 'git-gutter+))

  "Layers I consider core to Spacemacs.")


;;;; Langs

(defvar dotspacemacs/layers/langs
  '(;; Markups
    csv
    html
    markdown
    latex
    pandoc
    bibtex
    yaml

    ;; Languages
    c-c++
    emacs-lisp
    rust
    shell-scripts
    ruby

    (haskell :variables
             haskell-completion-backend 'intero
             haskell-enable-hindent-style "johan-tibell"
             haskell-process-type 'stack-ghci
             haskell-language-extensions '(
                                          "-XBangPatterns"
                                          "-XCPP"
                                          "-XConstraintKinds"
                                          "-XDataKinds"
                                          "-XDefaultSignatures"
                                          "-XDeriveDataTypeable"
                                          "-XDeriveFoldable"
                                          "-XDeriveFunctor"
                                          "-XDeriveGeneric"
                                          "-XDeriveTraversable"
                                          "-XDerivingStrategies"
                                          "-XEmptyCase"
                                          "-XEmptyDataDecls"
                                          "-XExistentialQuantification"
                                          "-XFlexibleContexts"
                                          "-XFlexibleInstances"
                                          "-XFunctionalDependencies"
                                          "-XGADTs"
                                          "-XGeneralizedNewtypeDeriving"
                                          "-XImplicitParams"
                                          "-XInstanceSigs"
                                          "-XKindSignatures"
                                          "-XLambdaCase"
                                          "-XMultiParamTypeClasses"
                                          "-XMultiWayIf"
                                          "-XNamedFieldPuns"
                                          "-XNoImplicitPrelude"
                                          "-XNoMonomorphismRestriction"
                                          "-XOverloadedLabels"
                                          "-XOverloadedStrings"
                                          "-XPackageImports"
                                          "-XPatternSynonyms"
                                          "-XQuasiQuotes"
                                          "-XRankNTypes"
                                          "-XRecordWildCards"
                                          "-XRecursiveDo"
                                          "-XScopedTypeVariables"
                                          "-XStandaloneDeriving"
                                          "-XTemplateHaskell"
                                          "-XTupleSections"
                                          "-XTypeApplications"
                                          "-XTypeFamilies"
                                          "-XTypeFamilyDependencies"
                                          "-XTypeOperators"
                                          "-XViewPatterns"
                                          ))

    (python :variables
            python-sort-imports-on-save t
            python-test-runner 'pytest
            :packages
            (not importmagic)))  ; Broken? Don't need it.

  "Programming and markup language layers.")

;;;; Extra

(defvar dotspacemacs/layers/extra
  '(gnus
    graphviz
    ;pdf-tools
    vagrant
    ranger

    (ibuffer :variables
             ibuffer-group-buffers-by 'projects))

  "Miscellaneous layers.")
