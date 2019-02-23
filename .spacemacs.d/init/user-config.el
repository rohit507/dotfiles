;; -*- mode: emacs-lisp -*-

;; -- Contact: rohit507@gmail.com --
;; -- Original Author: ekaschalk@gmail.com --
;;


(defun dotspacemacs/user-config ()
  "Configuration that cannot be delegated to layers."
  (dotspacemacs/user-config/pandoc)
  ;(dotspacemacs/user-config/ligatures)
  (dotspacemacs/user-config/defeat-smartparens)
  (dotspacemacs/user-config/line-width-bar)
  (dotspacemacs/user-config/toggles)
  (dotspacemacs/user-config/fish-color)
  (dotspacemacs/user-config/undo-tree-settings)
  (dotspacemacs/user-config/haskell-indent-settings)
  ;;(dotspacemacs/user-config/haskell-hare)

  (with-eval-after-load 'intero
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

  (require 'lsp-haskell)
  (add-hook 'haskell-mode-hook #'lsp-haskell-enable)

  ;; Change the default quit function to one that does not
  ;; interfere with an emacs-server setup
 ;;(evil-leader/set-key “q q” ‘spacemacs/frame-killer)
  )

(defun dotspacemacs/user-config/haskell-hare ()
  "Enable the HaRe haskell refactorer"
  (add-to-load-path "~/.cabal/share/x86_64-linux-ghc-8.0.2/HaRe-0.8.4.1/elisp")
  (require 'hare)
  (autoload 'hare-init "hare" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init) (hare-init)))
  )

(defun dotspacemacs/user-config/haskell-indent-settings ()
  "Setting for writing haskell code"

  (defun haskell-style ()
    "Sets the current buffer to use Haskell Style. Meant to be
  added to `haskell-mode-hook'"
    (interactive)
    (setq tab-width 2
          haskell-indentation-layout-offset 2
          haskell-indentation-left-offset 2
          haskell-indentation-ifte-offset 2))

  (add-hook 'haskell-mode-hook 'haskell-style)
  )
(defun dotspacemacs/user-config/undo-tree-settings ()
  "set up our global undotree and presistent undo settings"

  ;; (defadvice undo-tree-make-history-save-file-name
  ;;    (after undo-tree activate)
  ;;    (setq ad-return-value (concat ad-return-value ".gz")))

  (global-undo-tree-mode)

  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(concat spacemacs-cache-directory "undo")))
        undo-limit        100000000000
        undo-strong-limit 120000000000
        undo-outer-limit  100000000000)

  (unless (file-exists-p (concat spacemacs-cache-directory "undo"))
    (make-directory (concat spacemacs-cache-directory "undo")))

  (add-to-list 'warning-suppress-types '(undo discard-info))


  )

(defun dotspacemacs/user-config/pandoc ()
  "Automatically start pandoc mode in every markdown file"
  (add-hook 'markdown-mode-hook 'pandoc-mode))

(defun dotspacemacs/user-config/timestamps ()
  ;; Set the custom time stamps that we use.
  (setq org-time-stamp-custom-formats
        '("<%a %d-%b %Y>" . "<%a %d-%b %Y %H:%M>"))
  (setq-default org-display-custom-times t)

  ;; Bind org-time-stamp to a custom key
  (spacemacs/set-leader-keys-for-major-mode
    'markdown-mode
    "<"
    'markdown-exdent-region)

  (defun md-time-stamp-inactive () (interactive)
         (org-time-stamp-inactive 2))

  (spacemacs/set-leader-keys-for-major-mode
    'markdown-mode
    "."
    'md-time-stamp-inactive)
  (add-hook 'markdown-mode-hook
            (lambda () (interactive) (org-toggle-time-stamp-overlays))))


(defun dotspacemacs/user-config/defeat-smartparens ()
  "Defeat smartparens-mode in evil mode"
  (eval-after-load 'smartparens
    '(progn
       (sp-local-pair 'inferior-python-mode "(" nil :unless nil)
       (sp-local-pair 'inferior-python-mode "[" nil :unless nil)
       (sp-local-pair 'inferior-python-mode "'" nil :unless nil)
       (sp-local-pair 'inferior-python-mode "\"" nil :unless nil)))

  (remove-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'evil-insert-state-entry-hook 'turn-off-smartparens-mode)
  (add-hook 'evil-insert-state-exit-hook 'turn-on-smartparens-mode))

(defun dotspacemacs/user-config/fish-color ()
  (add-hook 'term-mode-hook 'toggle-truncate-lines)
  )


(defun dotspacemacs/user-config/toggles ()
  "Spacemacs toggles not intended to be put into layers."
  (spacemacs/toggle-highlight-long-lines-globally-on)
  (spacemacs/toggle-mode-line-minor-modes-off)
  ;(spacemacs/toggle-aggressive-indent-globally-on)
  (global-highlight-parentheses-mode 1)
  (rainbow-delimiters-mode-enable)
  )


(defun dotspacemacs/user-config/ligatures ()
  "Add support for fira-code and hasklig ligatures."

  ;(global-prettify-symbols-mode 1)

  ; (defun make-spaces (el)
  ;   (let* ((space-width (string-width (car el)))
  ;          (out (append
  ;                (replicate '(?\s (Br . Bl)) (- space-width 1))
  ;                '(?\s (Br . Br))
  ;                (list (decode-char 'ucs (cdr el))))))
  ;     (progn (prin1 el) (prin1 (type-of out)) (prin1 out) (print "") out)))


  ; (defun make-tabs (el)
  ;   (let ((out (string ?\t (cdr el))))
  ;     (progn (prin1 el) (prin1 (type-of out)) (prin1 out) (print "") out)))

  (defun make-spaces (el)
    (let ((space-width (string-width (car el))))
      (append  (make-list (- space-width 1) '(?\s (Br . Bl)))
              '(?\s (Br . Br))
               (list (decode-char 'ucs (cdr el))))))

  (defun make-tabs (el) (string ?\t (cdr el)))

  (defun my-correct-symbol-bounds (pretty-alist)
    "Prepend a TAB character to each symbol in this alist,
this way compose-region called by prettify-symbols-mode
will use the correct width of the symbols
instead of the width measured by char-width."
    ;(let ((out (mapcar (lambda (el) (setcdr el (make-tabs el)) el) pretty-alist)))
    (let ((out (mapcar (lambda (el) (setcdr el (make-spaces el)) el) pretty-alist)))
      (progn (print out) out)))

  (defun my-ligature-list (ligatures codepoint-start)
    "Create an alist of strings to replace with
codepoints starting from codepoint-start."
    (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
      (-zip-pair ligatures codepoints)))

                                        ; list can be found at https://github.com/i-tu/Hasklig/blob/master/GlyphOrderAndAliasDB#L1588
  (setq my-hasklig-ligatures
        (let* ((ligs '("&&" "***" "*>" "\\\\" "||" "|>" "::"
                       "==" "===" "==>" "=>" "=<<" "!!" ">>"
                       ">>=" ">>>" ">>-" ">-" "->" "-<" "-<<"
                       "<*" "<*>" "<|" "<|>" "<$>" "<>" "<-"
                       "<<" "<<<" "<+>" ".." "..." "++" "+++"
                       "/=" ":::" ">=>" "->>" "<=>" "<=<" "<->")))
          (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))


  (setq my-fira-code-ligatures
        (let* ((ligs '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
                       "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
                       "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
                       "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
                       ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
                       "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
                       "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
                       "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
                       ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
                       "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
                       "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
                       "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
                       "x" "+" "+" "+" "*")))
          (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))

  ;; nice glyphs for haskell with hasklig
  (defun my-set-hasklig-ligatures ()
    "Add hasklig ligatures for use with prettify-symbols-mode."
    (setq prettify-symbols-alist
          (append my-hasklig-ligatures prettify-symbols-alist))
    (prettify-symbols-mode))
  (defun my-set-fira-code-ligatures ()
    "Add hasklig ligatures for use with prettify-symbols-mode."
    (setq prettify-symbols-alist
          (append my-fira-code-ligatures prettify-symbols-alist))
    (prettify-symbols-mode)
    (print prettify-symbols-alist))

  (add-hook 'prog-mode-hook 'my-set-fira-code-ligatures)
  (my-set-fira-code-ligatures)
  )

(defun dotspacemacs/user-config/line-width-bar ()
  "Activate column indicator in prog-mode and text-mode"
  (add-hook 'prog-mode-hook 'turn-on-fci-mode)
  (add-hook 'text-mode-hook 'turn-on-fci-mode)
  (setq fci-rule-color "#586e75")
  )


(defun dotspacemacs/user-config/frame-geom ()
  "Save and restore the frame geometry of emacs windows."

  (defun save-framegeometry ()
    "Gets the current frame's geometry and saves to ~/.emacs.d/framegeometry."
    (let (
          (framegeometry-left (frame-parameter (selected-frame) 'left))
          (framegeometry-top (frame-parameter (selected-frame) 'top))
          (framegeometry-width (frame-parameter (selected-frame) 'width))
          (framegeometry-height (frame-parameter (selected-frame) 'height))
          (framegeometry-file (expand-file-name "~/.emacs.d/framegeometry"))
          )

      (when (not (number-or-marker-p framegeometry-left))
        (setq framegeometry-left 0))
      (when (not (number-or-marker-p framegeometry-top))
        (setq framegeometry-top 0))
      (when (not (number-or-marker-p framegeometry-width))
        (setq framegeometry-width 0))
      (when (not (number-or-marker-p framegeometry-height))
        (setq framegeometry-height 0))

      (with-temp-buffer
        (insert
         ";;; This is the previous emacs frame's geometry.\n"
         ";;; Last generated " (current-time-string) ".\n"
         "(setq initial-frame-alist\n"
         "      '(\n"
         (format "        (top . %d)\n" (max framegeometry-top 0))
         (format "        (left . %d)\n" (max framegeometry-left 0))
         (format "        (width . %d)\n" (max framegeometry-width 0))
         (format "        (height . %d)))\n" (max framegeometry-height 0)))
        (when (file-writable-p framegeometry-file)
          (write-file framegeometry-file))))
    )

  (defun load-framegeometry ()
    "Loads ~/.emacs.d/framegeometry which should load the previous frame's
  geometry."
    (let ((framegeometry-file (expand-file-name "~/.emacs.d/framegeometry")))
      (when (file-readable-p framegeometry-file)
        (load-file framegeometry-file)))
    )


  ;; Restore Frame size and location, if we are using gui emacs
  (if window-system
      (progn
        (add-hook 'after-init-hook 'load-framegeometry)
        (add-hook 'kill-emacs-hook 'save-framegeometry))
    )
  )



  ;; ;; Enable org's table editor in markdown mode
  ;; (add-hook 'markdown-mode-hook 'turn-on-orgtbl)

  ;; ;; Format for this function from http://ergoemacs.org/emacs/elisp_command_working_on_string_or_region.html
  ;; (defun markdown-fmt-orgtbl (string &optional from to)
  ;;   "Formats Org-mode style tables as GitHub Markdown Tables.
  ;; en called interactively, work on the current paragraph or text selection.
  ;; en called in Lisp code, if STRING is non-nil, returns a changed string.
  ;;  STRING is nil, change the text in the region between positions FROM,  TO."
  ;;   (interactive
  ;;    (if (use-region-p)
  ;;        (list nil (region-beginning) (region-end))
  ;;      (let ((bds (bounds-of-thing-at-point 'paragraph)))
  ;;        (list nil (car bds) (cdr bds)))))
  ;;   (let* ((work-on-string-p (if string t nil))
  ;;          (result (if work-on-string-p
  ;;                      string
  ;;                    (buffer-substring-no-properties from to)))
  ;;          (case-fold-search t)
  ;;          (replace-pairs '(("-|"    . " |")
  ;;                           ("|-"    . "| ")
  ;;                           ("-\\+-" . " | "))))
  ;;     (dolist (pair replace-pairs)
  ;;       (setq result (replace-regexp-in-string (car pair) (cdr pair) result)))
  ;;     (if work-on-string-p
  ;;         result
  ;;       (save-excursion
  ;;         (delete-region from to)
  ;;         (goto-char from)
  ;;         (insert result)))))

  ;; ;; bind table formatter to <SPC> m t
  ;; (spacemacs/set-leader-keys-for-major-mode 'markdown-mode "t" 'markdown-fmt-orgtbl)
