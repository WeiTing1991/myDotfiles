;; ui.ai

;; statusline
(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom (
    (doom-modeline-height 25)
    (doom-modeline-bar-width 1)
    (doom-modeline-icon t)
    (doom-modeline-buffer-state-icon t)
    (doom-modeline-buffer-encoding t)
    (doom-modeline-lsp t)           ;; Display LSP status
    ;; (doom-modeline-github t)        ;; Enable GitHub support
    (doom-modeline-git t)           ;; Display Git status
    (doom-modeline-buffer-file-name-style 'truncate-with-project) ;; File info
    (doom-modeline-major-mode-icon t) ;; Display icon for the major mode
    (doom-modeline-major-mode-color-icon t)
    (doom-modeline-indent-info t)   ;; Show indentation
    (doom-modeline-checker-simple-format t) ;; Spell checker and linter
    (doom-modeline-env-version t)  ;; Show environment version (Python, etc.)   (doom-modeline-indent-info t)
		)
  )

;; TODO check how to change it
(use-package indent-bars
  :straight t
  :custom
  (indent-bars-no-descend-lists t) ; no extra bars in continued func arg lists
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))

  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
	  if_statement with_statement while_statement)))

  :hook ((emacs-lisp-mode prog-mode) . indent-bars-mode)
	)

(setq-default
    indent-bars-color '(highlight :face-bg t :blend 0.2)
    indent-bars-pattern "."
    indent-bars-width-frac 0.1
    indent-bars-pad-frac 0.1
    indent-bars-zigzag nil
    indent-bars-color-by-depth nil
    indent-bars-highlight-current-depth '(:blend 0.5)
    indent-bars-display-on-blank-lines nil
)

;; color show
(use-package rainbow-delimiters
  :disabled t
  :hook (prog-mode . rainbow-delimiters-mode))

;; cursor
(custom-set-faces
 '(cursor ((t (:background "#eb6f92" :foreground "white")))))

(use-package rainbow-mode
  :diminish
  :hook (org-mode prog-mode)
	;; :bind 
	)

(use-package hl-todo
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))
