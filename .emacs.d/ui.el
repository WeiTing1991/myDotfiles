;; ui.el

;; statusline
(use-package all-the-icons
  :if (display-graphic-p))

;; TODO
;; check https://github.com/DarthFennec/highlight-indent-guides

;; indent mode and highlight
;; (electric-indent-mode t)
(use-package highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)
(setq highlight-indent-guides-character ?▏)
(setq highlight-indent-guides-responsive 'top)

(setq highlight-indent-guides-auto-enabled nil)
(set-face-background 'highlight-indent-guides-odd-face "darkgray")
(set-face-background 'highlight-indent-guides-even-face "darkgray")
(set-face-foreground 'highlight-indent-guides-character-face "dimgray")
(setq highlight-indent-guides-delay 0)

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom (
					 (doom-modeline-support-imenu t)
					 (doom-modeline-height 40)
           ;;
					 (doom-modeline-bar-width 5)
					 (doom-modeline-icon nil)
					 (doom-modeline-buffer-encoding t)
					 ;; (doom-modeline-persp-name t)
					 (doom-modeline-display-default-persp-name nil)
					 (doom-modeline-project-root t)
           (doom-modeline-project-detection 'auto)
           ;;
					 (doom-modeline-lsp t)           ;; Display LSP status
					 (doom-modeline-git t)           ;; Display Git status
					 (doom-modeline-buffer-file-name-style 'truncate-with-project) ;; File info
					 ;; (doom-modeline-major-mode-icon t) ;; Display icon for the major mode
					 ;; (doom-modeline-major-mode-color-icon t)
					 (doom-modeline-indent-info t)   ;; Show indentation
					 (doom-modeline-checker-simple-format t) ;; Spell checker and linter
					 ;; (doom-modeline-env-version t)  ;; Show environment version (Python, etc.)
		)
  )


(custom-set-faces
 '(cursor ((t (:background "#eb6f92" :foreground "white")))))

;; TODO
(use-package rainbow-mode
	:straight t
	:defer t
  ;; :hook (prog-mode . rainbow-mode)
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
          ("DEPRECATED" font-lock-doc-face bold)))
)
