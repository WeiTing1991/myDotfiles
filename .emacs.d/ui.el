;; ui.el

;; statusline
(use-package all-the-icons
  :if (display-graphic-p))


(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom (
					 (doom-modeline-support-imenu t)

					 (doom-modeline-height 35)
					 (doom-modeline-bar-width 2)
					 (doom-modeline-icon nil)
					 (doom-modeline-buffer-encoding t)
					 (doom-modeline-persp-name t)
					 (doom-modeline-display-default-persp-name nil)
					 (doom-modeline-project-root t)

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

;; TODO
;; check https://github.com/DarthFennec/highlight-indent-guides
;; indent mode
(electric-indent-mode t)
;; (use-package highlight-indent-guides
;; )
;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;; (setq highlight-indent-guides-method 'line)


(custom-set-faces
 '(cursor ((t (:background "#eb6f92" :foreground "white")))))

(setq scroll-margin 100)               ;; Keep 5 lines above/below the cursor
(setq scroll-conservatively 10000)   ;; Keep the cursor in the same position
(setq scroll-preserve-screen-position t)  ;; Maintain screen position

;; (use-package centered-cursor-mode
;;   :demand
;;   :config
;;   ;; Optional, enables centered-cursor-mode in all buffers.
;;   (global-centered-cursor-mode))

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
