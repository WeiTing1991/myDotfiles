;;; ui.el

;; statusline
(use-package all-the-icons
  :if (display-graphic-p))

(use-package nerd-icons
  :straight (nerd-icons
             :type git
             :host github
             :repo "rainstormstudio/nerd-icons.el"
             :files (:defaults "data"))
  :custom
  (nerd-icons-font-family "Hack Nerd Font")
)

;; TODO
;; check https://github.com/DarthFennec/highlight-indent-guides
;; indent mode and highlight
(use-package highlight-indent-guides
  :straight t
  :defer t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?▏)
  (setq highlight-indent-guides-responsive 'top)

  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "darkgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (setq highlight-indent-guides-delay 0)
  )

(use-package doom-modeline
  :straight t
  :defer 0
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-bar-width 5)
  (setq doom-modeline-height 30)
  (setq doom-modeline-hud t)
  (setq doom-modeline-indent-info t)
  (setq doom-modeline-icon t)
  )

;; cursor
(custom-set-faces
 '(cursor ((t (:background "#eb6f92" :foreground "white")))))

;; TODO
(use-package rainbow-mode
  :straight t
  :defer t
  ;; :hook (prog-mode . rainbow-mode)
  )

(use-package hl-todo
  :straight t
  :defer t
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
