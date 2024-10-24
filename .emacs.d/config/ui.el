;;; ui.el

;;; code:

;; indent mode and highlight
;; check https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :straight t
  :defer t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?▏)
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides--bitmap-line t)

  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "darkgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (setq highlight-indent-guides-delay 0)
  )

;; modeline
;; Hide the mode line globally
(setq-default mode-line-format nil)

;; Enable the mode line in programming modes
(defun enable-mode-line ()
  "Enable the mode line in programming modes."
  (setq mode-line-format (default-value 'mode-line-format)))

;; Hook into all programming modes
(add-hook 'prog-mode-hook 'enable-mode-line)

(setq-default mode-line-format
              '("%e" "  "
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                mode-line-format-right-align
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                mode-line-misc-info
                "  ")
              mode-line-percent-position nil
              mode-line-buffer-identification '(" %b")
              mode-line-position-column-line-format '(" %l:%c"))

(set-face-attribute 'mode-line nil :weight 'bold)
(set-face-attribute 'mode-line-inactive nil :weight 'bold)

(use-package minions
  :straight t
  :demand t
  :config
  (minions-mode 1)
  )

;; (set-face-attribute 'mode-line nil
;;                     :background "#0D0907"  ;; Background color
;;                     :box nil)              ;; Remove box around the mode line
;; (set-face-attribute 'mode-line-inactive nil
;;                     :background "#0D0907"  ;; Inactive background color
;;                     :box nil)              ;; Remove box around the inactive mode line

;; cursor
(custom-set-faces
 '(cursor ((t (:background "#eb6f92" :foreground "white")))))

(use-package rainbow-mode
  :straight t
  :defer t
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

(provide 'ui)
;;; ui.el ends here
