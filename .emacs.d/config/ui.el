;;; ui.el

;;; code
;; statusline
;; (use-package all-the-icons
;;   :if (display-graphic-p))

;; (use-package nerd-icons
;;   :straight (nerd-icons
;;              :type git
;;              :host github
;;              :repo "rainstormstudio/nerd-icons.el"
;;              :files (:defaults "data"))
;;   :custom
;;   (nerd-icons-font-family "Roboto")
;; )



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

;; workspace tab
;; (set-face-attribute 'tab-bar ni l
;;                     :background "black"  ;; Tab bar background color
;;                     :foreground "white") ;; Tab bar text color

;; (set-face-attribute 'tab-bar-tab-inactive  nil
;;                     :background "black"    ;; Background color for active tab
;;                     :foreground "white"   ;; Text color for active tab
;;                     :weight 'bold)        ;; Font weight for active ta

;; (set-face-attribute 'tab-bar-tab nil
;;                     :background "dim gray"    ;; Background color for active tab
;;                     :foreground "white"   ;; Text color for active tab
;;                     :weight 'light)        ;; Font weight for active ta


;; modeline
;; https://github.com/seagle0128/doom-modeline
;; (setq-default mode-line-format nil)
;; (use-package doom-modeline
;;   :straight t
;;   :hook (prog-mode . doom-modeline-mode)
;;   )

(set-face-attribute 'mode-line nil
                    :background "#0D0907"  ;; Background color
                    :box nil)              ;; Remove box around the mode line

(set-face-attribute 'mode-line-inactive nil
                    :background "#0D0907"  ;; Inactive background color
                    :box nil)              ;; Remove box around the inactive mode line


(use-package lambda-line
  :straight (:type git :host github :repo "lambda-emacs/lambda-line")
  :custom
  (lambda-line-icon-time t) ;; requires ClockFace font (see below)
  (lambda-line-clockface-update-fontset "ClockFaceRect") ;; set clock icon
  (lambda-line-position 'bottom) ;; Set position of status-line
  (lambda-line-abbrev t) ;; abbreviate major modes
  (lambda-line-hspace "  ")  ;; add some cushion
  (lambda-line-prefix t) ;; use a prefix symbol
  (lambda-line-prefix-padding nil) ;; no extra space for prefix
  (lambda-line-status-invert nil)  ;; no invert colors
  (lambda-line-gui-ro-symbol  " ⨂") ;; symbols
  (lambda-line-gui-mod-symbol " ⬤")
  (lambda-line-gui-rw-symbol  " ◯")
  (lambda-line-space-top +.50)  ;; padding on top and bottom of line
  (lambda-line-space-bottom -.50)
  (lambda-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
  :config
  ;; activate lambda-line
  (lambda-line-mode)
  ;; set divider line in footer
  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_"))))

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
