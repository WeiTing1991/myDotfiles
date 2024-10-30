;;; ui.el

;;; code:

;; NOTE
;; https://symbl.cc/en/2022/
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


;; Modeline
;; https://github.com/domtronn/all-the-icons.el/tree/svg
;; Hide the mode line globally
;; https://www.reddit.com/r/emacs/comments/6ftm3x/share_your_modeline_customization/

(setq-default mode-line-format nil)

(set-face-attribute 'mode-line nil
                    ;; :height 1.5
                    ;; :background "#0D0907"
                    :box t
                    )
(set-face-attribute 'mode-line-inactive nil
                    ;; :background "#0D0907"
                    ;; :height 1.5
                    :box t)

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

;; (set-face-attribute 'mode-line-active nil :height 100)
;; (set-face-attribute 'mode-line-inactive nil :height 100)

(setq-default mode-line-format
              '((:eval
                 (simple-mode-line-render
                  ;; Left.
                  (quote ("%e "
                          evil-mode-line-tag
                          "[%*]"
                          " "
                          (:propertize
                           ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))
                          " "
                          " %l : %c"
                          " "
                          (vc-mode vc-mode)
                          mode-line-frame-identification '(" %b")
                          mode-line-buffer-identification
                          )
                         )
                  ;; Right.
                  (quote ("%p "
                          ;; mode-line-format-right-align
                          (:eval (format " Tab (%d) " tab-width))
                          mode-line-frame-identification
                          mode-line-modes
                          mode-line-misc-info)))
                 )
                )
              )


;; Hook into all programming modes
;; (add-hook 'prog-mode-hook 'enable-mode-line)


;; ;; Optionally disable mode line in other modes
;; (defun disable-mode-line ()
;;   "Disable the mode line."
;;   (setq mode-line-format nil))

;; ;; Example: disable mode line in text mode
;; (add-hook 'text-mode-hook 'disable-mode-line)

(use-package minions
  :straight t
  :demand t
  :config
  (minions-mode 1)
  )

;; cursor
(custom-set-faces
 '(cursor ((t (:background "#eb6f92" :foreground "white")))))

(use-package rainbow-mode
  :straight t
  :defer t
  )


;;TODO
(use-package hl-todo
  :straight t
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(
          ("TODO"    . (:foreground "#232136" :background "#f6c177" :weight 'bold))
          ("NOTE"    . (:foreground "#232136" :background "#6e6a86" :weight 'bold))
          ("BUG"     . (:foreground "#232136" :background "#eb6f92" :weight 'bold))
          ("DEPRECATED" font-lock-doc-face bold)))
  )

;; (text       "#e0def4")  ;; foreground
;; (base       "#232136")  ;; background
;; (high       "#393552")  ;; highlight
;; (gold       "#f6c177")  ;; critical
;; (iris       "#c4a7e7")  ;; salient
;; (surface    "#6e6a86")  ;; strong
;; (love       "#eb6f92")  ;; popout
;; (subtle     "#2a273f")  ;; subtle
;; (faded      "#6e6a86")  ;; faded
;; (cursor     "#c4a7e7")) ;; cursor

;; ("HACK"       font-lock-constant-face bold)
;; ("REVIEW"     font-lock-keyword-face bold)
;; (("HOLD" . "#d0bf8f")
;;  ("TODO" . "#cc9393")
;;  ("NEXT" . "#dca3a3")
;;  ("THEM" . "#dc8cc3")
;;  ("PROG" . "#7cb8bb")
;;  ("OKAY" . "#7cb8bb")
;;  ("DONT" . "#5f7f5f")
;;  ("FAIL" . "#8c5353")
;;  ("DONE" . "#afd8af")
;;  ("NOTE" . "#d0bf8f")
;;  ("MAYBE" . "#d0bf8f")
;;  ("KLUDGE" . "#d0bf8f")
;;  ("HACK" . "#d0bf8f")
;;  ("TEMP" . "#d0bf8f")
;;  ("FIXME" . "#cc9393")
;;  ("XXXX*" . "#cc9393"))

(provide 'ui)
;;; ui.el ends here
