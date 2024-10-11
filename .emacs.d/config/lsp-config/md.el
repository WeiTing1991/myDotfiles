;;; markdown

(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . markdon-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  ;; :init (setq markdown-command "multimarkdown"))
  )

(use-package gh-md
  :defer t
  :after markdown-mode
  :straight (:build t)
  :general
  (wt/leader-key
    :packages 'gh-md
    :keymaps 'markdown-mode-map
    "mr" #'gh-md-render-buffer)
  )

;; (custom-set-faces
;;  '(markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
;;  '(markdown-header-face-1 ((t (:height 1.8 :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face))))
;;  '(markdown-header-face-2 ((t (:height 1.4 :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face))))
;;  '(markdown-header-face-3 ((t (:height 1.2 :foreground "#D08770" :weight extra-bold :inherit markdown-header-face))))
;;  '(markdown-header-face-4 ((t (:height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face))))
;;  '(markdown-header-face-5 ((t (:height 1.1 :foreground "#b48ead" :weight bold :inherit markdown-header-face))))
;;  '(markdown-header-face-6 ((t (:height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face))))
;; )


