;;; app.el

;;; code:
;; Spell
;; NOTE https://github.com/redguardtoo/wucuo?tab=readme-ov-file
(setq ispell-program-name "aspell")
;; You could add extra option "--camel-case" for camel case code spell checking if Aspell 0.60.8+ is installed
;; @see https://github.com/redguardtoo/emacs.d/issues/796
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))

(flyspell-mode -1)
(use-package wucuo
  :straight t
  :defer t
  :config
  ;; (add-hook 'prog-mode-hook #'wucuo-start)
  (add-hook 'org-mode-hook #'wucuo-start)
  (add-hook 'markdon-mode-hook #'wucuo-start)
  )

;; pandoc
(cond
 ((eq system-type 'darwin)  ;; 'darwin' is for macOS
   (setq pandoc-binary "/opt/homebrew/bin/pandoc")
  )
 ((eq system-type 'windows-nt)  ;; 'windows-nt' is for Windows
   (setq pandoc-binary "/AppData/Local/Pandoc/pandoc.exe")
  )
 )

;; org mode
;; https://doc.norang.ca/org-mode.html
(use-package org
  :straight t
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  )

;; ;; custom-visual mode
;; (custom-set-faces
;;  '(markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
;;  '(markdown-header-face-1 ((t (:height 1.8 :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face))))
;;  '(markdown-header-face-2 ((t (:height 1.4 :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face))))
;;  '(markdown-header-face-3 ((t (:height 1.2 :foreground "#D08770" :weight extra-bold :inherit markdown-header-face))))
;;  '(markdown-header-face-4 ((t (:height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face))))
;;  '(markdown-header-face-5 ((t (:height 1.1 :foreground "#b48ead" :weight bold :inherit markdown-header-face))))
;;  '(markdown-header-face-6 ((t (:height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face))))
;;  '(markdown-code-face ((t (:background "#2E2E2E" :foreground "#FFFFFF"))))  ;; Change colors as needed
;;  ;; '(markdown-inline-code-face ((t (:background "#2E2E2E" :foreground "#FFFFFF" :weight bold))))  ;; Inline code
;;  )

;; (setq org-agenda-files '("~/org"))



(provide 'app)
;;; app.el ends here
