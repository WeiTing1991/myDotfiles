;; lsp.el

;; treesitter
;; (global-treesit-auto-mode)

;; (dolist (lang '(python c cpp lua))
;;   (treesit-install-language-grammar lang))
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode)))


;; https://emacs-lsp.github.io/lsp-mode/page/installation/
;; lsp server
(defun wt/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :straight t 
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
)
;; https://emacs-lsp.github.io/lsp-ui/
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
)

;; auto compelte
(use-package company
  :straight t
  ;; :after lsp-mode
  :hook ((lsp-mode . company-mode)
         (emacs-lisp-mode)) 
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))

  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1)
  ;; (company-tooltip-align-annotations 't)
)
;; (use-package lua-mode)

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)
)

;; lsp setting
;; markdown
(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  )


