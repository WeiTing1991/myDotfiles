;; lsp.el
;; TODO add the linter keybinding, and check md

;; treesitter
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; https://emacs-lsp.github.io/lsp-mode/page/installation/
;; lsp server
(defun wt/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . wt/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
)

;; https://emacs-lsp.github.io/lsp-ui/
(use-package lsp-ui
  :hook ((prog-mode lsp-mode). lsp-ui-mode)
  :custom
  (lsp-ui-doc-position)
  )

;; auto compelte
;; TODO
;; https://company-mode.github.io/manual/
(use-package company
  :straight t
  :hook ((lsp-mode . company-mode)
         (emacs-lisp-mode)
         )
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))

  :config
  (push 'company-lsp company-backends)

  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 1)
	(company-tooltip-limit 8)
  ;; (company-tooltip-align-annotations 't)
  )

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-files)
  (add-to-list 'company-backends 'company-capf)
)

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)
)

;;https://github.com/daviwil/dotfiles/blob/master/.emacs.d/modules/dw-interface.el
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary t)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init
  (global-corfu-mode t)
	)

(use-package lsp-treemacs
  :after lsp
  )
;; Syntax check as linter
;; NOTE
;; https://www.flycheck.org/en/latest/user/installation.html
(use-package flycheck
  :straight t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  )

;; for Emacs Lisp
(with-eval-after-load 'flycheck
  '(flycheck-package-setup)
)

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1)
  )

;; lsp setting

;; (load-file (expand-file-name "..el" user-emacs-directory))

;; TODO check this https://www.reddit.com/r/emacs/comments/10h9jf0/beautify_markdown_on_emacs/
;; markdown
(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  ;; :init (setq markdown-command "multimarkdown"))
  )
(add-hook 'markdown-mode-hook 'lsp)

(custom-set-faces
 '(markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
 '(markdown-header-face-1 ((t (:height 1.8 :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face))))
 '(markdown-header-face-2 ((t (:height 1.4 :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face))))
 '(markdown-header-face-3 ((t (:height 1.2 :foreground "#D08770" :weight extra-bold :inherit markdown-header-face))))
 '(markdown-header-face-4 ((t (:height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face))))
 '(markdown-header-face-5 ((t (:height 1.1 :foreground "#b48ead" :weight bold :inherit markdown-header-face))))
 '(markdown-header-face-6 ((t (:height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face))))
)

;; lua
(use-package lua-mode)
(use-package c++-mode)

;; c/cpp




;; https://github.com/copilot-emacs/copilot.el
