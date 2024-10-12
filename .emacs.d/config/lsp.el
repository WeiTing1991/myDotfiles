;;;lsp.el

;; treesitter
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit
  :defer t
  :straight (:type built-in)
  :hook ((bash-ts-mode
          c-ts-mode
          c++-ts-mode
          html-ts-mode
          js-ts-mode
          typescript-ts-mode
          json-ts-mode
          rust-ts-mode
          tsx-ts-mode
          python-ts-mode
          go-ts-mode
          css-ts-mode
          yaml-ts-mode) . lsp-deferred)
  :init
  (setq treesit-font-lock-level 4
        treesit-language-source-alist
        '((c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (js ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
          (java ("https://github.com/tree-sitter/java-tree-sitter"))
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (lua "https://github.com/Azganoth/tree-sitter-lua")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  )

;; https://emacs-lsp.github.io/lsp-mode/page/installation/
;; lsp server

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
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
  :init
  (global-company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))

  :config
  (setq company-minimum-prefix-length     1
        company-idle-delay                0.0
        company-toolsip-limit             14
        company-tooltip-align-annotations nil
        company-require-match             'never
        company-backends '((company-capf company-files))
        )
  )

(use-package company-dict
  :after company
  :straight t
  :config
  (setq company-dict-dir (expand-file-name "dicts" user-emacs-directory)))

;; (with-eval-after-load 'company
;;   (add-to-list 'company-backends 'company-files)
;;   (add-to-list 'company-backends 'company-capf)
;; )

(use-package company-box
  :after company
  :straight t
  :hook (company-mode . company-box-mode)
)

;;https://github.com/daviwil/dotfiles/blob/master/.emacs.d/modules/dw-interface.el
;; (use-package corfu
;;   :defer t
;;   :custom
;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)                 ;; Enable auto completion
;;   ;; (corfu-separator ?\s)          ;; Orderless field separator
;;   (corfu-quit-at-boundary t)   ;; Never quit at completion boundary
;;   (corfu-quit-no-match t)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin

;;   ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
;;   :hook ((prog-mode . corfu-mode)
;;          (shell-mode . corfu-mode)
;;          (eshell-mode . corfu-mode))
;; 	)

;; (use-package lsp-treemacs
;;   :after lsp-mode
;;   )

;; NOTE
;; Syntax check as linter
;; https://www.flycheck.org/en/latest/user/installation.html
;; flyspell

(use-package flycheck
  :straight (:build t)
  :config
  (add-hook 'prog-mode-hook #'global-flycheck-mode)
  (setq flycheck-display-errors-delay 0.2))


;; for Emacs Lisp
(with-eval-after-load 'flycheck
  '(flycheck-package-setup)
)

;; (use-package yasnippet
;;   :defer t
;;   :straight t
;;   :init
;;   (yas-global-mode)
;;   :hook ((prog-mode . yas-minor-mode)
;;          (text-mode . yas-minor-mode)))


;; lsp server setting
(load-file (expand-file-name "./config/lsp-config/md.el" user-emacs-directory))

;; lua
(use-package lua-mode
  :straight t
  :hook (lua-mode . lsp-mode))

;; c/cpp
;; Note check here https://config.phundrak.com/emacs/packages/programming.html#caddy
;; https://github.com/emacs-exordium/exordium/blob/master/modules/init-cpp.el
(use-package cc-mode
  :straight nil
  :hook ((c++-mode . lsp-mode)
         (c++-mode . #'tree-sitter-hl-mode)
         ;; (c-mode . #'tree-sitter-hl-mode)
         )
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
)

;; (use-package cmake-mode
;;   :defer t
;;   :straight t
;;   :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
;;          ("\\.cmake\\'" . cmake-mode)))

;; (use-package modern-cpp-font-lock
;;   :if (eq exordium-enable-c++11-keywords :modern)
;;   :diminish modern-c++-font-lock-mode
;;   :hook (c++-mode . modern-c++-font-lock-mode)
;; )

;; python





;; https://github.com/copilot-emacs/copilot.el

;;;
