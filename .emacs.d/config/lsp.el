;;; lsp.el

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
  (setq lsp-keymap-prefix "C-l l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)

  ;; LSP Keybinding
  (evil-define-key 'normal 'global (kbd "gk") 'lsp-ui-doc-show)
  (evil-define-key 'normal 'global (kbd "gd") 'lsp-ui-peek-find-definitions)
  (evil-define-key 'normal 'global (kbd "gD") 'lsp-find-definition)
  (evil-define-key 'normal 'global (kbd "gr") 'lsp-find-references)
  (evil-define-key 'normal 'global (kbd "gI") 'lsp-goto-implementation)
  (evil-define-key 'normal 'global (kbd "C-j") 'lsp-treemacs-symbols)

  )

;; https://emacs-lsp.github.io/lsp-ui/
(use-package lsp-ui
  :hook ((prog-mode lsp-mode). lsp-ui-mode)
  :defer t
  :commands lsp-ui-mode
  :config
  (require 'lsp-ui-flycheck)
  (require 'lsp-ui-sideline)
  (setq lsp-ui-peek-always-show t
        lsp-ui-doc-enable t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-action t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-enable t)
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

(use-package company-box
  :after company
  :straight t
  :hook (company-mode . company-box-mode)
)
;; TODO
;; (use-package corfu
;;   :bind (:map corfu-map
;;               ("C-j" . corfu-next)
;;               ("C-k" . corfu-previous)
;;               ("TAB" . corfu-insert)
;;               ([tab] . corfu-insert)
;;               ("C-f" . corfu-insert))
;;   :custom
;;   (corfu-cycle t)
;;   (corfu-auto t)
;;   (corfu-preview-current nil)
;;   (corfu-quit-at-boundary t)
;;   (corfu-quit-no-match t)

;;   :config
;;   (global-corfu-mode 1)

;;   (defun corfu-enable-in-minibuffer ()
;;     "Enable Corfu in the minibuffer if `completion-at-point' is bound."
;;     (when (where-is-internal #'completion-at-point (list (current-local-map)))
;;       ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
;;       (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
;;                   corfu-popupinfo-delay nil)
;;       (corfu-mode 1)))

;;   (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
;;   )

;; ;; TODO
;; ;; Check the cape
;;
;; (use-package lsp-treemacs
;;   :after lsp-mode
;;   )
;;
;; ;; NOTE
;; ;; Syntax check as linter
;; ;; https://www.flycheck.org/en/latest/user/installation.html
;; ;; flyspell
;;

(use-package flycheck
  :straight (:build t)
  :config
  (add-hook 'prog-mode-hook #'global-flycheck-mode)
  (setq flycheck-display-errors-delay 0.2)
  ;; (setq flycheck-highlighting-mode 'lines)
  )


;; for Emacs Lisp
(with-eval-after-load 'flycheck
  '(flycheck-package-setup)
)


(use-package yasnippet-snippets
  :straight t
  )

(use-package yasnippet
  :defer t
  :straight t
  :init
  (yas-global-mode)
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :config
  (setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ;; "/path/to/some/collection/"           ;; foo-mode and bar-mode snippet collection
        ;; "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
        ))
  )

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1)
  )

(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("Python"     (ruff-format))
                  ;; ("" ())
                  )
  )
 )


;; lsp server setting
(load-file (expand-file-name "./config/lsp-config/md.el" user-emacs-directory))

;; lua
(use-package lua-mode
  :straight t
  :hook (lua-mode . lsp-deferred)
)

;;
;; c/cpp
;; Note check here https://config.phundrak.com/emacs/packages/programming.html#caddy
;; https://github.com/emacs-exordium/exordium/blob/master/modules/init-cpp.el
(use-package cc-mode
  :straight nil
  :hook ((c++-mode . lsp-deferred)
         (c++-mode . #'tree-sitter-hl-mode)
         ;; (c-mode . #'tree-sitter-hl-mode)
         )
  :config
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
)

;; ;; (use-package cmake-mode
;; ;;   :defer t
;; ;;   :straight t
;; ;;   :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
;; ;;          ("\\.cmake\\'" . cmake-mode)))
;;
;; ;; (use-package modern-cpp-font-lock
;; ;;   :if (eq exordium-enable-c++11-keywords :modern)
;; ;;   :diminish modern-c++-font-lock-mode
;; ;;   :hook (c++-mode . modern-c++-font-lock-mode)
;; ;; )

;;;
;; python
(use-package python-mode
  :straight nil
  :hook (python-mode . lsp-deferred)
  ;; :custom
  ;; (python-shell-interpreter "python3")
  )

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))  ; or lsp-deferred

;; (use-package elpy
;;   :straight t
;;   :defer t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable)
;;   )

(use-package pyvenv
  :straight t
  )

(use-package conda
  :straight t
  :config
  (setq conda-env-subdirectory ".env")
  )

;;

;; https://github.com/copilot-emacs/copilot.el

;;;
