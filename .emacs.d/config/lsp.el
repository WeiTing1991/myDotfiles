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
  (defun wt/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :custom
  (lsp-completion-provider :none) ;; we use corfu!
  :hook
  (lsp-completion-mode . wt/lsp-mode-setup-completion) ;; setup orderless completion style.

  :config
  (lsp-enable-which-key-integration t)

  ;; LSP Keybinding
  (evil-define-key 'normal 'global (kbd "gk") 'lsp-ui-doc-show)
  (evil-define-key 'normal 'global (kbd "gd") 'lsp-ui-peek-find-definitions)
  (evil-define-key 'normal 'global (kbd "gD") 'lsp-find-definition)
  (evil-define-key 'normal 'global (kbd "gr") 'lsp-find-references)
  (evil-define-key 'normal 'global (kbd "gI") 'lsp-goto-implementation)
  ;; (evil-define-key 'normal 'global (kbd "C-j") 'lsp-treemacs-symbols)
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

;; (use-package company
;;   :straight t
;;   :init
;;   (global-company-mode)
;;   :bind (:map company-active-map
;;               ("<tab>" . company-complete-selection))
;;   :config
;;   (setq company-minimum-prefix-length     1
;;         company-idle-delay                0.0
;;         company-toolsip-limit             5
;;         company-tooltip-align-annotations t
;;         company-tooltip-maximum-width      50
;;         company-tooltip-minimum-width      50
;;         company-tooltip-height            5
;;         company-require-match             'never
;;         company-selection-wrap-around     t
;;         company-tooltip-scroll-amount 10
;;         company-tooltip-scrollbar-width 0.0
;;         company-backends '((company-capf company-files company-dabbrev))
;;         )
;;   )

;; (use-package company-dict
;;   :straight t
;;   :defer t
;;   :config
;;   (setq company-dict-dir (expand-file-name "dicts" user-emacs-directory)))

;; (use-package company-box
;;   :straight t
;;   :defer t
;;   :hook (company-mode . company-box-mode)
;;   )

;; (use-package company-prescient
;;   :straight t
;;   :defer t
;;   :hook (company-mode . company-prescient-mode)
;;   )

;; TODO
;; https://github.com/minad/corfu
(use-package corfu
  :straight t
  :bind
  (:map corfu-map
        ("TAB" . corfu-insert)
        ("<tab>" . corfu-insert)
        ("RET" . nil)
        ;; ("TAB" . corfu-complete)
        ;; ("SPC" . corfu-insert-separator) ;; Insert a separator for multi-part completion
        ("<escape>" . corfu-quit))
  :custom
  ;;  disable for Insert
  ;; (define-key evil-insert-state-map (kbd "TAB") nil)
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 1)
  (corfu-auto-delay  0.05)
  (corfu-popupinfo-delay '(0.05 . 0.05))
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-min-width 1)
  (corfu-max-width 50)
  (corfu-count 10)
  (corfu-scroll-margin 4)
  ;; Have Corfu wrap around when going up
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect-first t)
  ;; ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current 'insert)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  ;; Enable Corfu
  (global-corfu-mode t)
  (corfu-history-mode t)
  (corfu-popupinfo-mode t)
  )

;; A few more useful configurations...
(use-package emacs
  :straight t
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;; (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package corfu-popupinfo
  :defer t
  :after corfu
  :straight nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  )

;; TODO
;;https://github.com/minad/cape
(use-package cape
  :straight t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("M-p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init

  ;; "p" #'completion-at-point
  ;; "t" #'complete-tag
  ;; "d" #'cape-dabbrev
  ;; "h" #'cape-history
  ;; "f" #'cape-file
  ;; "s" #'cape-elisp-symbol
  ;; "e" #'cape-elisp-block
  ;; "a" #'cape-abbrev
  ;; "l" #'cape-line
  ;; "w" #'cape-dict
  ;; "k"  'cape-keyword
  ;; ":"  'cape-emoji
  ;; "\\" 'cape-tex
  ;; "_"  'cape-tex
  ;; "^"  'cape-tex
  ;; "&"  'cape-sgml
  ;; "r"  'cape-rfc1345)

  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-abbrev)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history)
)

;; (use-package lsp-treemacs
;;   :straight t
;;   :defer t
;;   :after lsp-mode
;;   )

;; https://www.flycheck.org/en/latest/user/installation.html
(use-package flycheck
  :straight (:build t)
  :defer t
  :config
  (add-hook 'prog-mode-hook #'global-flycheck-mode)
  (setq flycheck-display-errors-delay 0.1)
  :custom
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  )
;; for Emacs Lisp
(with-eval-after-load 'flycheck
  '(flycheck-package-setup)
  )

;; TODO
;; check if this is working or not
(use-package yasnippet-snippets
  :straight t
  :defer t
  :after yasnippet
  )

(use-package yasnippet
  :straight t
  :defer t
  ;; :init (yas-global-mode 1)
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :config
  ;; (yas-reload-all)
  ;; (setq yas-snippet-dirs
  ;;       '("~/.emacs.d/snippets"                    ;; personal snippets
  ;;         ;; "/path/to/some/collection/"           ;; foo-mode and bar-mode snippet collection
  ;;         ;; "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
  ;;         ))
  )

(use-package yasnippet-capf
  :after cape
  :defer t
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package editorconfig
  :straight t
  :defer t
  :hook (prog-mode . editorconfig-mode)
  )

(use-package format-all
  :straight t
  :defer t
  :preface
  (defun wt/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (if (derived-mode-p 'prolog-mode)
        (prolog-indent-buffer)
      (format-all-buffer)))
  :config
  ;;TODO
  ;; (setq-default format-all-formatters
  ;;               '(
  ;;                 ("Python"     (ruff-format))
  ;;                 )
  ;;               )
  (require 'general)
  (wt/leader-keys
    "m" '(wt/format-code :wk "fomating")
    )
  (add-hook 'prog-mode-hook #'format-all-ensure-formatter)
  )

;; lsp server setting
(load-file (expand-file-name "./config/lsp-config/md.el" user-emacs-directory))
;; (load-file (expand-file-name "./config/lsp-config/cpplsp.el" user-emacs-directory))
;; (load-file (expand-file-name "./config/lsp-config/pythonlsp.el" user-emacs-directory))

;; lua
(use-package lua-mode
  :straight t
  :defer t
  :hook (lua-mode . lsp-deferred)
  )

;; c/cpp
;; Note check here https://config.phundrak.com/emacs/packages/programming.html#caddy
;; https://github.com/emacs-exordium/exordium/blob/master/modules/init-cpp.el

;; (use-package cc-mode
;;   :defer t
;;   :straight nil
;;   :hook ((c++-mode . lsp-deferred)
;;          (c++-mode . #'tree-sitter-hl-mode)
;;          ;; (c-mode . #'tree-sitter-hl-mode)
;;          )
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;;   )

;; (use-package cmake-mode
;;   :defer t
;;   :straight t
;;   :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
;;          ("\\.cmake\\'" . cmake-mode)))

;;https://github.com/ludwigpacifici/modern-cpp-font-lock
                                        ;(use-package modern-cpp-font-lock
                                        ;  :straight t
                                        ;  :defer t
                                        ;  :diminish modern-c++-font-lock-mode
                                        ;  :hook (c++-mode . modern-c++-font-lock-mode)
                                        ;)

;; python
;; (use-package python-mode
;;   :straight nil
;;   :defer t
;;   :hook (python-mode . lsp-deferred)
;;   ;; :custom
;;   ;; (python-shell-interpreter "python3")
;;   )

;; (use-package python-ts-mode
;;   :straight nil
;;   :defer t
;;   :hook (python-ts-mode . lsp-deferred)
;;   :custom
;;   (python-shell-interpreter "python3")
;;   ;; (when (eq system-type 'windows-nt)
;;   ;;   (setq python-shell-interpreter "C://Users//weitingche//anaconda//python.exe")
;;   ;;   )
;;   )

;; (use-package lsp-pyright
;;   :straight t
;;   :defer t
;;   :custom (lsp-pyright-langserver-command "pyright")
;;   :hook (python-ts-mode . (lambda ()
;;                             (require 'lsp-pyright)
;;                             (lsp-deferred))))

;; (use-package conda
;;   :straight t
;;   :defer t
;;   :hook(python-ts-mode . conda-env-autoactivate-mode)
;;   :config
;;   (conda-env-initialize-interactive-shells)
;;   (conda-env-initialize-eshell))

;; (use-package pyenv-mode
;;   :straight t
;;   :defer t
;;   :hook (python-mode .pyenv-mode)
;;   )

;; https://github.com/copilot-emacs/copilot.el

(provide 'lsp)

;;;
