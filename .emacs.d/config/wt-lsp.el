;;; wt-lsp.el --- lsp setting.| -*- lexical-binding: t -*-

;;; code:
;; treesitter

(use-package treesit-auto
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  )

(use-package treesit
  :straight nil
  :defer t
  :hook (
         (bash-ts-mode
          c-ts-mode
          c++-ts-mode
          python-ts-mode
          go-ts-mode
          js-ts-mode
          json-ts-mode
          html-ts-mode
          css-ts-mode
          dockerfile-ts-mode
          rust-ts-mode
          typescript-ts-mode
          tsx-ts-mode
          yaml-ts-mode) . lsp-deferred)
  :init
  (setq treesit-font-lock-level 4
        treesit-language-source-alist
        '(
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (java "https://github.com/tree-sitter/java-tree-sitter")

          (json "https://github.com/tree-sitter/tree-sitter-json")
          (html "https://github.com/tree-sitter/tree-sitter-html")

          (lua "https://github.com/Azganoth/tree-sitter-lua")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")

          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (js ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          )
        )
  )

(with-eval-after-load 'lsp-mode
  (setq major-mode-remap-alist
        '(
          (bash-mode . bash-ts-mode)
          (cc-mode . c-ts-mode)
          (cc-mode . c++-ts-mode)
          (python-mode . python-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          )
        )
  )

;; https://emacs-lsp.github.io/lsp-mode/page/installation/
;;; lsp server
(use-package lsp-mode
  :straight t
  :defer t
  :commands (lsp lsp-deferred)
  :hook (
         (prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (lsp-deferred))))
         (lsp-completion-mode . wt/lsp-mode-setup-completion)
         ((markdown-mode yaml-mode yaml-ts-mode) . lsp-deferred)
         )

  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references))
  :preface
  ;; Performace tuning
  ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance/
  ;; (setq read-process-output-max (* 1024 1024)) ; 1MB
  ;; for some lanugage issue
  ;; (setenv "LSP_USE_PLISTS" "false")

  :custom
  (lsp-completion-provider :none) ;; we use corfu!

  :init
  (defun wt/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  (setq
   ;; lsp-use-plists t
   lsp-keymap-prefix "C-c l"
   lsp-keep-workspace-alive nil
   lsp-signature-auto-activate nil

   lsp-modeline-code-actions-enable nil
   lsp-modeline-diagnostics-enable nil
   lsp-modeline-workspace-status-enable nil

   lsp-semantic-tokens-enable t
   lsp-progress-spinner-type 'progress-bar-filled

   lsp-headerline-breadcrumb-enable t
   lsp-headerline-breadcrumb-enable-diagnostics nil
   ;; lsp-headerline-breadcrumb-enable-symbol-numbers t
   lsp-headerline-breadcrumb-icons-enable t
   lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)

   lsp-enable-file-watchers nil
   lsp-enable-folding t
   lsp-enable-symbol-highlighting nil

   lsp-enable-text-document-color nil

   lsp-enable-indentation nil
   lsp-enable-on-type-formatting nil

   ;; For diagnostics
   lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode)

   ;; For clients
   ;; lsp-clients-python-library-directories '("/usr/local/" "/usr/")
   )

  :config
  (lsp-enable-which-key-integration t)
  ;; (set-face-attribute 'header-line nil :foreground "white" :background "black" :weight 'bold)

  ;; (custom-set-faces
  ;;  '(lsp-headerline-breadcrumb-project-prefix-face
  ;;    ((t :background "black" :underline nil))))

  ;; (custom-set-faces
  ;;  '(lsp-headerline-breadcrumb-path-face ((t :underline nil :background "black" :inherit lsp-headerline-breadcrumb-path-face)))

  ;; (use-package consult-lsp
  ;;   :bind (:map lsp-mode-map
  ;;          ("C-M-." . consult-lsp-symbols)))

  (with-no-warnings
    ;; Emacs LSP booster
    ;; @seee https://github.com/blahgeek/emacs-lsp-booster
    (when (executable-find "emacs-lsp-booster")
      (defun lsp-booster--advice-json-parse (old-fn &rest args)
        "Try to parse bytecode instead of json."
        (or
         (when (equal (following-char) ?#)
           (let ((bytecode (read (current-buffer))))
             (when (byte-code-function-p bytecode)
               (funcall bytecode))))
         (apply old-fn args)))
      (advice-add (if (progn (require 'json)
                             (fboundp 'json-parse-buffer))
                      'json-parse-buffer
                    'json-read)
                  :around
                  #'lsp-booster--advice-json-parse)

      (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
        "Prepend emacs-lsp-booster command to lsp CMD."
        (let ((orig-result (funcall old-fn cmd test?)))
          (if (and (not test?)                             ;; for check lsp-server-present?
                   (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                   lsp-use-plists
                   (not (functionp 'json-rpc-connection))  ;; native json-rpc
                   (executable-find "emacs-lsp-booster"))
              (progn
                (message "Using emacs-lsp-booster for %s!" orig-result)
                (cons "emacs-lsp-booster" orig-result))
            orig-result)))
      (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))
    )
  )


;; https://emacs-lsp.github.io/lsp-ui/
;; TODO: check the posframe
(use-package lsp-ui
  :straight t
  :defer t
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :hook ((prog-mode lsp-mode). lsp-ui-mode)
  :commands lsp-ui-mode
  :bind (("C-c lu" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ;; ("M-<f6>" . lsp-ui-hydra/body)
         ;; ("s-<return>" . lsp-ui-sideline-apply-code-actions)
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         )
  :init
  (require 'lsp-ui-flycheck)
  (require 'lsp-ui-sideline)
  (setq lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-symbol nil
        lsp-ui-doc-delay 0.1
        ;; lsp-ui-doc-show-with-cursor (not (display-graphic-p))
        lsp-ui-imenu-auto-refresh 'after-save
        lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                              ,(face-foreground 'font-lock-string-face)
                              ,(face-foreground 'font-lock-constant-face)
                              ,(face-foreground 'font-lock-variable-name-face)))
  (setq lsp-ui-doc-position 'at-point)
  ;; Set correct color to borders
  (defun my-lsp-ui-doc-set-border ()
    "Set the border color of lsp doc."
    (setq lsp-ui-doc-border
          (if (facep 'posframe-border)
              (face-background 'posframe-border nil t)
            (face-background 'region nil t))))
  (my-lsp-ui-doc-set-border)
  (add-hook 'after-load-theme-hook #'my-lsp-ui-doc-set-border t)

  :config
  (setq lsp-ui-peek-enable t)
  ;; TODO:
  (with-no-warnings
    ;; Display peek in child frame if possible
    ;; @see https://github.com/emacs-lsp/lsp-ui/issues/441
    (defvar lsp-ui-peek--buffer nil)
    (defun lsp-ui-peek--peek-display (fn src1 src2)
      (if (childframe-workable-p)
          (-let* ((win-width (frame-width))
                  (lsp-ui-peek-list-width (/ (frame-width) 2))
                  (string (-some--> (-zip-fill "" src1 src2)
                            (--map (lsp-ui-peek--adjust win-width it) it)
                            (-map-indexed 'lsp-ui-peek--make-line it)
                            (-concat it (lsp-ui-peek--make-footer)))))
            (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
            (posframe-show lsp-ui-peek--buffer
                           :string (mapconcat 'identity string "")
                           :min-width (frame-width)
                           :internal-border-color (face-background 'posframe-border nil t)
                           :internal-border-width 1
                           :poshandler #'posframe-poshandler-frame-center))
        (funcall fn src1 src2)))

    (defun lsp-ui-peek--peek-destroy (fn)
      (if (childframe-workable-p)
          (progn
            (when (bufferp lsp-ui-peek--buffer)
              (posframe-hide lsp-ui-peek--buffer))
            (setq lsp-ui-peek--last-xref nil))
        (funcall fn)))
    (advice-add #'lsp-ui-peek--peek-new :around #'lsp-ui-peek--peek-display)
    (advice-add #'lsp-ui-peek--peek-hide :around #'lsp-ui-peek--peek-destroy)

    ;; Handle docs
    (defun my-lsp-ui-doc--handle-hr-lines nil
      (let (bolp next before after)
        (goto-char 1)
        (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
          (when (get-text-property next 'markdown-hr)
            (goto-char next)
            (setq bolp (bolp)
                  before (char-before))
            (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
            (setq after (char-after (1+ (point))))
            (insert
             (concat
              (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
              (propertize "\n" 'face '(:height 0.5))
              (propertize " "
                          ;; :align-to is added with lsp-ui-doc--fix-hr-props
                          'display '(space :height (1))
                          'lsp-ui-doc--replace-hr t
                          'face `(:background ,(face-foreground 'font-lock-comment-face nil t)))
              ;; :align-to is added here too
              (propertize " " 'display '(space :height (1)))
              (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))
    (advice-add #'lsp-ui-doc--handle-hr-lines :override #'my-lsp-ui-doc--handle-hr-lines)
    )
    ;; LSP Keybinding
    (evil-define-key 'normal 'global (kbd "gk") 'lsp-ui-doc-show)
    (evil-define-key 'normal 'global (kbd "gd") 'lsp-ui-peek-find-definitions)
    (evil-define-key 'normal 'global (kbd "gD") 'lsp-find-definition)
    (evil-define-key 'normal 'global (kbd "gr") 'lsp-find-references)
    (evil-define-key 'normal 'global (kbd "gI") 'lsp-goto-implementation)
    ;; (evil-define-key 'normal 'global (kbd "C-j") 'lsp-treemacs-symbols)
    )


;; TODO
;; https://github.com/minad/corfu
;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-completion.el
(use-package corfu
  :straight t
  :bind
  (:map corfu-map
        ("TAB" . corfu-insert)
        ("<tab>" . corfu-insert)
        ("RET" . nil)
        ;; ("TAB" . corfu-complete)
        ;; ("SPC" . corfu-insert-separator) ;; Insert a separator for multi-part completion
        ("<escape>" . corfu-quit)
        ("C-c" . corfu-quit))
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
  :defer t
  :after corfu
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c c" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
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
  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-hook 'completion-at-point-functions #'cape-file)
  ;; (add-hook 'completion-at-point-functions #'cape-abbrev)
  ;; (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)

  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)

  )

;; TODO https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-lsp.el
;; (use-package lsp-treemacs
;;   :straight t
;;   :defer t
;;   :after lsp-mode
;;   )

;; lanugage
(with-eval-after-load 'lsp-mode
  (add-to-list 'load-path (expand-file-name "./config/lsp/" user-emacs-directory))

  ;; lua
  (use-package lua-mode
    :straight t
    :defer t
    :hook (lua-mode . lsp-deferred)
    )

  ;; python
  (require 'wt-python)
  ;; (require 'wt-cpp)

)

(provide 'wt-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wt-lsp.el ends here
