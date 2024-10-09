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

;; auto compelte
;; TODO
;; https://company-mode.github.io/manual/
(use-package company
  :straight t
  :hook ((lsp-mode . company-mode)
         (emacs-lisp-mode))
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))

  :custom
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 1)
	(company-tooltip-limit 8)
  ;; (company-tooltip-align-annotations 't)
)

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)
)

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
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

(use-package treemacs
  :straight nil
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

;; lsp setting

;; markdown
(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
	)

;; (custom-set-faces!
;;   '(markdown-header-delimiter-face :foreground "#616161" :height 0.9)
;;   '(markdown-header-face-1 :height 1.8 :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face)
;;   '(markdown-header-face-2 :height 1.4 :foreground "#EBCB8B" :weight extra :Inherit Markdown-Header-FacE)
;;   '(Markdown-header-face-3 :height 1.2 :foreground "#D08770" :weight extra-bold :inherit markdown-header-face)
;;   '(markdown-header-face-4 :height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face)
;;   '(markdown-header-face-5 :height 1.1 :foreground "#b48ead" :weight bold :inherit markdown-header-face)
;;   '(markdown-header-face-6 :height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face))
;; )
;;  (defvar nb/current-line '(0 . 0)
;;    "(start . end) of current line in current buffer")
;;  (make-variable-buffer-local 'nb/current-line)
;;
;;  (defun nb/unhide-current-line (limit)
;;    "Font-lock function"
;;    (let ((start (max (point) (car nb/current-line)))
;;          (end (min limit (cdr nb/current-line))))
;;      (when (< start end)
;;        (remove-text-properties start end
;;                        '(invisible t display "" composition ""))
;;        (goto-char limit)
;;        t)))
;;
;;  (defun nb/refontify-on-linemove ()
;;    "Post-command-hook"
;;    (let* ((start (line-beginning-position))
;;           (end (line-beginning-position 2))
;;           (needs-update (not (equal start (car nb/current-line)))))
;;      (setq nb/current-line (cons start end))
;;      (when needs-update
;;        (font-lock-fontify-block 3))))
;;
;;  (defun nb/markdown-unhighlight ()
;;    "Enable markdown concealling"
;;    (interactive)
;;    (markdown-toggle-markup-hiding 'toggle)
;;    (font-lock-add-keywords nil '((nb/unhide-current-line)) t)
;;    (add-hook 'post-command-hook #'nb/refontify-on-linemove nil t))
;;
;;  (add-hook 'markdown-mode-hook #'nb/markdown-unhighlight)
;;
;; (defun markdown-view-mode-maybe ()
;;   (cond ((and (eq major-mode 'markdown-mode) buffer-read-only) (markdown-view-mode))
;;         ((and (eq major-mode 'markdown-view-mode) (not buffer-read-only)) (markdown-mode))))
;;
;; (add-hook 'read-only-mode-hook 'markdown-view-mode-maybe)
;;
;;  (if (equal major-mode 'markdown-view-mode)
;;    (local-set-key (kbd "C-x C-q") 'markdown-mode))
;;  (if (equal major-mode 'markdown-mode)
;;    (local-set-key (kbd "C-x C-q") 'markdown-view-mode))
;;
;; lua
(use-package lua-mode)


;; https://github.com/copilot-emacs/copilot.el
