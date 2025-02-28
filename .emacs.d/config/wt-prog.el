;;; wt-prog.el --- programming setting.| -*- lexical-binding: t -*-

;;; Code:
;; NOTE https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-prog.el

;; (use-package prog-mode
;;   :straight nil
;;   :hook (prog-mode . prettify-symbols-mode)
;;   :init
;;   (setq prettify-symbols-unprettify-at-point 'right-edge)
;;   )

;; Show function arglist or variable docstring
(use-package eldoc
  :straight nil
  :diminish
  :config
  (when (childframe-workable-p)
    (use-package eldoc-box
      :straight t
      :after eldoc
      :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
      :custom
      (eldoc-box-lighter nil)
      (eldoc-box-only-multi-line t)
      (eldoc-box-clear-with-C-g t)
      :custom-face
      (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
      (eldoc-box-body ((t (:inherit tooltip))))
      :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
      :config
      ;; Prettify `eldoc-box' frame
      (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
            (alist-get 'right-fringe eldoc-box-frame-parameters) 8))))

;; Cross-referencing
(use-package xref
  :straight nil
  :bind (("C-c gd" . xref-find-definitions)
         ("C-c gb" . xref-go-back))
  :init
  ;; Use faster search tool
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))

  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read))

;; TODO:
;; https://www.flycheck.org/en/latest/user/installation.html
;; (define-fringe-bitmap 'wt-flycheck-error-icon
;;   (vector #b00000000
;;           #b00011000  ;; Row 1
;;           #b00111100  ;; Row 2
;;           #b00111100  ;; Row 3
;;           #b00011000  ;; Row 4
;;           #b00000000  ;; Row 5
;;           #b00000000  ;; Row 6
;;           #b00000000  ;; Row 7
;;           #b00000000  ;; Row 8
;;           #b00000000  ;; Row 9
;;           #b00000000  ;; Row 10
;;           #b00000000  ;; Row 11
;;           #b00000000  ;; Row 12
;;           #b00000000  ;; Row 13
;;           #b00000000  ;; Row 14
;;           #b00000000)) ;; Row 15

(use-package flycheck
  :straight (:build t)
  :defer t
  :hook((prog-mode emacs-lisp-mode) . flycheck-mode)
  :config
  (setq flycheck-display-errors-delay 0.1)

  ;;TODO how to change the symbol
  ;; ;; Show indicators in the left margin
  (setq-default flycheck-indication-mode 'left-margin)
  (setq flycheck-highlighting-sy 'symbols)

  ;; ;; Adjust margins and fringe widths…
  ;; (defun my/set-flycheck-margins ()
  ;;   (setq left-fringe-width 15 right-fringe-width 15
  ;;         left-margin-width 15 right-margin-width 0)
  ;;   (flycheck-refresh-fringes-and-margins))

  ;; (flycheck-redefine-standard-error-levels
  ;;  '((error   :fringe-bitmap 'wt-flycheck-error-icon :fringe-face 'flycheck-fringe-error)
  ;;    (warning :fringe-bitmap 'wt-flycheck-error-icon :fringe-face 'flycheck-fringe-warning)
  ;;    (info    :fringe-bitmap 'wt-flycheck-error-icon :fringe-face 'flycheck-fringe-info)))

  (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode)

  ;; :custom
  ;; (add-hook 'prog-mode-hook #'flycheck-mode)
  ;; (add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  )

(use-package editorconfig
  :straight t
  :defer t
  :hook (prog-mode . editorconfig-mode)
  )

(use-package format-all
  :straight t
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
  (define-key evil-normal-state-map (kbd "M-m") 'wt/format-code)
  (add-hook 'prog-mode-hook #'format-all-ensure-formatter)
  )

;; Yet another snippet extension
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

;; Collection of yasnippet snippets
(use-package yasnippet-snippets
  :straight t
  :defer t
  :after yasnippet
  )

(use-package yasnippet-capf
  :straight t
  :defer t
  :after cape
  :init
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  )

(provide 'wt-prog)
;;; wt-prog.el ends here
