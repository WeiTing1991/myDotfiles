;; file search

;; may be check with ivy and consel
;; https://github.com/minad/vertico
;; check https://github.com/tumashu/vertico-posframe
(setq enable-recursive-minibuffers t)

(use-package vertico
  :straight t
  :diminish
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  (vertico-resize nil)
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :straight nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; https://github.com/minad/consult
(use-package consult
  :straight t
	;; :bind ()
  )      

(use-package marginalia
  :after vertico
  :straight t
  :config
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; https://github.com/oantolin/embark
(use-package embark
  :after vertico
  :straight t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
	 :map minibuffer-local-map
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
	 ("C-c C-c" . embark-collect)
	 ("C-c C-e" . embark-export))
)
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
)
