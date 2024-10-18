;;; cmd-system
;; file search
(use-package hydra)

;; https://github.com/minad/vertico
(use-package vertico
  :bind (:map vertico-map
         ("C-n" . vertico-next)
         ("C-p" . vertico-previous)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :init
  (vertico-mode)
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-directory-up t)
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
                                          (lambda (orig cand prefix suffix index _start)
                                            (setq cand (funcall orig cand prefix suffix index _start))
                                            (concat
                                             (if (= vertico--index index)
                                                 (propertize ">> " 'face 'vertico-current)
                                               "  ")
                                             cand)))
  )

(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;; Do not allow the cursor in the minibuffer prompt
  ;; (setq minibuffer-prompt-properties
  ;;       '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  )

(use-package vertico-directory
  :after vertico
  :straight nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  )

;; https://github.com/tumashu/vertico-posframe
(use-package vertico-posframe
  :after vertico
  :straight t
  :config
  (vertico-posframe-mode 1)
)

;; Enable vertico-multiform
;; (vertico-multiform-mode 1)
;; (setq vertico-multiform-commands
;;       '((find-file reverse)
;;         (execute-extended-command reverse)
;;         )
;;       )

;; ;; Use a buffer with indices for imenu
;; ;; and a flat (Ido-like) menu for M-x.
;; (setq vertico-multiform-commands
;;       '((consult-imenu buffer indexed)
;;         (execute-extended-command unobtrusive)))

;; (setq vertico-multiform-categories
;;       '((file reverse)
;;         (find-file)
;;         (execute-extended-command)
;;         ))


;; ;; Sort directories before files
;; (defun sort-directories-first (files)
;;   (setq files (vertico-sort-history-length-alpha files))
;;   (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
;;          (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

;; ;;TODO check this
;; https://github.com/minad/consult
(use-package consult
  :straight t
  :bind (;; C-c bindings in `mode-specific-map'
         ;; ("C-c M-x" . consult-mode-command)
         ;; ("C-c h" . consult-history)
         ;; ("C-c k" . consult-kmacro)
         ;; ("C-c m" . consult-man)
         ;; ("C-c i" . consult-info)
         ;; ([remap Info-search] . consult-info)
         ;; ;; C-x bindings in `ctl-x-map'
         ;; ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ;; ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ;; ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ;; ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ;; ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)
         ;; ;; Other custom bindings
         ;; ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; ;; M-g bindings in `goto-map'
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ;; ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;; ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ;; ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ("C-M-l" . consult-imenu)
         ;; ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ;; ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ;; ("M-s c" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ;; ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; ;; Isearch integration
         ;; ("M-s e" . consult-isearch-history)
         ;; :map isearch-mode-map
         ;; ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ;; ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ;; ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ;; ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("C-s" . consult-history)                 ;; orig. next-matching-history-element
         ("C-r" . consult-history)                 ;; orig. previous-matching-history-element
         )

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.5 any)
   )
  ;; custom fd, grep, and rg
  (setq consult-fd-args '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
                          "--full-path --color=never --hidden"
                          "--exclude" ".git"))
  (setq consult-grep-args '("grep" (consult--grep-exclude-args)
      "--null --line-buffered --hidden --color=never --ignore-case\
      --with-filename --line-number -I -r"))

  (setq consult-ripgrep-args
    "rg --null --hidden --line-buffered --color=never --max-columns=1000 --path-separator /\
    --smart-case --no-heading --with-filename --line-number --search-zip"
    )
)

;; https://github.com/minad/marginalia
(use-package marginalia
  :after vertico
  :straight t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-align 'right)
  :init
  (marginalia-mode)
 )

;; faster sorting
(use-package vertico-prescient
             :straight t
             :after consult
             :config
             (vertico-prescient-mode 1)
             ;; (setq prescient-sort-length-enable nil)
             ;; (setq prescient-filter-method '(literal regexp fuzzy))
             (prescient-persist-mode 1)
             )

;; TODO Check the oderless
;; https://kristofferbalintona.me/posts/202202211546/
