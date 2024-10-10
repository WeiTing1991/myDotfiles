;; Option.el

;; Theme
(use-package doom-themes
             :ensure t
             :config
             (setq doom-themes-enable-bold t
                   doom-themes-enable-italic nil)
             (load-theme 'doom-palenight t)
             ;; Enable flashing mode-line on errors
             (doom-themes-visual-bell-config)
             ;; (setq doom-themes-treemacs-theme "doom-atom")
             ;; (doom-themes-treemacs-config)
             ;; (doom-themes-org-config)
             )

;; (Custom-set-faces
;;   '(default ((t (:background "#0D0907"))))
;;   )

;; Gerenal setting
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq require-final-newline t)

(prefer-coding-system 'utf-8)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(horizontal-scroll-bar-mode -1)
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(setq use-dialog-box nil)
;;
(global-visual-line-mode t)

(menu-bar-mode -1)          ; Disable the menu bar
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq echo-keystrokes .1)

;; NOTE delay
;; smooth scrolling
;; (when (>= emacs-major-version 29)
;;   (pixel-scroll-precision-mode 1))

;; Revert buffer
(recentf-mode 1)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; default editorconfig
; Indentation settings
(setq-default tab-width 2)         ;; Equivalent to 'set tabstop'
(setq-default tab-width 2 indent-tabs-mode nil)         ;; Equivalent to 'set tabstop'
(setq-default evil-shift-width 2)  ;; Equivalent to 'set shiftwidth'
(setq standard-indent 2)

(setq scroll-margin 50)               ;; Keep 5 lines above/below the cursor
(setq scroll-conservatively 10000)   ;; Keep the cursor in the same position
(setq scroll-preserve-screen-position t)  ;; Maintain screen position

;; (use-package centered-cursor-mode
;;   :demand
;;   :config
;;   ;; Optional, enables centered-cursor-mode in all buffers.
;;   (global-centered-cursor-mode))


;; code vim style fold
(add-hook 'prog-mode-hook #'hs-minor-mode)
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "<tab>") 'hs-toggle-hiding)
;; (define-key evil-normal-state-map (kbd "zc") 'hs-hide-block)
;; (define-key evil-normal-state-map (kbd "zo") 'hs-show-block)
;; (define-key evil-normal-state-map (kbd "zr") 'hs-show-all)
;; (define-key evil-normal-state-map (kbd "zm") 'hs-hide-all)
  )

;; whitespace
(use-package whitespace-cleanup-mode
  :straight t
  :config
  :hook (prog-mode , whitespaces-cleanup-mode)
  )
(add-hook 'ruby-mode-hook 'whitespace-cleanup-mode)
;; delete becasue the slow
;; (add-hook 'before-save-hook '(lambda()
;;                                (when (not (or (derived-mode-p 'markdown-mode)
;;                                               (derived-mode-p 'org-mode)))
;;                                 (delete-trailing-whitespace))))

(setq-default
	whitespace-style '(face tabs tab-mark spaces space-mark trailing newline newline-mark)
	whitespace-display-mappings '(
		(space-mark   ?\     [?\u00B7]     [?.])
		(space-mark   ?\xA0  [?\u00A4]     [?_])
		;; (newline-mark ?\n    [182 ?\n])
		(tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t]))
	whitespace-line-column 120
	)
;; (add-hook 'prog-mode-hook (lambda ()
;;   (whitespace-mode)))

;; line-column
(setq-default display-fill-column-indicator-column 130)
(setq-default display-fill-column-indicator-character ?|)
(set-face-foreground 'fill-column-indicator "grey")

(add-hook 'prog-mode-hook (lambda ()
  (display-fill-column-indicator-mode)))

;; disable in cetain mode
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
                dired-mode-hook)
              )
  (add-hook mode (lambda ()
                      (display-fill-column-indicator-mode -1)
                      (display-line-numbers-mode 0)
                      )
            )
  )
