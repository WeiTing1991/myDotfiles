;; opt.el

(recentf-mode 1)

;; Revert buffer
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers 1)

;; Relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(setq use-dialog-box nil)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
								vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
								dired-mode-hook)
							)
  (add-hook mode (lambda () (display-line-numbers-mode 0)))
)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

;; default editorconfig
; Indentation settings
(setq-default tab-width 2)         ;; Equivalent to 'set tabstop'
(setq-default evil-shift-width 2)  ;; Equivalent to 'set shiftwidth'
(setq standard-indent 2)

(electric-pair-mode 1)
;; indeent mode
(electric-indent-mode -1)

;; whitespace
(setq-default
	whitespace-style '(face tabs tab-mark spaces space-mark trailing newline newline-mark)
	whitespace-display-mappings '(
		(space-mark   ?\     [?\u00B7]     [?.])
		(space-mark   ?\xA0  [?\u00A4]     [?_])
		;; (newline-mark ?\n    [182 ?\n])
		(tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t]))
	whitespace-line-column 120
	)

(add-hook 'prog-mode-hook (lambda ()
  (whitespace-mode)))

;; Searching settings
;; (setq search-highlight t)
;; (setq case-fold-search t)   ;; Equivalent to 'set ignorecase'
;; (setq evil-ex-search-case 'smart) ;; Equivalent to 'set smartcase'

;; Display settings
(setq-default display-fill-column-indicator-column 120)
(setq-default display-fill-column-indicator-character ?|)
(set-face-foreground 'fill-column-indicator "grey")

(add-hook 'prog-mode-hook (lambda ()
  (display-fill-column-indicator-mode)))

(dolist (mode '(org-mode-hook
                term-mode-hook
								vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
								dired-mode-hook)
							)
  (add-hook mode (lambda () (display-fill-column-indicator-mode -1))))

;; Centering cursor while jumping or searching
(defun center-after-scroll ()
  (evil-scroll-line-to-center (line-number-at-pos)))
(advice-add 'evil-next-line :after #'center-after-scroll)
(advice-add 'evil-previous-line :after #'center-after-scroll)


;; zoom in and out 
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Toggle between split windows and a single window
(defun toggle-windows-split()
  (interactive)
  (if (not(window-minibuffer-p (selected-window)))
      (progn
        (if (< 1 (count-windows))
            (progn
              (window-configuration-to-register ?u)
              (delete-other-windows))
          (jump-to-register ?u))))
  (my-iswitchb-close))

(define-key global-map (kbd "C-`") 'toggle-windows-split)
