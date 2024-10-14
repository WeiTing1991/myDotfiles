;;; config.el

(use-package emacs
  :straight nil
  :preface
  (defvar wt/indent-width 2)
  (defalias 'yes-or-no-p 'y-or-n-p)

  :config
  ;; (setq initial-scratch-message "I am EMPTY.")
  (setq echo-keystrokes 0.01)

  (setq use-dialog-box nil)
  (global-visual-line-mode 1)
  (setq ring-bell-function 'ignore)

  (setq scroll-step 1)
  (setq scroll-margin 10)
  (setq scroll-conservatively 100000)
  (setq scroll-preserve-screen-position 'always)

  (setq default-directory "~/")

  ;; (set-window-margins nil 0 0)
  ;; (setq hscroll-step 1)
  ;; (setq hscroll-margin 1)
  ;; (setq auto-window-vscroll nil)
  ;; (setq auto-hscroll-mode nil)


  ;; (put 'scroll-right 'disabled nil)
  ;; (put 'scroll-left 'disabled nil)
  ;; (setq load-prefer-newer t)
  ;; ;; (setq inhibit-compacting-font-caches t)
  ;; ;; (setq kill-buffer-query-functions nil)
  ;; (setq delete-by-moving-to-trash t)
  ;; ;; (put 'downcase-region 'disabled nil
  ;; ;; (put 'upcase-region 'disabled nil)


  ;; Revert buffer
  ;; (recentf-mode 1)
  ;; (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)

  ;; Relative line numbers
  (setq display-line-numbers-type 'relative)
  (setq-default display-line-numbers-width 6)

  ;; (setq next-line-add-newlines t)

  (set-fringe-mode '(8 . 8))
  (set-default 'truncate-lines t)
  (pixel-scroll-precision-mode 1)

  ;; default editorconfig

  (electric-pair-mode 1)
  (electric-indent-mode 1)
  (setq comment-style 'indent)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default indent-width #'wt/indent-width)
  (setq-default standard-indent 2)        ;; Set standard indentation to 2 spaces

  (setq-default split-height-threshold  4
                split-width-threshold   160) ; the reasonable limit for horizontal splits

  ;; (setq max-specpdl-size 10000)
  ;; (setq max-lisp-eval-depth 10000)

  (setq jit-lock-stealth-time nil)
  (setq jit-lock-defer-time nil)
  (setq jit-lock-stealth-load 200)
  ;; (setq fast-but-imprecise-scrolling t)

  (setq xterm-mouse-mode +1)
  (context-menu-mode 1)

  ;; (wt/maybe-set-default-browser)

  ;; ignore some buffer

  )
(cond
  ;; macOS configuration
  ((eq system-type 'darwin)
    (setq switch-to-prev-buffer-skip-regexp "\*[^*]+\*")
   )
  ;; Windows configuration
  ((eq system-type 'windows-nt)
    (setq switch-to-prev-buffer-skip-regexp "\\*[^*]+\\*")
   )
  )
;; line-column
(setq-default display-fill-column-indicator-column 130)
;; (setq-default display-fill-column-indicator-character ?▏)
(setq-default display-fill-column-indicator-character ?┆)
;; (set-face-foreground 'fill-column-indicator "grey")
;; (global-display-line-numbers-mode 1)
;; (display-fill-column-indicator-mode 1)

(dolist (mode '(prog-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode 1)
                   (display-fill-column-indicator-mode 1)
                   ))
  )

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
