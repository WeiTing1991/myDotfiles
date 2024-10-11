;;; config.el

;; (defun ian/maybe-set-default-browser ()
;;   "When in WSL Emacs, open links in default Windows 11 browser."
;;   (cond
;;    ((eq system-type 'gnu/linux)
;;     (when (string-match "Linux.*microsoft.*Linux"
;;                         (shell-command-to-string "uname -a"))
;;       (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
;;             browse-url-generic-args '("/c" "start" "")
;;             browse-url-browser-function 'browse-url-generic)))))

(defun wt/split-and-follow-vertically ()
  "Split window vertically (below)."
  (interactive)
  (split-window-below)
  (other-window 1))
(defun wt/split-and-follow-horizontally ()
  "Split window horizontally (right)."
  (interactive)
  (split-window-right)
  (other-window 1))

(use-package emacs
  :straight nil
  :preface
  (defvar wt/indent-width 2)
  (defalias 'yes-or-no-p 'y-or-n-p)
  :config
  (setq user-full-name "Wei-Ting Chen")
  (setq frame-title-format '("Emacs " emacs-version))

  (setq initial-scratch-message "")
  (setq echo-keystrokes 0.05)
  (setq require-final-newline t)
  ;; ;; (setq use-dialog-box nil)
  ;; ;; (global-visual-line-mode t)
  ;;
  ;; (setq ring-bell-function 'ignore)
  ;; (setq-default default-directory "~/")
  ;;
  ;; (setq auto-window-vscroll nil)
  ;; (setq frame-resize-pixelwise t)
  ;;
  (setq scroll-margin 50)
  (setq scroll-conservatively 101)
  (setq scroll-preserve-screen-position t)
  
  ;; (when (>= emacs-major-version 29)
  ;;   (pixel-scroll-precision-mode 1))
  ;; ;;
  ;; (put 'scroll-right 'disabled nil)
  ;; (put 'scroll-left 'disabled nil)
  ;;
  ;; (setq hscroll-step 0)
  ;; (setq scroll-step 1)
  ;; (setq hscroll-margin 0)
  ;; (setq load-prefer-newer t)
  ;;
  ;; ;; (setq inhibit-compacting-font-caches t)
  ;; ;; (setq kill-buffer-query-functions nil)
  ;; (setq delete-by-moving-to-trash t)
  ;; ;; (put 'downcase-region 'disabled nil)
  ;; ;; (put 'upcase-region 'disabled nil)
  ;;
  ;;
  ;; Revert buffer
  (recentf-mode 1)
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)

  ;; Relative line numbers
  (setq display-line-numbers-type 'relative)
  (global-display-line-numbers-mode t)

  ;; default editorconfig
  ; Indentation settings
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default indent-width #'wt/indent-width)
  (setq-default standard-indent 2)        ;; Set standard indentation to 2 spaces


  ;;; Fix annoying vertical window splitting.
  ;;; https://lists.gnu.org/archive/html/help-gnu-emacs/2015-08/msg00339.html
  ;; (with-eval-after-load "window"
  ;;   (defcustom split-window-below nil
  ;;     "If non-nil, vertical splits produce new windows below."
  ;;     :group 'windows
  ;;     :type 'boolean)
  ;;   (defcustom split-window-right nil
  ;;     "If non-nil, horizontal splits produce new windows to the right."
  ;;     :group 'windows
  ;;     :type 'boolean)
  ;;   (fmakunbound #'split-window-sensibly)
  ;;   (defun split-window-sensibls
  ;;       (&optional window)
  ;;     (setq window (or window (selected-window)))
  ;;     (or (and (window-splittable-p window t)
  ;;              ;; Split window horizontally.
  ;;              (split-window window nil (if split-window-right 'left  'right)))
  ;;         (and (window-splittable-p window)
  ;;              ;; Split window vertically.
  ;;              (split-window window nil (if split-window-below 'above 'below)))
  ;;         (and (eq window (frame-root-window (window-frame window)))
  ;;              (not (window-minibuffer-p window))
  ;;              ;; If WINDOW is the only window on its frame and is not the
  ;;              ;; minibuffer window, try to split it horizontally disregarding the
  ;;              ;; value of `split-width-threshold'.
  ;;              (let ((split-width-threshold 0))
  ;;                (when (window-splittable-p window t)
  ;;                  (split-window window nil (if split-window-right
  ;;                                               'left
  ;;                                             'right))))))))

  ;; (setq-default split-height-threshold  4
  ;;               split-width-threshold   160) ; the reasonable limit for horizontal splits
  ;;
  ;; (setq max-specpdl-size 10000)
  ;; (setq max-lisp-eval-depth 10000)
  ;; (set-default 'truncate-lines t)
  ;; ;; (wt/maybe-set-default-browser)
  ;; (setq jit-lock-defer-time 0)
  ;; (setq fast-but-imprecise-scrolling t)
  ;; (xterm-mouse-mode +1)

  )

;; basic font and frame setting
(cond
  ;; macOS configuration
  ((eq system-type 'darwin)  ;; 'darwin' is for macOS
   (setq wt/default-font-size 140)
   (setq wt/default-variable-font-size 140)
   (setq wt/frame-transparency '(90 . 90))
   )
  ;; Windows configuration
  ((eq system-type 'windows-nt)  ;; 'windows-nt' is for Windows
   (setq  wt/default-font-size 100)
   (setq  wt/default-variable-font-size 100)
   (setq  wt/frame-transparency '(95 . 90))
   )
  )

;; Set the font
(set-face-attribute 'default nil :font "Hack Nerd Font" :height wt/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Hack Nerd Font" :height wt/default-variable-font-size)
;; Set the variable pitch face
;; (set-face-attribute 'variable-pitch nil :font "Times New Rome" :height wt/default-variable-font-size :weight 'regular)

;; Set frame transparency
;; (set-frame-parameter (selected-frame) 'alpha wt/frame-transparency)
;; (add-to-list 'default-frame-alist `(alpha . ,wt/frame-transparency))
;; (add-hook 'minibuffer-setup-hook
;;           (lambda ()
;;             (set-frame-parameter nil 'alpha '(100 . 100)))) ;; Ensure fully opaque

;; line-column
(setq-default display-fill-column-indicator-column 130)
(setq-default display-fill-column-indicator-character ?|)
(set-face-foreground 'fill-column-indicator "grey")

(add-hook 'prog-mode-hook (lambda ()
  (display-fill-column-indicator-mode)))

;; ;; disable in cetain mode
;; (dolist (mode '(org-mode-hook
;;                 term-mode-hook
;;                 vterm-mode-hook
;;                 shell-mode-hook
;;                 treemacs-mode-hook
;;                 eshell-mode-hook
;;                 dired-mode-hook)
;;               )
;;   (add-hook mode (lambda ()
;;                       (display-fill-column-indicator-mode -1)
;;                       (display-line-numbers-mode 0)
;;                       )
;;             )
;;   )

(use-package doom-themes
  :straight t
  :custom-face
  (region                         ((t (:extend nil))))
  (highlight-symbol-face          ((t (:background "#355266" :distant-foreground "#bbbbbb"))))
  (highlight                      ((t (:foreground "#4db2ff" :background nil :underline t)))) ; link hover
  (link                           ((t (:foreground "#3794ff"))))
  (evil-ex-substitute-replacement ((t (:strike-through nil))))
  (vertical-border                ((t (:foreground "black" :background "black"))))
  (fringe                         ((t (:background nil))))
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic nil)
  (load-theme 'doom-palenight t))

;;
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
  :init
  (setq whitespaces-cleanup-mode 0)
  :hook (whitespaces-cleanup-mode . prog-mode)
  )

(setq-default
	whitespace-style '(face tabs tab-mark spaces space-mark trailing newline newline-mark)
	whitespace-display-mappings '(
		(space-mark   ?\     [?\u00B7]     [?.])
		(space-mark   ?\xA0  [?\u00A4]     [?_])
		;; (newline-mark ?\n    [182 ?\n])
		(tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t]))
	whitespace-line-column 140
	)

;; (add-hook 'prog-mode-hook (lambda ()
;;   (whitespace-mode)))

;;; config code end here









