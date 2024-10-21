;;; config.el

(use-package emacs
  :straight nil
  :preface
  (defvar wt/indent-width 2)
  (defalias 'yes-or-no-p 'y-or-n-p)

  :config

  (setq echo-keystrokes 0.01)

  (setq use-file-dialog nil)
  (setq use-dialog-box nil)
  ;; No message in scratch buffer
  (setq initial-scratch-message nil)

  ;; Initial buffer
  (setq initial-buffer-choice nil)
  ;; No frame title
  (setq frame-title-format nil)

  ;; No popup windows
  (setq pop-up-windows nil)

  ;; No empty line indicators
  (setq indicate-empty-lines nil)

  ;; No cursor in inactive windows
  (setq cursor-in-non-selected-windows nil)

  ;; Text mode is initial mode
  (setq initial-major-mode 'text-mode)

  ;; Text mode is default major mode
  (setq default-major-mode 'text-mode)

;; Moderate font lock
(setq font-lock-maximum-decoration nil)

;; No limit on font lock
(setq font-lock-maximum-size nil)

;; No line break space points
(setq auto-fill-mode nil)

(setq fill-column 100)

;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; Completion style, see
;; gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
(setq completion-styles '(basic substring))

;; Use RET to open org-mode links, including those in quick-help.org
(setq org-return-follows-link t)

  (global-visual-line-mode 1)
  (setq ring-bell-function 'ignore)

  (setq scroll-step 1)
  (setq scroll-margin 10)
  (setq scroll-conservatively 100000)
  (setq scroll-preserve-screen-position 'always)

  (setq default-directory "~/")

  ;; Revert buffer
  (recentf-mode 1)
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)

  ;; Relative line numbers
  (setq display-line-numbers-type 'relative)
  (setq-default display-line-numbers-width 6)

  ;; (setq next-line-add-newlines t)

  (set-fringe-mode '(8 . 8))
  (set-default 'truncate-lines t)
  ;; (pixel-scroll-precision-mode 1)

  ;; default editorconfig

  (show-paren-mode 1)
  (electric-pair-mode 1)
  ;; (electric-indent-mode 1)
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

;; Mouse active in terminal
  (unless (display-graphic-p)
    (xterm-mouse-mode 1)
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

  (context-menu-mode 1)

  ;; (wt/maybe-set-default-browser)


  )

;; No scroll bars
(if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))

;; No toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; No menu bar
(if (display-graphic-p)
    (menu-bar-mode t) ;; When nil, focus problem on OSX
  (menu-bar-mode -1))

;; Tab behavior
;; (setq tab-always-indent 'complete)
;; (global-company-mode)
;; (define-key company-mode-map [remap indent-for-tab-command]
;;   #'company-indent-or-complete-common)

;; Pixel scroll (as opposed to char scrool)
;; (pixel-scroll-mode t)

;; Mac specific
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen t
        mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier nil
        mac-use-title-bar nil))

;; Make sure clipboard works properly in tty mode on OSX
;; (defun copy-from-osx ()
;;   (shell-command-to-string "pbpaste"))
;; (defun paste-to-osx (text &optional push)
;;   (let ((process-connection-type nil))
;;     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;;       (process-send-string proc text)
;;       (process-send-eof proc))))
;; (when (and (not (display-graphic-p))
;;            (eq system-type 'darwin))
;;     (setq interprogram-cut-function 'paste-to-osx)
;;     (setq interprogram-paste-function 'copy-from-osx))


;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; Minimum window height
(setq window-min-height 1)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")


;; buffer ignore
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

(provide 'config)
;;;
