;;; wt-base.el ---   -*- lexical-binding: t -*-

;;; code
(use-package emacs
  :demand t
  :straight nil
  :init
  (defvar wt-indent-width 2)
  (defalias 'yes-or-no-p 'y-or-n-p)

  (setq enable-recursive-minibuffers t)

  (setq user-full-name "WeiTingChen") ;; my details
  ;; (setq user-mail-address "weitingchen@gmail.com")

  ;; (setq sentence-end-double-space nil)
  ;; (setq show-trailing-whitespace t) ;; self-explanatory

  (setq enable-local-variables :all)     ; fix =defvar= warnings
  (setq delete-by-moving-to-trash t) ;; use trash-cli rather than rm when deleting files.

  :config

  (setq load-prefer-newer t)
  (setq echo-keystrokes 0.01)

  (setq default-directory "~/")

  ;; Initial buffer
  (setq initial-buffer-choice nil)

  ;; No frame title
  (setq frame-title-format nil)

  ;; No popup windows
  ;; (setq pop-up-windows nil)
  ;; No empty line indicators
  ;; (setq indicate-empty-lines nil)
  ;; No cursor in inactive windows
  (setq cursor-in-non-selected-windows nil)

  ;; Text mode is initial mode
  (setq initial-major-mode 'text-mode)
  ;; Text mode is default major mode
  (setq default-major-mode 'text-mode)

  ;; Moderate font lock
  ;; will change the syntax highlihght
  ;; (setq font-lock-maximum-decoration nil)
  ;; No limit on font lock
  (setq font-lock-maximum-size nil)

  ;; No line break space points
  (setq auto-fill-mode t)
  (setq fill-column 100)
  (setq confirm-nonexistent-file-or-buffer nil)

  ;; Completion style
  ;; gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
  (setq completion-styles '(basic substring))

  ;; Use RET to open org-mode links, including those in quick-help.org
  ;; (setq org-return-follows-link t)

  (global-visual-line-mode 1)
  (setq ring-bell-function 'ignore)

  (setq scroll-step 1)
  (setq scroll-margin 10)
  (setq scroll-conservatively 100000)
  (setq scroll-preserve-screen-position 'always)

  ;; Revert buffer
  (recentf-mode 1)
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)

  ;; Relative line numbers
  (setq display-line-numbers-type 'relative)
  (setq-default display-line-numbers-width 5)

  ;; wrap the long line
  (set-default 'truncate-lines t)

  ;; default editorconfig
  (show-paren-mode 1)

  (electric-pair-mode 0)
  (electric-indent-mode 0)
  ;; (setq electric-pair-preserve-balance nil)

  (setq comment-style 'indent)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default indent-width #'wt-indent-width)
  (setq-default standard-indent 2)

  (setq-default split-height-threshold  160 split-width-threshold   5) ; the reasonable limit for horizontal splits

  (setq jit-lock-stealth-time nil)
  (setq jit-lock-defer-time nil)
  (setq jit-lock-stealth-load 200)

  ;; Mouse active in terminal
  (unless (display-graphic-p)
    (xterm-mouse-mode 1)
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line))
  (context-menu-mode 1)

  (pixel-scroll-mode t)

  )

;; double check the mode
;; No scroll bars
;; (if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))
;; ;; No toolbar
;; (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Mac specific
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen t
  mac-option-key-is-meta nil
  mac-command-key-is-meta t
  mac-command-modifier 'meta
  mac-option-modifier nil
  mac-use-title-bar nil))

(set-charset-priority 'unicode)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)
(set-selection-coding-system 'utf-8)
(set-locale-environment "en_US.UTF-8")

(setq locale-coding-system 'utf-8
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)

(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; do I need this
(defun wt/encoding-dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix 't))

(defun wt/file-remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; buffer ignore
(cond
  ((eq wt-os-type 'mac)
    (setq switch-to-prev-buffer-skip-regexp "\*[^*]+\*")
   )
  ((eq wt-os-type 'window)
    (setq switch-to-prev-buffer-skip-regexp "\\*[^*]+\\*")
   )
  )

(provide 'wt-base)
;;; wt-base.el ends here
