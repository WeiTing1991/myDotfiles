;;; config.el

;;; code
(use-package emacs
  :demand t
  :straight nil

  :init
  (defvar wt/indent-width 2)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq enable-recursive-minibuffers t)

  (setq backup-by-copying t)

  (setq sentence-end-double-space nil)

  (setq frame-inhibit-implied-resize t) ;; useless for a tiling window manager

  (setq show-trailing-whitespace t) ;; self-explanatory

  (setq user-full-name "WeiTingChen") ;; my details
  ;; (setq user-mail-address "patrick.d.elliott@gmail.com")

  ;; Don't persist a custom file
  (setq custom-file (make-temp-file "")) ; use a temp file as a placeholder
  (setq custom-safe-themes t)            ; mark all themes as safe, since we can't persist now
  (setq enable-local-variables :all)     ; fix =defvar= warnings

  (setq delete-by-moving-to-trash t) ;; use trash-cli rather than rm when deleting files.

  ;; less noise when compiling elisp
  ;; (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  ;; (setq native-comp-async-report-warnings-errors nil)
  ;; (setq load-prefer-newer t)

  ;; Hide commands in M-x which don't work in the current mode
  ;; (setq read-extended-command-predicate #'command-completion-default-include-p)

  :config
  (setq echo-keystrokes 0.01)
  ;; No popup
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
  (setq-default display-line-numbers-width 5)


  ;; (fringe-mode 0)
  (set-fringe-mode '(5 . 5))
  (set-default 'truncate-lines t)

  ;; default editorconfig
  (show-paren-mode 1)

  (electric-pair-mode 1)
  ;; (setq electric-pair-preserve-balance nil)

  (setq comment-style 'indent)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default indent-width #'wt/indent-width)
  (setq-default standard-indent 2)        ;; Set standard indentation to 2 spaces

  (setq-default split-height-threshold  4
                split-width-threshold   160) ; the reasonable limit for horizontal splits

  (setq jit-lock-stealth-time nil)
  (setq jit-lock-defer-time nil)
  (setq jit-lock-stealth-load 200)

  ;; Mouse active in terminal
  (unless (display-graphic-p)
    (xterm-mouse-mode 1)
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line))
  (context-menu-mode 1)

  )

;; double check the mode
;; No scroll bars
(if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))
;; No toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; No menu bar

;; Pixel scroll (as opposed to char scrool)
(pixel-scroll-mode t)

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

(set-charset-priority 'unicode) ;; utf8 everywhere

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

;; (cond
;;  ((string-equal system-type "windows-nt")
;;   (progn
;;     (setq default-process-coding-system '(utf-8-dos . utf-8-dos))))
;;  ((string-equal system-type "gnu/linux")
;;   (progn
;;     (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))))

(defun wt/encoding/dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix 't))

(defun wt/file/remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

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
(setq-default display-fill-column-indicator-column 120)
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
