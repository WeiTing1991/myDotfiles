;;; init.el ---   -*- lexical-binding: t -*-

;;; code
;; straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(use-package no-littering)
;; no-littering doesn't set this by default so we must plac
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; do not save the custom change into init.el
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; (add-to-list 'default-frame-alist '(undecorated-round . t))

(use-package base16-theme
  :straight t
  :defer t
  )

(use-package zenburn-theme
  :straight t
  :defer t
  )

(use-package nord-theme
  :straight t
  :demand t
  )

(defun wt/set-custom-theme ()
  "Set my theme."
  ;;(load-theme 'zenburn t)
  ;;(load-theme 'base16-onedark t)
  (load-theme 'nord t)
  (set-face-background 'default "#0D0907")
  (set-face-background 'fringe "#0D0907")
  )

(wt/set-custom-theme)

;; Font and background
(cond ;; macOS configuration
 ((eq system-type 'darwin)  ;; 'darwin' is for macOS
  (defvar wt/default-font-size 140)
  (defvar wt/default-variable-font-size 140)
  (defvar wt/frame-transparency '(98 . 90))
  )
 ;; Windows configuration
 ((eq system-type 'windows-nt)  ;; 'windows-nt' is for Windows
  (defvar  wt/default-font-size 100)
  (defvar  wt/default-variable-font-size 100)
  (defvar  wt/frame-transparency '(95 . 90))
  )
 )

;; Set the font
(when (display-graphic-p)
  (set-frame-font "RobotoMono Nerd Font" nil t))

(cond
 ((eq system-type 'windows-nt)
  (setq inhibit-compacting-font-caches 1)
  (set-face-attribute 'default nil :font "RobotoMono Nerd Font" :height wt/default-font-size :weight 'medium)
  ;;  windows system Can`t read it
  (set-face-attribute 'fixed-pitch nil :font "SauceCodePro NF" :height wt/default-variable-font-size :weight 'regular)
  (set-face-attribute 'variable-pitch nil :font "SauceCodePro NF" :height wt/default-variable-font-size :weight 'regular)
  )
 ((eq system-type 'darwin)
  (set-face-attribute 'default nil :font "RobotoMono Nerd Font" :height wt/default-font-size :weight 'medium)
  (set-face-attribute 'fixed-pitch nil :font "SauceCodePro Nerd Font" :height wt/default-variable-font-size :weight 'regular)
  (set-face-attribute 'variable-pitch nil :font "SauceCodePro Nerd Font" :height wt/default-variable-font-size :weight 'regular)
  )
 )

(set-frame-parameter (selected-frame) 'alpha wt/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,wt/frame-transparency))

;; (add-to-list 'load-path "~/.emacs.d/nano/")
;; check ?
(require 'disp-table)

;; Fall back font for glyph missing in Roboto
(defface fallback '((t :family "Roboto"
                       :inherit 'nano-face-faded)) "Fallback")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'fallback))

;; Fix bug on OSX in term mode & zsh (spurious % after each command)
;; (add-hook 'term-mode-hook
;;           (lambda () (setq buffer-display-table (make-display-table))))
;; (setq x-underline-at-descent-line t)

;; Vertical window divider
(setq window-divider-default-right-width 1)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

;; (set-face-attribute 'line-number nil :background "#0D0907" )
;; (set-face-attribute 'line-number-current-line nil :foreground "light grey" )

(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/theme/")

(require 'config)

(load-file (expand-file-name "./config/core.el" user-emacs-directory))
(load-file (expand-file-name "./config/ui.el" user-emacs-directory))
(load-file (expand-file-name "./config/cmd-system.el" user-emacs-directory))
(load-file (expand-file-name "./config/file-system.el" user-emacs-directory))
(load-file (expand-file-name "./config/terminals.el" user-emacs-directory))
(load-file (expand-file-name "./config/lsp.el" user-emacs-directory))
(load-file (expand-file-name "./config/app.el" user-emacs-directory))


;; set title
(setq frame-title-format
      '("Emacs v" emacs-version "  " "%b"
        )
      )

;; (setq garbage-collection-messages t) ; for debug
(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold (* 32 1024 1024))
  (setq gc-cons-percentage 0.1)
  (garbage-collect))

(run-with-idle-timer 1.0 nil #'my-cleanup-gc)

(defun wt/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'wt/display-startup-time)

;; Disable backup files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; ;; keep backup and save files in a dedicated directory
;; (setq backup-directory-alist
;;   `((".*" . ,(concat user-emacs-directory "backups")))
;;   auto-save-file-name-transforms
;;   `((".*" ,(concat user-emacs-directory "backups") t)))

(provide 'init)
;;; init.el ends here
