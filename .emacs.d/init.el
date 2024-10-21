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
;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; do not save the custom change into init.el
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)


;;Font and background
(cond
  ;; macOS configuration
  ((eq system-type 'darwin)  ;; 'darwin' is for macOS
   (setq wt/default-font-size 140)
   (setq wt/default-variable-font-size 120)
   (setq wt/frame-transparency '(85 . 90))
   )
  ;; Windows configuration
  ((eq system-type 'windows-nt)  ;; 'windows-nt' is for Windows
   (setq  wt/default-font-size 100)
   (setq  wt/default-variable-font-size 80)
   (setq  wt/frame-transparency '(95 . 90))
   )
  )

(add-to-list 'load-path "~/.emacs.d/nano/")

;; (require 'nano-layout)
;; (require 'nano-base-colors)
;; (require 'nano-faces)

;; Set the font
(set-face-attribute 'default nil :font "Hack Nerd Font" :height wt/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Hack Nerd Font" :height wt/default-variable-font-size)
(set-face-attribute 'variable-pitch nil :font "Hack Nerd Font" :height wt/default-variable-font-size)
;(setq nano-font-size (/ wt/default-font-size 10))

;; Set frame transparency
;(set-frame-parameter (selected-frame) 'alpha wt/frame-transparency)
;(add-to-list 'default-frame-alist `(alpha . ,wt/frame-transparency))

(use-package base16-theme
  :straight t
:defer 0
:config
(load-theme 'base16-onedark)
)

;(use-package zenburn-theme
;  :straight t
;  :defer 0
  ;; :config
  ;; (load-theme 'zenburn t)
  ;)

;(use-package nord-theme
;  :straight t
;  :defer 0
;  )
  ;; (load-theme 'nord t)

;; disabled nano
;; (require 'nano-theme)
;; (require 'nano-theme-dark)
;; (require 'nano-theme-light)
;; (nano-theme-set-dark)
;; (call-interactively 'nano-refresh-theme)

;(use-package lambda-themes
;  :straight (:type git :host github :repo "lambda-emacs/lambda-themes")
;  :custom
;  (lambda-themes-set-italic-comments t)
;  (lambda-themes-set-italic-keywords t)
;  (lambda-themes-set-variable-pitch t)
;  :config
;  ;; load preferred theme
;  (load-theme 'lambda-dark)
;  )

;; (use-package modus-themes
;;   :straight t
;;   :config
;;   ;; load preferred theme
;;   (load-theme 'modus-vivendi)
;;   )

;; (set-face-background 'default "#0D0907")
;; (set-face-background 'fringe "#0D0907")
;; (set-face-attribute 'line-number-current-line nil :foreground "light grey" )

(add-to-list 'load-path "~/.emacs.d/config/")

;; (require 'nano-defaults)
;; (require 'config)

;; (load-file (expand-file-name "./config/core.el" user-emacs-directory))
;; (load-file (expand-file-name "./config/ui.el" user-emacs-directory))
;; (load-file (expand-file-name "./config/cmd-system.el" user-emacs-directory))
;; (load-file (expand-file-name "./config/file-system.el" user-emacs-directory))
;; (load-file (expand-file-name "./config/lsp.el" user-emacs-directory))
;; (load-file (expand-file-name "./config/app.el" user-emacs-directory))

(load-file (expand-file-name "./config/terminals.el" user-emacs-directory))

(defun my-persp-format ()
  "Return the name of the current perspective."
  (if (persp-mode)
      (format "[%s]" (persp-name (persp-curr)))
    ""))

(setq frame-title-format '("WeiTingEmacs v" emacs-version "     "
                           (:eval (my-persp-format))
                           "  "
                           " %b"))

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

(provide 'init)
;;; init.el ends here
