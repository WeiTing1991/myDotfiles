;;; init.el ---   -*- lexical-binding: t -*-

;;; code:
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
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el" 'silent 'inhibit-cookies)
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

;; Define a variable to hold the operating system type
(defconst wt-os-type
  (cond
   ((eq system-type 'windows-nt) 'windows)
   ((eq system-type 'darwin) 'mac)
   ((eq system-type 'gnu/linux) 'linux)
   (t 'unknown))
  )

(defun wt/set-font-and-background ()
  ;; Font and background
  (cond
   ((eq wt-os-type 'mac)
    (defvar wt-default-font-size 140)
    (defvar wt-default-variable-font-size 140)
    (defvar wt-frame-transparency '(98 . 90))

    (set-face-attribute 'fixed-pitch nil :font "SauceCodePro Nerd Font" :height wt-default-variable-font-size :weight 'regular)
    (set-face-attribute 'variable-pitch nil :font "SauceCodePro Nerd Font" :height wt-default-variable-font-size :weight 'regular)

    )
   ((eq wt-os-type 'linux)
    (defvar  wt-default-font-size 140)
    (defvar  wt-default-variable-font-size 140)
    (defvar  wt-frame-transparency '(95 . 90))
    (setq inhibit-compacting-font-caches 1)

    (set-frame-font "Hack Nerd Font-18" nil t)
    (set-face-attribute 'fixed-pitch nil :font "Hack Nerd Font" :height wt-default-variable-font-size :weight 'regular)
    (set-face-attribute 'variable-pitch nil :font "Hack Nerd Font" :height wt-default-variable-font-size :weight 'regular)

    )
   )
   ;; (set-face-attribute 'default nil :height wt-default-font-size :weight 'medium)
   ;; (set-frame-parameter (selected-frame) 'alpha wt-frame-transparency)
   ;; (add-to-list 'default-frame-alist `(alpha . ,wt-frame-transparency))

  ;; Set the font
  ;; (set-face-attribute 'default nil :font "RobotoMono Nerd Font" :height wt-default-font-size :weight 'medium)

  ;; Set transparency
  ;; (set-frame-parameter (selected-frame) 'alpha wt-frame-transparency)
  ;; (add-to-list 'default-frame-alist `(alpha . ,wt-frame-transparency))

  )

;; load theme
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/colors/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(require 'bella-base-color)
(use-package gruvbox-theme
  :straight t
  :demand t
  )

(defun wt/set-dark-theme ()
  "Set wt dark theme."
  (load-theme 'gruvbox-dark-hard t)
  ;; (set-face-background 'default bella-color-black)
  ;; (set-face-background 'fringe bella-color-black)
  (set-face-background 'internal-border (face-background 'default))

  ;; (set-face-attribute 'line-number nil :background bella-color-black)
  ;; (set-face-attribute 'line-number-current-line nil :back bella-color-black )
  (wt/set-font-and-background)
  )


(defun wt/set-light-theme ()
  "Set wt light theme."
  (load-theme 'modus-operandi)
  (set-face-background 'default bella-color-white)
  (set-face-background 'fringe bella-color-white)

  (set-face-attribute 'line-number nil :background bella-color-white )
  ;; (set-face-attribute 'line-number-current-line nil :foreground bella-color-white)
  (wt/set-font-and-background)

  )


(defun wt/toggle-theme ()
  "Toggle the theme."
  (interactive)
  (cond ((eq (car custom-enabled-themes) 'gruvbox-dark-hard)
         (mapc #'disable-theme custom-enabled-themes)
         (wt/set-light-theme)
         )
        ((eq (car custom-enabled-themes) 'modus-operandi)
         (mapc #'disable-theme custom-enabled-themes)
         (wt/set-dark-theme)
         )
        )
  (save-buffer)
  (revert-buffer t t)
  )

                                        ; load theme and font
(wt/set-font-and-background)
(wt/set-dark-theme)

;; toggle theme
(global-set-key (kbd "C-c t") 'wt/toggle-theme)

;; Fall back font for glyph missing in Roboto
;; (defface fallback '((t :family "Roboto"
;;                        :inherit 'nano-face-faded)) "Fallback")

;; TODO check??
;; Fix bug on OSX in term mode & zsh (spurious % after each command)

;; (require 'disp-table)
;; (add-hook 'term-mode-hook
;;           (lambda () (setq buffer-display-table (make-display-table))))
;; (setq x-underline-at-descent-line t)

;; (set-display-table-slot standard-display-table 'truncation
;;                         (make-glyph-code ?… 'fallback))
;; (set-display-table-slot standard-display-table 'wrap
;;                         (make-glyph-code ?↩ 'fallback))

;; Vertical window divider
(setq window-divider-default-right-width 1)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; No ugly button for checkboxes
(setq widget-image-enable nil)


(add-to-list 'load-path "~/.emacs.d/config/")

(require 'wt-base)
(require 'wt-core)
(require 'wt-ui)
(require 'wt-cmd-system)
(require 'wt-file-system)
(require 'wt-term)
(require 'wt-lsp)
(require 'wt-prog)
;; (require 'wt-app)
;; (require 'wt-org)
(require 'wt-git-tool)

;; TODO
;; (require 'wt-md)

;; (add-to-list 'default-frame-alist '(undecorated . t))
(setq frame-title-format
      '("wtEmacs v1.00-" emacs-version " - " "%b")
      )

;; for debug
;; (setq garbage-collection-messages t)
(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold (* 512 1024 1024))
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
