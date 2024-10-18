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

(setq frame-title-format '("WeiTing Emacs " emacs-version))
;; (load-theme 'modus-vivendi)

;;Font and background
(cond
  ;; macOS configuration
  ((eq system-type 'darwin)  ;; 'darwin' is for macOS
   (setq wt/default-font-size 140)
   (setq wt/default-variable-font-size 120)
   (setq wt/frame-transparency '(95 . 90))
   )
  ;; Windows configuration
  ((eq system-type 'windows-nt)  ;; 'windows-nt' is for Windows
   (setq  wt/default-font-size 100)
   (setq  wt/default-variable-font-size 80)
   (setq  wt/frame-transparency '(95 . 90))
   )
  )

;; Set the font
(set-face-attribute 'default nil :font "Hack Nerd Font" :height wt/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Hack Nerd Font" :height wt/default-variable-font-size)
(set-face-attribute 'variable-pitch nil :font "Hack Nerd Font" :height wt/default-variable-font-size)

;; line spacling
;; (setq-default line-spacing 0.12)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha wt/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,wt/frame-transparency))

(use-package base16-theme
  :straight t
  :defer t
  :config
  (load-theme 'base16-default-dark t))

(use-package nano-theme
  :straight (nano-theme :type git :host github :repo "rougier/nano-theme")
  :config
  (load-theme 'nano t))

(load-file (expand-file-name "./config/config.el" user-emacs-directory))
(load-file (expand-file-name "./config/core.el" user-emacs-directory))
(load-file (expand-file-name "./config/ui.el" user-emacs-directory))
(load-file (expand-file-name "./config/cmd-system.el" user-emacs-directory))
(load-file (expand-file-name "./config/file-system.el" user-emacs-directory))
(load-file (expand-file-name "./config/app.el" user-emacs-directory))
;; (load-file (expand-file-name "./config/terminals.el" user-emacs-directory))
;; (load-file (expand-file-name "./config/lsp.el" user-emacs-directory))


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

(provide 'init)
;;; init.el ends here
