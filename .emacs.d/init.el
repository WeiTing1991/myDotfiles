;; Init.el

;; Performance tweaks
(setq gc-cons-threshold (* 500 1000 1000))

(if (boundp 'comp-deferred-compilation)
  (setq comp-deferred-compilation t)
  (setq native-comp-deferred-compilation t)
)
(setq load-prefer-newer noninteractive)

;; The default is 800 kilobytes.  Measured in bytes.
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)


;; disable the backup file 
;; (setq auto-save-default nil)
(setq make-backup-files nil)
;; (setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))
(setq create-lockfiles nil)

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


;; Gerenal setting
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar
(setq ring-bell-function 'ignore)

;;
;; basic font and frame setting

(cond
  ;; macOS configuration
  ((eq system-type 'darwin)  ;; 'darwin' is for macOS
  (setq wt/default-font-size 140)
  (setq wt/default-variable-font-size 140)
  (setq wt/frame-transparency '(95 . 95))
)
  ;; Windows configuration
  ((eq system-type 'windows-nt)  ;; 'windows-nt' is for Windows
  (setq  wt/default-font-size 100)
  (setq  wt/default-variable-font-size 100)
  (setq  wt/frame-transparency '(90 . 90))
  )
  )

;; Set the font
(set-face-attribute 'default nil :font "Hack Nerd Font" :height wt/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Hack Nerd Font" :height wt/default-font-size)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Hack Nerd Font" :height wt/default-variable-font-size :weight 'regular)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha wt/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,wt/frame-transparency))

;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))


;; Load the package
(load-file (expand-file-name "core.el" user-emacs-directory))
(load-file (expand-file-name "cmd-system.el" user-emacs-directory))
(load-file (expand-file-name "file-system.el" user-emacs-directory))
(load-file (expand-file-name "option.el" user-emacs-directory))
(load-file (expand-file-name "ui.el" user-emacs-directory))
(load-file (expand-file-name "lsp.el" user-emacs-directory))

;; dont save the custom change into init.el
(setq custom-file (locate-user-emacs-file "custon-vars.el"))
(load custom-file 'noerror 'nomessage)


(setq gc-cons-threshold (* 2 1000 1000))
