;; Init.el

;; Performance tweaks
(setq gc-cons-threshold (* 500 1000 1000))

(if (boundp 'comp-deferred-compilation)
  (setq comp-deferred-compilation t)
  (setq native-comp-deferred-compilation t)
)
(setq load-prefer-newer noninteractive)
(setq native-comp-async-report-warnings-errors nil)

;; The default is 800 kilobytes.  Measured in bytes.
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)


;; disable the backup file
(setq auto-save-default t)
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

;;
;; basic font and frame setting
(cond
  ;; macOS configuration
  ((eq system-type 'darwin)  ;; 'darwin' is for macOS
   (setq wt/default-font-size 140)
   (setq wt/default-variable-font-size 140)
   (setq wt/frame-transparency '(95 . 90))
   )
  ;; Windows configuration
  ((eq system-type 'windows-nt)  ;; 'windows-nt' is for Windows
   (setq  wt/default-font-size 100)
   (setq  wt/default-variable-font-size 100)
   (setq  wt/frame-transparency '(85 . 60))
   )
  )

;; Set the font
(set-face-attribute 'default nil :font "Hack Nerd Font" :height wt/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Hack Nerd Font" :height wt/default-font-size)
;; Set the variable pitch face
;; (set-face-attribute 'variable-pitch nil :font "Times New Rome" :height wt/default-variable-font-size :weight 'regular)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha wt/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,wt/frame-transparency))

;; Set inital frame size
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Load the package
(load-file (expand-file-name "opts.el" user-emacs-directory))
(load-file (expand-file-name "core.el" user-emacs-directory))
(load-file (expand-file-name "cmd-system.el" user-emacs-directory))
(load-file (expand-file-name "file-system.el" user-emacs-directory))
(load-file (expand-file-name "ui.el" user-emacs-directory))
(load-file (expand-file-name "lsp.el" user-emacs-directory))
(load-file (expand-file-name "terminals.el" user-emacs-directory))

;; do not save the custom change into init.el
(setq custom-file (locate-user-emacs-file "custon-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq gc-cons-threshold (* 2 1000 1000))

