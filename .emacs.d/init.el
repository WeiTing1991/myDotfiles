;;; init.el

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
(setq custom-file (locate-user-emacs-file "custon-vars.el"))
(load custom-file 'noerror 'nomessage)

(use-package gcmh
  :demand t
  :config
  (setq gcmh-low-cons-threshold (* 64 1024 1024))
  (gcmh-mode +1))


(setq frame-title-format '("WeiTing Emacs " emacs-version))
;; (load-theme 'modus-vivendi)

(use-package doom-themes
  :straight t
  :defer 0
  :config
    (setq doom-themes-enable-bold t)
    (setq doom-themes-enable-italic nil)
    (load-theme 'doom-one t)
    (set-face-background 'default "black")
  )

;;Font and background
(cond
  ;; macOS configuration
  ((eq system-type 'darwin)  ;; 'darwin' is for macOS
   (setq wt/default-font-size 140)
   (setq wt/default-variable-font-size 120)
   (setq wt/frame-transparency '(90 . 90))
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

(load-file (expand-file-name "./config/config.el" user-emacs-directory))
(load-file (expand-file-name "./config/core.el" user-emacs-directory))
;; (load-file (expand-file-name "./config/ui.el" user-emacs-directory))
;; (load-file (expand-file-name "./config/cmd-system.el" user-emacs-directory))
;; (load-file (expand-file-name "./config/file-system.el" user-emacs-directory))
;; (load-file (expand-file-name "./config/lsp.el" user-emacs-directory))
;; (load-file (expand-file-name "./config/terminals.el" user-emacs-directory))
;; (load-file (expand-file-name "./config/app.el" user-emacs-directory))


(provide 'init)
;;; init.el end here
