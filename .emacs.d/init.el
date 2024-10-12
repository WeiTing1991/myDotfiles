;;; init.el
(defun wt/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'wt/display-startup-time)

;; disable the backup file
(setq make-backup-files nil)

(setq create-lockfiles nil)
;; (setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

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

;; (if (boundp 'comp-deferred-compilation)
;;   (setq comp-deferred-compilation t)
;;   (setq native-comp-deferred-compilation t)
;;   )

;;; GccEmacs (native-comp) stuff
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq native-comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)
    (setq load-prefer-newer noninteractive)
))


(use-package no-littering)
;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq auto-save-default t)
;; do not save the custom change into init.el
(setq custom-file (locate-user-emacs-file "custon-vars.el"))
(load custom-file 'noerror 'nomessage)


(load-file (expand-file-name "./config/config.el" user-emacs-directory))
(load-file (expand-file-name "./config/core.el" user-emacs-directory))
(load-file (expand-file-name "./config/ui.el" user-emacs-directory))
(load-file (expand-file-name "./config/cmd-system.el" user-emacs-directory))
(load-file (expand-file-name "./config/file-system.el" user-emacs-directory))
(load-file (expand-file-name "./config/terminals.el" user-emacs-directory))
(load-file (expand-file-name "./config/lsp.el" user-emacs-directory))


(use-package gcmh
  ;; :hook (emacs-startup-hook . gcmh-mode)
  :demand t
  :config
  (setq gcmh-low-cons-threshold (* 16 1024 1024))
  (gcmh-mode +1))

;;; init.el end here
