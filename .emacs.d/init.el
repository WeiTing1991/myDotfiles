;; init.el
;; Performance tweaks

;; The default is 800 kilobytes.  Measured in bytes.
(setq native-comp-deferred-compilation t)
(setq gc-cons-threshold (* 100 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar
(xterm-mouse-mode 1)        ; enable the mouse

;; Set up the visible bell
;; (setq visible-bell t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Enable relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

(load-theme 'wombat)

(defvar wt/default-font-size 140)
(defvar wt/default-variable-font-size 140)
(defvar wt/frame-transparency '(90 . 90))

;; Set the font
(set-face-attribute 'default nil :font "Hack Nerd Font" :height wt/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Hack Nerd Font" :height wt/default-font-size)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Hack Nerd Font" :height wt/default-variable-font-size :weight 'regular)

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

(load-file (expand-file-name "config.el" user-emacs-directory))
