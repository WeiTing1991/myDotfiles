;;; early-init.el

;; (setq native-comp-async-report-warnings-errors 'silent)

(setq gc-cons-threshold (* 512 1024 1024))

;; ;; Reset the garbage collection threshold after startup
;; (add-hook 'emacs-startup-hook
;;          (lambda ()
;;            (setq gc-cons-threshold (* 2 1024 1024))))

(setq package-enable-at-startup nil
      inhibit-startup-message t
      inhibit-startup-screen t
      )

;; Disable unnecessary UI elements
(menu-bar-mode -1)                 ; Disable the menu bar
(scroll-bar-mode -1)               ; Disable scrollbar
(tool-bar-mode -1)                 ; Disable toolbar
(tooltip-mode -1)                  ; Disable tooltips
(setq inhibit-startup-message t)   ; Disable startup message
(setq inhibit-startup-screen t)    ; Disable startup screen
(fringe-mode 0)                    ; Disable fringes


(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil
      read-process-output-max (* 64 1024 1024)
      bidi-inhibit-bpa t)

(defvar wt/gc-cons-threshold (* 1024 1024 1024))

(add-hook 'emacs-startup-hook ; hook run after loading init files
          #'(lambda ()
              (setq gc-cons-threshold wt/gc-cons-threshold
                    gc-cons-percentage 0.1)))

(add-hook 'minibuffer-setup-hook
          #'(lambda ()
             (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'minibuffer-exit-hook
          #'(lambda ()
             (garbage-collect)
             (setq gc-cons-threshold wt/gc-cons-threshold)))

;; Display startup time in the messages buffer
(defun wt/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'wt/display-startup-time)

;;; Native compilation settings
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  ;; (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-deferred-compilation t)
  (setq package-native-compile t)
  (setq load-prefer-newer noninteractive)
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Disable backup files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(setq initial-frame-alist '((background-color . "black")))

(provide 'early-init)
;;; early-init.el ends here
