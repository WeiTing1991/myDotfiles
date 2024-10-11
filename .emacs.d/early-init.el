;;; early-init.el
(setq package-enable-at-startup nil
      inhibit-startup-message   t
      inhibit-startup-screen t
      )

;; Gerenal setting
(menu-bar-mode -1)                 ; Disable the menu bar

(scroll-bar-mode -1)               ; disable scrollbar
(horizontal-scroll-bar-mode -1)

(tool-bar-mode -1)                 ; disable toolbar
(tooltip-mode -1)                  ; disable tooltips
;; (set-fringe-mode 10)               ; give some breathing room


(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil
      read-process-output-max (* 10 1024 1024)
      bidi-inhibit-bpa t)

(defvar wt/gc-cons-threshold (* 256 1024 1024))

(add-hook 'emacs-startup-hook ; hook run after loading init files
          #'(lambda ()
              (setq gc-cons-threshold wt/gc-cons-threshold
                    gc-cons-percentage 0.1)))

(add-hook 'minibuffer-setup-hook #'(lambda ()
                                     (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook #'(lambda ()
                                    (garbage-collect)
                                    (setq gc-cons-threshold ian/gc-cons-threshold)))

(setq initial-frame-alist '((background-color . "#282828")))

(provide 'early-init)
;;; early-init.el ends here

