;;; org.el

;;; code:
;; org mode
;; https://doc.norang.ca/org-mode.html

;; TODO
(require 'bella-base-color)

(setq editorconfig-exclude-modes '(org-mode))

(defun org-custom-setting ()
  "Custom org setttings"
  (setq tab-width 8)
  (org-indent-mode)
  ;; it a bit slow in windows
  ;; (variable-pitch-mode 1)

  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  )

(defcustom org-bella-list
  '((?+ . "◦")
    (?- . "–")
    (?* . "•"))
  "List of bullet replacement strings. Set to nil to disable styling list bullets."
  :type '(alist :key-type character :value-type string))

(defun org-bella-bullet-keywords ()
  "Generate font-lock keywords for list bullets based on `org-modern-list`."
  (append
   (when-let ((bullet (alist-get ?+ org-bella-list)))
     `((,(format "^[ \t]*\\(+\\)[ \t]" ) 1 '(face nil display ,bullet))))
   (when-let ((bullet (alist-get ?- org-bella-list)))
     `((,(format "^[ \t]*\\(-\\)[ \t]") 1 '(face nil display ,bullet))))
   (when-let ((bullet (alist-get ?* org-bella-list)))
     `((,(format "^[ \t]*\\(*\\)[ \t]") 1 '(face nil display ,bullet)))))
  )

(defun org-bella-style ()
  "custom style mode"
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.0)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (face-remap-add-relative (car face) :height (cdr face) :weight 'regular :foreground bella-color-black))

  (face-remap-add-relative 'default :foreground bella-color-black :background bella-color-text-light)
  (face-remap-add-relative 'org-indent :background bella-color-text-light)
  ;; (load-theme 'modus-operandi)
  )

(use-package org
  :straight t
  :config
  (setq org-ellipsis " ▾")
  ;; (setq org-hide-emphasis-markers t)
  ;; (setq org-agenda-start-with-log-mode t)
  ;; (setq org-log-done 'time)
  ;; (setq org-log-into-drawer t)
  (add-hook 'org-mode-hook #'org-custom-setting)
  (add-hook 'org-mode-hook #'org-bella-style)
  )


;; (setq org-agenda-files '("~/org"))

(provide 'org)
;;;
