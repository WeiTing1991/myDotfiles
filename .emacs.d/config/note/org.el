;;; org.el

;;; code:
;; org mode
;; https://doc.norang.ca/org-mode.html

(require 'bella-base-color)

;; NOTE
;; https://github.com/minad/org-modern?tab=readme-ov-file
;; https://github.com/jakebox/jake-emacs
(defun wt/org-mode-setting ()
  "Custom org setttings"
  ;; it a bit slow in windows
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (auto-fill-mode 0)
  )
;; https://jethrokuan.github.io/org-roam-guide/
;; https://github.com/integral-dw/org-superstar-mode/blob/master/DEMO.org
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


;; set the font and size
(defun org-style-dark ()
  "custom style mode."
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.0)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (face-remap-add-relative (car face) :height (cdr face) :weight 'regular))
  ;; BUG
  ;; (cond
  ;;  ((eq system-type 'darwin)
  ;;  ((eq system-type 'windows-nt)
  ;;   (setq org-directory  "~/iCloudDrive/iCloud~md~obsidian/weitingchen/"))
  ;;  )

  ;; (face-remap-add-relative 'org-indent :background bella-color-black)
  ;; (face-remap-add-relative 'default :foreground bella-color-black :background bella-color-text-light)
  ;; (face-remap-add-relative 'org-block :background bella-color-base :inherit 'fixed-pitch)

  ;; (defvar bella-color-base "#232136")
  ;; (defvar bella-color-high "#393552")
  ;; (defvar bella-color-grey"#D8DEE9")

  (set-face-attribute 'org-block-end-line nil
                      :foreground bella-color-grey
                      :background bella-color-base
                      :weight 'bold
                      )
  (set-face-attribute 'org-block-begin-line nil
                      :foreground bella-color-grey
                      :background bella-color-base
                      :weight 'bold
                      )
  (set-face-attribute 'org-block nil :background bella-color-base   :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)
  )

(defun wt/toggle-org-markers ()
  "Toggle markers"
  (interactive)
  (if org-hide-emphasis-markers
      (progn
        (setq org-hide-emphasis-markers nil)
        (message "markers VISIBLE"))
    (progn
      (setq org-hide-emphasis-markers t)
      )
    (message " markers HIDDEN"))
  (save-buffer)
  (revert-buffer t t)
  )


;; nice bullets
(use-package org-bullets
  :straight t
  :defer t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org
  :straight t
  :config
  ;; indent tab-width
  (add-hook 'org-mode-hook
            (lambda ()
              ;; (setq-default tab-width 8)
              (setq-default indent-tabs-mode nil)
              (setq-local evil-auto-indent nil)
              (org-indent-mode)
              )
            )

  (setq org-ellipsis " ... ")
  (setq org-hide-emphasis-markers nil)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)


  (add-hook 'org-mode-hook 'wt/org-mode-setting)
  (add-hook 'org-mode-hook 'org-style-dark)
  ;; (add-hook 'org-mode-hook #'org-bella-bullet-keywords)

  (cond
   ((eq system-type 'darwin)
    (setq org-directory "/Users/weitingchen/Library/Mobile Documents/iCloud~md~obsidian/Documents/weitingchen/"))
   ((eq system-type 'windows-nt)
    (setq org-directory  "~/iCloudDrive/iCloud~md~obsidian/weitingchen/"))
   )
  ;; (setq org-agenda-files '((concat org-directory "weitingchen.org")))
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))

  )

;; set keybinding
(add-hook 'org-mode-hook
          (lambda ()
            (wt/leader-keys
              "m" '(:ignore :wk "org")
              "mm" '(org-emphasize :wk "org markers")
              "mte" #'(wt/toggle-org-markers :wk "org toggle markers")
              "mtl" '(org-toggle-link-display :wk "org toggle link")
              "RET" '(org-open-at-point :wk "org open link")
              )
            )
          )

;; TODO check those link
;;https://github.com/daviwil/emacs-from-scratch/blob/master/Emacs.org

(setq org-roam-directory
      (cond
       ((eq system-type 'darwin)
        "/Users/weitingchen/Library/Mobile Documents/iCloud~md~obsidian/Documents/weitingchen/")
       ((eq system-type 'windows-nt)
        "~/iCloudDrive/iCloud~md~obsidian/weitingchen/")))

(use-package org-roam
  :straight t
  :after org
  :custom
  (org-roam-directory (file-truename org-roam-directory))

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config

  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)
  )

(provide 'org)
;;; org.el ends here
