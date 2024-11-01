;;; org.el

;;; code:
;; org mode
;; https://doc.norang.ca/org-mode.html

(require 'bella-base-color)

;; NOTE
;; https://github.com/minad/org-modern?tab=readme-ov-file
;; https://github.com/jakebox/jake-emacs
(defun wt/org-mode-setting ()
  "Custom org settings."
  ;; it a bit slow in windows
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (auto-fill-mode 0)
  )

;; set the font and size
(defun wt/org-style-dark ()
  "Custom style mode."
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.0)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :height (cdr face) :weight 'regular))

  (cond
   ((eq (car custom-enabled-themes) 'nord)
    (set-face-attribute 'org-hide nil
                        :background bella-color-black
                        :foreground bella-color-black)

    (set-face-attribute 'org-block-end-line nil
                        ;; :foreground bella-color-grey
                        :background bella-color-high
                        :weight 'bold
                        :inherit 'fixed-pitch
                        )

    (set-face-attribute 'org-block-begin-line nil
                        ;; :foreground bella-color-grey
                        :background bella-color-high
                        :weight 'bold
                        :inherit 'fixed-pitch
                        )
    )

   ((eq (car custom-enabled-themes) 'modus-operandi)
    (set-face-attribute 'org-hide nil
                        :background "white"
                        :foreground "white"
                        )
    (set-face-attribute 'org-block-begin-line nil
                        :inherit 'fixed-pitch
                        )
    )
   )

  (set-face-attribute 'org-block nil    :inherit 'fixed-pitch)
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

  ;; (setq org-todo-keywords
  ;;       '((sequence "☐ TODO(t)" "✎ INPROG(i)" "⚙ WORK(w)" "|" "✔ DONE(d)" "✘ CANCELLED(c@)")))

  )

(use-package org-appear
  :straight t
  :defer t
  :hook (org-mode . org-appear-mode)
  :commands (org-appear-mode)
  :init
  (setq org-hide-emphasis-markers t		;; A default setting that needs to be t for org-appear
        org-appear-autoemphasis t     ;; Enable org-appear on emphasis (bold, italics, etc)
        org-appear-autolinks t        ;; Don't enable on links
        org-appear-autosubmarkers t)
  )

(use-package org-download
  :straight t
  :after org
  :config
  ;; (setq org-download-method 'Directory)
  ;; (setq org-download-screenshot-method "convert clipboard: %s")
  (setq-default org-download-image-dir "~/image")
  )

(use-package org
  :straight t
  :demand t
  :config
  ;; indent tab-width
  ;; Edit settings
  ;; (setq org-ellipsis " ... ")
  ;; (setq org-indent-mode-turns-on-hiding-stars nil)

  (setq org-startup-folded 'showeverything)
  (setq org-image-actual-width 300)

  ;; (setq org-fontify-whole-heading-line t)
  (setq org-pretty-entities t)
  (setq org-src-fontify-natively t) ;; Syntax highlighting in org src blocks
  ;; (setq org-highlight-latex-and-related '(native)) ;; Highlight inline LaTeX

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-startup-indented t)
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width nil)

  ;; TODO
  (defun my-set-org-image-width-based-on-columns (columns)
    "Set `org-image-actual-width` based on a specified COLUMN width."
    (let ((column-width (* columns (frame-char-width))))  ;; Calculate width in pixels
      (setq org-image-actual-width column-width)))  ;; Set image width

  ;; Example: Set images to a width of 40 columns
  (defun my-set-org-image-width ()
    (my-set-org-image-width-based-on-columns 120))  ;; Change 40 to your desired column width

  ;; Add hook to Org mode
  (add-hook 'org-mode-hook 'my-set-org-image-width)

  (cond
   ((eq system-type 'darwin)
    (setq org-directory "/Users/weitingchen/Library/Mobile Documents/iCloud~md~obsidian/Documents/weitingchen/"))
   ((eq system-type 'windows-nt)
    (setq org-directory  "~/iCloudDrive/iCloud~md~obsidian/weitingchen/"))
   )
  ;; (setq org-agenda-files '((concat org-directory "weitingchen.org")))
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))

  (add-hook 'org-mode-hook
            (lambda ()
              ;; (setq-default tab-width 8)
              (setq-default indent-tabs-mode nil)
              (setq-local evil-auto-indent nil)
              (org-indent-mode)
              (setq org-hide-emphasis-markers t)
              (setq org-hide-leading-stars t)
              )
            )

  (add-hook 'org-mode-hook #'wt/org-mode-setting)
  (add-hook 'org-mode-hook #'wt/org-style-dark)

  ;;TODO
  ;; (text       "#e0def4")  ;; foreground
  ;; (base       "#232136")  ;; background
  ;; (high       "#393552")  ;; highlight
  ;; (gold       "#f6c177")  ;; critical
  ;; (iris       "#c4a7e7")  ;; salient
  ;; (surface    "#6e6a86")  ;; strong
  ;; (love       "#eb6f92")  ;; popout
  ;; (subtle     "#2a273f")  ;; subtle
  ;; (faded      "#6e6a86")  ;; faded
  ;; (cursor     "#c4a7e7")) ;; cursor

  (setq org-todo-keywords '((type
                             "TODO(t)"
                             "INPROG-TODO(i)"
                             "WORK(w)"
                             "NOTE(n)"
                             "PROJ(p)"
                             "|" "DONE(d)" "CANCELLED(C@)")))

  (setq org-todo-keyword-faces
        '(("TODO"  :inherit (org-todo region) :foreground "black" :background "#f6c177" :weight bold)
          ("INPROG-TODO"  :inherit (org-todo region) :foreground "black" :background "#eb6f92" :weight bold)
          ("WORK"  :inherit (org-todo region) :foreground "black" :background "#c4a7e7" :weight bold)
          ("NOTE"  :inherit (org-todo region) :foreground "white" :background "#6e6a86" :weight bold)
          ("PROJ"  :inherit (org-todo region) :foreground "white" :background "#c4a7e7" :weight bold)
          ("DONE"  :inherit (org-todo region) :foreground "white" :background "#393552" :weight bold)
          ;; ("CANCELLED"  :inherit (org-todo region) :foreground "black" :background "grey" :weight bold)
          )
        )

  ;; set keybinding
  (add-hook 'org-mode-hook
            (lambda ()
              (wt/leader-keys
                "m" '(:ignore :wk "org")
                "mm" '(org-emphasize :wk "org markers")
                "mr" '(org-appear-mode :wk "org render")
                "mp" '(org-download-clipboard :wk "org paste form clipboard")
                "ml" '(org-insert-todo-subheading :wk "org paste form clipboard")
                "mti" '(org-display-inline-images :wk "org display image")
                "RET" '(org-open-at-point :wk "org open link")
                ;; "mtl" '(org-toggle-link-display :wk "org toggle link")
                )
              )
            )
  )

(provide 'org)
;;; org.el ends here
