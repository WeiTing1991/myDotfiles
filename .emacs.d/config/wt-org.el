;;; wt-org.el --- org mode setting.| -*- lexical-binding: t -*-

;;; code:

;; org mode
;; NOTE
;; https://github.com/minad/org-modern?tab=readme-ov-file
;; https://doc.norang.ca/org-mode.html
;; https://github.com/jakebox/jake-emacs

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

  ;; indent
  (cond
   ;; dark
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
   ;; light
   ((eq (car custom-enabled-themes) 'modus-operandi)
    (set-face-attribute 'org-hide nil
                        :background bella-color-white
                        :foreground bella-color-white
                        )
    (set-face-attribute 'org-block-begin-line nil
                        :inherit 'fixed-pitch
                        )
    )
   )

  ;; (set-face-attribute 'org-block nil    :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  ;; (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)

  ;; (set-face-attribute 'org-quote nil    :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))

  ;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  ;; (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  ;; (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  ;; (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)
  )

(defcustom wt-prettify-org-symbols-alist
  '(("[ ]"            . ?)
    ("[-]"            . ?)
    ("[X]"            . ?)

    ;; ("#+BEGIN_SRC"    . ?✎)
    ;; ("#+END_SRC"      . ?□)
    ;; ("#+BEGIN_QUOTE"  . ?«)
    ;; ("#+END_QUOTE"    . ?»)
    ;; ("#+RESULTS:"     . ?💻)
    )
  "A list of symbol prettifications for `org-mode'."
  :group 'wt
  :type '(alist :key-type string :value-type (choice character sexp)))

;; (org-modern-star ["⦶" "⦷" "⦹" "⊕" "⍟" "⊛" "⏣" "❂"])

(use-package org
  :straight t
  :custom-face (org-ellipsis ((t (:foreground unspecified))))
  :init (setq
         variable-pitch-mode 1
         visual-line-mode 1
         auto-fill-mode 0)

  :hook (
         (org-mode . (lambda ()
                       ;; (setq-default tab-width 8)
                       (setq-default indent-tabs-mode nil)
                       (setq-local evil-auto-indent nil)
                       (org-indent-mode)
                       )
                   )
         (org-mode . (lambda ()
                       (wt/org-style-dark)
                       )
                   )
         (org-mode . (lambda ()
                       "Beautify org symbols."
                       (if prettify-symbols-alist
                           (push wt-prettify-org-symbols-alist prettify-symbols-alist)
                         (setq prettify-symbols-alist wt-prettify-org-symbols-alist))
                       (prettify-symbols-mode 1)
                       )
                   )
         )
  :config
  (setq org-modules nil                 ; Faster loading
        ;; org-capture-templates
        ;; `(("i" "Idea" entry (file ,(concat org-directory "/idea.org"))
        ;;    "*  %^{Title} %?\n%U\n%a\n")
        ;;   ("t" "Todo" entry (file ,(concat org-directory "/gtd.org"))
        ;;    "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ;;   ("n" "Note" entry (file ,(concat org-directory "/note.org"))
        ;;    "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ;;   ("j" "Journal" entry (file+olp+datetree
        ;;                         ,(concat org-directory "/journal.org"))
        ;;    "*  %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ;; ("b" "Book" entry (file+olp+datetree
        ;;                      ,(concat org-directory "/book.org"))
        ;;  "* Topic: %^{Description}  %^g %? Added: %U"))

        ;; org-priority-faces '((?A . error)
        ;;                      (?B . warning)
        ;;                      (?C . success))

        ;; Agenda styling
        ;; org-agenda-files (list centaur-org-directory)
        ;; org-agenda-block-separator ?─
        ;; org-agenda-time-grid
        ;; '((daily today require-timed)
        ;;   (800 1000 1200 1400 1600 1800 2000)
        ;;   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        ;; org-agenda-current-time-string
        ;; "⭠ now ─────────────────────────────────────────────────"

        org-tags-column -80
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
        org-pretty-entities nil
        org-hide-emphasis-markers t
        org-startup-with-inline-images t

        ;; (setq org-src-fontify-natively t) ;; Syntax highlighting in org src blocks
        ;; (setq org-highlight-latex-and-related '(native)) ;; Highlight inline LaTeX
        ;; (setq org-image-actual-width 300)
        ;; (setq org-agenda-start-with-log-mode t)
        ;; (setq org-log-into-drawer t)

        ;; (setq org-hide-block-startup t)
        ;; (setq org-image-actual-width nil)

        )

  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "WORK(w)" "NOTE(n)" "PROJ(p)" "|" "DONE(d)" "CANCEL(C)")
                            (sequence "⚑(T)" "🏴(I)" "❓(N)" "|" "✔(D)" "✘(C)")
                            )
        )
  (setq org-todo-keyword-faces
        '(("TODO"  :inherit (org-todo region) :foreground "black" :background "#f6c177" :weight bold)
          ("DOING"  :inherit (org-todo region) :foreground "black" :background "#eb6f92" :weight bold)
          ("WORK"  :inherit (org-todo region) :foreground "black" :background "#c4a7e7" :weight bold)
          ("NOTE"  :inherit (org-todo region) :foreground "white" :background "#6e6a86" :weight bold)
          ("PROJ"  :inherit (org-todo region) :foreground "white" :background "#c4a7e7" :weight bold)
          ("DONE"  :inherit (org-todo region) :foreground "white" :background "#393552" :weight bold)
          ;; ("CANCELLED"  :inherit (org-todo region) :foreground "black" :background "grey" :weight bold)
          )
        )

  ;; set path
  (cond
   ((eq wt-os-type 'mac)
    (setq org-directory "/Users/weitingchen/Library/Mobile Documents/iCloud~md~obsidian/Documents/weitingchen/"))
   ((eq wt-os-type 'window)
    (setq org-directory  "~/iCloudDrive/iCloud~md~obsidian/weitingchen/"))
   ) ;; (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))

  )

;; set keybinding
(add-hook 'org-mode-hook
          (lambda ()
            (wt/leader-keys
              "m" '(:ignore :wk "org")
              "mm" '(org-emphasize :wk "org markers")
              "mr" '(org-appear-mode :wk "org render")
              "mp" '(org-download-clipboard :wk "org paste form clipboard")
              "ml" '(org-insert-todo-subheading :wk "org insert subheading")

              "=" '(image-increase-size :wk "org image Increase size")
              "-" '(image-decrease-size :wk "org image Decrease size")

              "mti" '(org-display-inline-images :wk "org display image")
              "RET" '(org-open-at-point :wk "org open link")
              ;; "mtl" '(org-toggle-link-display :wk "org toggle link")
              )
            )
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
  (setq-default org-download-image-dir "~/image")
  ;; (setq org-download-screenshot-method "convert clipboard: %s")
  )

;; Roam
;; TODO: check the url
(defvar wt-org-roam-directory
  (cond
   ((eq wt-os-type 'mac)
    "/Users/weitingchen/Library/Mobile Documents/iCloud~md~obsidian/Documents/weitingchen/")
   ((eq wt-os-type 'windows)
    "~/iCloudDrive/iCloud~md~obsidian/weitingchen/")))

(use-package org-roam
  :straight t
  :defer t
  :after org
  :diminish
  :defines org-roam-graph-viewer
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :init
  (setq org-roam-directory (file-truename wt-org-roam-directory)
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
        ;; org-roam-graph-viewer #'centaur-browse-url
        )
  :config
  ;; (unless (file-exists-p org-roam-directory)
  ;;   (make-directory org-roam-directory))
  ;; (add-to-list 'org-agenda-files (format "%s/%s" org-roam-directory "roam"))
  (org-roam-db-autosync-enable)
  )

(use-package org-roam-ui
  :straight t
  :defer t
  :after org-roam
  :bind ("C-c n u" . org-roam-ui-mode)
  ;; :init (setq org-roam-ui-browser-function #'centaur-browse-url)
  )

(provide 'wt-org)
;;; wt-org.el ends here
