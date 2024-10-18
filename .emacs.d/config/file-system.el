;;; file sytsem

(setq dired-kill-when-opening-new-dired-buffer t)

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  ;; :custom ((dired-listing-switches "-al --group-directories-first"))
  )
;; (setq-default dired-listing-switches "-alh")

(use-package dired-single
  :straight nil
  :commands (dired dired-jump))

;; (use-package all-the-icons-dired
;;   :straight t
;;   :defer t
;;   :hook (dired-mode . all-the-icons-dired-mode))

;; https://github.com/Fuco1/dired-hacks
(use-package dired-open
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("gif" . "sxiv")
                                ("jpg" . "sxiv")
                                ("png" . "sxiv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

;; TODO
;; (use-package dired-narrow
;;   :defer t
;;   :hook (dired-mode . dired-narrow-mode)
;;   :custom
;;   (evil-define-key 'normal dired-mode-map (kbd "C-/") 'peep-dired)
;;  )

(setq dired-hide-dotfiles-mode -1)
(use-package dired-hide-dotfiles
  :straight t
  ;; :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map (kbd "H") 'dired-hide-dotfiles-mode)
  )

;; https://github.com/Fuco1/dired-hacks
;; (setq peep-dired-mode 1)
(use-package peep-dired
  :straight t
  :after dired
  ;; :hook (dired-mode . peep-dired)
  :config
  (evil-define-key 'normal dired-mode-map (kbd "C-p") 'peep-dired)
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file)
  ;; ; use dired-find-file instead if not using dired-open package
  (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
  (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
)

(use-package ibuffer
  :straight nil
  )

;; workspace
;; NOTE https://github.com/nex3/perspective-el
(use-package perspective
  :straight t
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode)
)

(use-package perspective-tabs
  :straight (:host sourcehut :repo "woozong/perspective-tabs")
  :defer t
  :after (perspective)
  :init
  (perspective-tabs-mode +1)
)

(use-package company-prescient
             :straight t
             :after company
             :config
             (company-prescient-mode 1)
             )


;; TODO https://github.com/mclear-tools/tabspaces?tab=readme-ov-file
;; https://docs.projectile.mx/projectile/projects.html
;; (use-package projectile
;;  :defer t
;;   :diminish projectile-mode
;;   :config (projectile-mode)
;;   ;; (define-key projectile-mode-map (kbd "C-p") 'projectile-persp-switch-project)
;;   :custom ((projectile-completion-system 'default))
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :init
;;   ;; NOTE: Set this to the folder where you keep your Git repos!
;;  ;; (setq projectile-project-search-path '("~/projects/" "~/work/"))
;;   (setq projectile-enable-caching t)  ;; Enable caching for faster project lookups
;;   (setq projectile-indexing-method 'native)  ;; Use native indexing for performance
;; )

;; (use-package persp-projectile
;;   :straight t
;; )

;; git tool
(use-package magit
  :straight t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(provide 'file-system)
;;;file-system code end here
