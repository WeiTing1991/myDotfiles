;; file sytsem

(setq dired-kill-when-opening-new-dired-buffer t)

;; check
;; https://github.com/daviwil/emacs-from-scratch/blob/master/Emacs.org
(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  ;; :custom ((dired-listing-switches "-al --group-directories-first"))
	)
;; (setq-default dired-listing-switches "-alh")

;; NOTE
(use-package dired-single
  :straight nil
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

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
;; fuzzy and filter
(use-package dired-narrow
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :custom
  (define-key dired-mode-map (kbd "/") 'dired-narrow-fuzzy)
 )

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode)
	)

;; https://github.com/Fuco1/dired-hacks
(use-package peep-dired
 :straight t
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  :config
    (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
    (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file)
		; use dired-find-file instead if not using dired-open package
    (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
    (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
    (evil-define-key 'normal dired-mode-map (kbd "C-p") 'peep-dired)
)

;; (tab-bar-mode t)
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

;;TODO https://docs.projectile.mx/projectile/projects.html
(use-package projectile
	:defer t
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'default))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  ;; :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
	;; (setq projectile-project-search-path '("~/projects/" "~/work/" ("~/github" . 1)))
	;;   (when (file-directory-p "~/Projects/Code")
	;;     (setq projectile-project-search-path '("~/Projects/Code")))
	;;   (setq projectile-switch-project-action #'projectile-dired)
)

(defun wt/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))
(define-key evil-normal-state-map (kbd "M-c d") 'wt/get-project-root)


;; git tool
(use-package magit
  :straight t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
