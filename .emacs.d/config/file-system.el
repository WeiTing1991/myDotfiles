;;; file sytsem

;; code:
(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)
  ;;keybinding
  (evil-collection-define-key 'normal 'dired-mode-map (kbd "n") 'dired-create-empty-file)
  (evil-collection-define-key 'normal 'dired-mode-map (kbd "N") 'dired-create-directory)
  (evil-collection-define-key 'normal 'dired-mode-map (kbd "D") 'dired-do-delete)
  (evil-collection-define-key 'normal 'dired-mode-map (kbd "r") 'dired-do-rename)
  (evil-collection-define-key 'normal 'dired-mode-map (kbd "y") 'dired-do-copy)

  ;; (setq insert-directory-program "gls")

  ;; :custom
  ;; (setq variable-pitch-mode t)
  ;; ((dired-listing-switches "-al --group-directories-first"))
  ;; (setq-default dired-listing-switches "-alh")
  )

(use-package dired+
  :straight t
  :defer t
  :config
  (require 'dired+))
;; (setq dired-dwim-target nil)

;; File open
;; https://emacs.stackexchange.com/questions/3105/how-to-use-an-external-program-as-the-default-way-to-open-pdfs-from-emacs
;; Function to open a file with the system's default application
(defun my-dired-open-with-system (file)
  "Open FILE with the system's default application."
  (cond
   ((eq system-type 'darwin)
    ;; macOS
    (start-process "open" nil "open" file)
    )
   ((eq system-type 'windows-nt)
    (w32-shell-execute "open" file))
   )
  )

(defun my-dired-open ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if file
        (my-dired-open-with-system file)
      (message "No file selected."))))


(evil-collection-define-key 'normal 'dired-mode-map (kbd "C-o") 'my-dired-open)
(use-package dired-single
  :straight nil
  :commands (dired dired-jump))

;; https://github.com/Fuco1/dired-hacks
(use-package dired-open
  :commands (dired dired-jump)
  ;; :bind ("C-o" . dired-open-functions)
  ;; :config
  ;; (add-to-list 'dired-open-functions #'my-dired-open t)
  ;; (evil-collection-define-key 'normal 'dired-mode-map (kbd "C-o") #'my-dired-open)
  )

(setq dired-hide-dotfiles-mode -1)
(use-package dired-hide-dotfiles
  :straight t
  ;; :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map (kbd "H") 'dired-hide-dotfiles-mode)
  )

(use-package dired-preview
  :straight t
  :after dired
  :config
  (evil-define-key 'normal dired-mode-map (kbd "C-p") 'dired-preview-mode)
  ;; (evil-define-key 'normal dired-preview-mode-map (kbd "h") 'dired-up-directory)
  ;; (evil-define-key 'normal dired-preview-mode-map (kbd "l") 'dired-open-file)
  ;; (evil-define-key 'normal dired-preview-mode-map (kbd "j") 'peep-dired-next-file)
  ;; (evil-define-key 'normal dired-preview-mode-map (kbd "k") 'peep-dired-prev-file)

  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-preview-delay 0.1)
  (setq dired-preview-max-size (* 100 1024 1024))
  (setq dired-preview-ignored-extensions-regexp
        (concat "\\."
                "\\(gz\\|"
                "zst\\|"
                "tar\\|"
                "xz\\|"
                "rar\\|"
                "zip\\|"
                "iso\\|"
                "epub"
                "\\)")
        )
  ;; globally:
  ;; (dired-preview-global-mode 1)
  )

(use-package ibuffer
  :straight nil
  :config
  )

;; workspace
(cond
 ;; macOS configuration
 ((eq system-type 'darwin)  ;; 'darwin' is for macOS
  (defvar wt/tab-font-size 140)
  )
 ;; Windows configuration
 ((eq system-type 'windows-nt)  ;; 'windows-nt' is for Windows
  (defvar wt/tab-font-size 120)
  )
 )

(tab-bar-mode t)
(require 'bella-base-color)
(set-face-attribute 'tab-bar nil
                    :font "RobotoMono Nerd Font"
                    :weight 'bold
                    :background "black"
                    :foreground "#ffffff"
                    :height wt/tab-font-size)

;; Customize the appearance of active tabs
(set-face-attribute 'tab-bar-tab nil
                    :background bella-color-black
                    :foreground "#ffffff"
                    :weight 'bold
                    :box nil
                    )

;; Customize the appearance of inactive tabs
(set-face-attribute 'tab-bar-tab-inactive nil
                    :background bella-color-base
                    :foreground "#ffffff"
                    :box nil
                    )


;; NOTE https://github.com/nex3/perspective-el
;; (use-package perspective
;;   :straight nil
;;   :bind
;;   ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
;;   :custom
;;   (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
;;   :init
;;   (persp-mode)
;;   )

;; TODO check more function
;; https://github.com/mclear-tools/tabspaces

(use-package tabspaces
  ;; use this next line only if you also use straight, otherwise ignore it.
  :straight (:type git :host github :repo "mclear-tools/tabspaces")
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Main")
  ;; (tabspaces-remove-to-default t)
  ;; (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo t)
  ;; (tabspaces-todo-file-name "project-todo.org")
  ;; sessions
  ;; (tabspaces-session t)
  ;; (tabspaces-session-auto-restore t)
  (tab-bar-new-tab-choice "*scratch*")
  )

;; TODO https://github.com/mclear-tools/tabspaces?tab=readme-ov-file
;; https://docs.projectile.mx/projectile/projects.html
;; (use-package projectile
;;  :defer t
;;   :diminish projectile-mod
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


(provide 'file-system)
;;;file-system code end here
