;;; file sytsem

;; code:
(setq dired-kill-when-opening-new-dired-buffer t)

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  ;; :custom ((dired-listing-switches "-al --group-directories-first"))
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

;; (evil-collection-define-key 'normal 'dired-mode-map (kbd "C-o") 'my-dired-open)
(use-package dired-single
  :straight nil
  :commands (dired dired-jump))

;; https://github.com/Fuco1/dired-hacks
(use-package dired-open
  :commands (dired dired-jump)
  :bind ("C-o" . dired-open-functions)
  :config
  (add-to-list 'dired-open-functions #'my-dired-open t)
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
  )
(add-hook 'ibuffer-hook
          (lambda ()
            (persp-ibuffer-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;; workspace
(cond
 ;; macOS configuration
 ((eq system-type 'darwin)  ;; 'darwin' is for macOS
  (defvar wt/tab-font-size 120)
  )
 ;; Windows configuration
 ((eq system-type 'windows-nt)  ;; 'windows-nt' is for Windows
  (defvar wt/tab-font-size 100)
  )
 )

(tab-bar-mode t)
(set-face-attribute 'tab-bar nil
                    :font "RobotoMono Nerd Font"
                    :weight 'bold
                    :background "black"
                    :foreground "#ffffff"
                    :height wt/tab-font-size)

;; Customize the appearance of active tabs
(set-face-attribute 'tab-bar-tab-inactive nil
                    :background "#61afef" ;; Background color for active tab
                    :foreground "#282c34" ;; Foreground color for active tab text
                    :weight 'bold) ;; Make active tab text bold

;; Customize the appearance of inactive tabs
(set-face-attribute 'tab-bar-tab nil
                    :background "#3e4451" ;; Background color for inactive tabs
                    :foreground "#dcdfe4") ;; Foreground color for inactive tab text


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
