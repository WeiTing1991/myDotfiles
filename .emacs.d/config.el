;; config.el
;; Initialize package sources
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; (straight-use-package 'use-package)

(use-package drag-stuff
  :straight t
 )

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-redo)

  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (define-key evil-normal-state-map (kbd "-") 'comment-line)
  (define-key evil-visual-state-map (kbd "-") 'comment-line)

  (define-key evil-visual-state-map (kbd "J")   'drag-stuff-down)   ;; Move lines down
  (define-key evil-visual-state-map (kbd "K")   'drag-stuff-up)     ;; Move lines up

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (add-to-list 'evil-collection-mode-list 'help) ;; evilify help mode
  (evil-collection-init))

(use-package general
  :straight t
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer wt/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  ;; main kyes
  (wt/leader-keys
    "/" '(counsel-M-x :wk "Counsel M-x")
    )

  ;; find file 
  (wt/leader-keys
    "f"  '(:ignore t :wk "Files")
    "ff" '(find-file :wk "Find Files")
    "fg" '(find-grep-dired :wk "Search for string in files in DIR")
    "fl" '(counsel-grep-or-swiper :wk "Search for string current file")
    "fn" '(counsel-recentf :wk "Find recent files")
    )

  ;; buffer move
  (wt/leader-keys
    "b" '(:ignore t :wk "Bookmarks/Buffers")
    "bb" '(list-buffers :wk "List buffers")
    "bs" '(switch-to-buffer :wk "Switch buffer")
    ;; "q" '(kill-this-buffer :wk "Kill this buffer")
    "o" '(next-buffer :wk "Next buffer")
    "i" '(previous-buffer :wk "Previous buffer")
    "br" '(revert-buffer :wk "Reload buffer"))

  (wt/leader-keys
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "q" '(evil-window-delete :wk "Close window")
    "wn" '(evil-window-new :wk "New window")
    "ws" '(evil-window-split :wk "Horizontal split window")
    "wv" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "h" '(evil-window-left :wk "Window left")
    "j" '(evil-window-down :wk "Window down")
    "k" '(evil-window-up :wk "Window up")
    "l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; Move Windows
    ;; "w H" '(buf-move-left :wk "Buffer move left")
    ;; "w J" '(buf-move-down :wk "Buffer move down")
    ;; "w K" '(buf-move-up :wk "Buffer move up")
    ;; "w L" '(buf-move-right :wk "Buffer move right")
    )

  ;; reload 
  ;; (wt/leader-keys
  ;;   "h" '(:ignore t :wk "Help")
  ;;   "hr" '((lambda () (interactive)
  ;;               (load-file "~/.emacs.d/init.el")
  ;;               (ignore (elpaca-process-queues)))
  ;;             :wk "Reload emacs config")
  ;;   )

)

;; auto cmp
(use-package counsel
  :after ivy
  :diminish
  :config 
    (counsel-mode)
    (setq ivy-initial-inputs-alist nil)) ;; removes starting ^ regex in M-x

(use-package ivy
  :bind (("C-n" . ivy-next-line)
         ("C-p" . ivy-previous-line)
         :map ivy-switch-buffer-map)
  :diminish
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package all-the-icons-ivy-rich
  :straight t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))
;; UI
(use-package all-the-icons
  :straight t
 ) 
(use-package nerd-icons
  :straight t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  ;; Set the font for Nerd Icons
  )

(use-package nerd-icons-dired
  :straight t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35      ;; sets modeline height
        doom-modeline-bar-width 5    ;; sets right bar width
        doom-modeline-persp-name t   ;; adds perspective name to modeline
        doom-modeline-persp-icon t)) ;; adds folder icon next to persp name

;; which key
(use-package which-key
  :init
    (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
	  which-key-sort-order #'which-key-key-order-alpha
	  which-key-allow-imprecise-window-fit nil
	  which-key-sort-uppercase-first nil
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.8
	  which-key-max-description-length 25
	  which-key-allow-imprecise-window-fit nil
	  which-key-separator " → " )
  )
