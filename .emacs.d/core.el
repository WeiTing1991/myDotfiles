;; core.el

;; gernal keybinding
;; zoom in and out 
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Toggle between split windows and a single window
(defun toggle-windows-split()
  (interactive)
  (if (not(window-minibuffer-p (selected-window)))
      (progn
        (if (< 1 (count-windows))
            (progn
              (window-configuration-to-register ?u)
              (delete-other-windows))
          (jump-to-register ?u))))
  (my-iswitchb-close))

(define-key global-map (kbd "C-'") 'toggle-windows-split)

;; vim mode
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
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

  (define-key evil-normal-state-map (kbd "-") 'comment-line)
  (define-key evil-visual-state-map (kbd "-") 'comment-line)
	
  (define-key evil-visual-state-map (kbd "J")   'drag-stuff-down)   ;; Move lines down
  (define-key evil-visual-state-map (kbd "K")   'drag-stuff-up)     ;; Move lines up

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

)

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Main keybinding
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
    "/" '(execute-extended-command :wk "consult-M-x")
    "'" '(project-eshell :wk "run eshell")
		)

  ;; find file 
  (wt/leader-keys
    "f"  '(:ignore t :wk "Files")
    "ff" '(find-file :wk "Find Files")
    "fg" '(consult-grep :wk "Search for string in files in DIR")
    "fl" '(consult-ripgrep :wk "Search for string current file")
    ;; "fn" '(counsel-recentf :wk "Find recent files")
    )

  ;; buffer move
  (wt/leader-keys
    "b" '(:ignore t :wk "Bookmarks/Buffers")
    "bl" '(persp-ibuffer ibu :wk "List buffers")
    "bb" '(consult-buffer-other-window :wk "Switch buffer")
    "q" '(kill-buffer-and-window :wk "Kill this buffer")
    "o" '(next-buffer :wk "Next buffer")
    "i" '(previous-buffer :wk "Previous buffer")
    ;; "br" '(revert-buffer :wk "Reload buffer")
    )

	;; window
  (wt/leader-keys
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "wq" '(evil-window-delete :wk "Close window")
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
 
	;; dir
  (wt/leader-keys
    "d" '(:ignore t :wk "Dir")
    "dd" '(dired :wk "Open dired")
    "dj" '(dired-jump :wk "Open dired")

		)

	;; project
  (wt/leader-keys
    "p" '(:ignore t :wk "project")
    "pc" '(persp-new :wk "persp new ")
    "ps" '(persp-switch :wk "persp switch ")
    "pwj" '(persp-next :wk "persp next ")
    "pwk" '(persp-prev :wk "persp prev ")

    "pk" '(persp-kill :wk "persp kill")
    "pK" '(persp-kill-others :wk "persp kill")

		)

	;; toggle
  (wt/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "to" '(org-mode :wk "Toggle org mode")
    "tr" '(rainbow-mode :wk "Toggle rainbow mode")
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
;j; hightlight yank
(setq evil-goggles-delete nil)
(setq evil-goggles-duration 0.05)

(use-package evil-goggles
  :straight t
  :config
  (evil-goggles-mode)
  ;; optionally use diff-mode's faces; as a result, deleted text
  (evil-goggles-use-diff-faces)
  )
(custom-set-faces 
  '(evil-goggles-default-face ((t (:inherit 'menu))))
  '(evil-goggles-paste-face ((t (:inherit 'lazy-highlight))))
  '(evil-goggles-yank-face ((t (:inherit 'menu))))
  )

;; TODO
;; maybe check this https://github.com/casouri/vundo
(use-package undo-tree
	:straight t
	:config 
 (global-undo-tree-mode)
 :custom
 (setq undo-tree-auto-save-history t)
 (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
)

;; which-key
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.2)
  )

;; TODO
;; help fuction
;; https://github.com/Wilfred/helpful?tab=readme-ov-file
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-function) 
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable) 
  ([remap describe-callable] . helpful-callable) 
  ([remap describe-key] . helpful-key) 
	)
