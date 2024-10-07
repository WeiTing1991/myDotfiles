;; core.el

;; essential package
;; theme
(use-package doom-themes
  :init (load-theme 'doom-palenight t)
)
(custom-set-faces
  '(default ((t (:background "#0D0907")))))

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
  (add-to-list 'evil-collection-mode-list 'help) ;; evilify help mode
  (evil-collection-init))

;; main keybinding
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
    "'" '(vterm-toggle :wk "Toggle vterm")
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

  (wt/leader-keys
    "d" '(:ignore t :wk "Dir")
    "dd" '(dired :wk "Open dired")
    "dj" '(dired-jump :wk "Open dired")

		)

  (wt/leader-keys
    "p" '(:ignore t :wk "project")
    "pc" '(persp-new :wk "persp new ")
    "ps" '(persp-switch :wk "persp switch ")
    "pwj" '(persp-next :wk "persp next ")
    "pwk" '(persp-prev :wk "persp prev ")

    "pk" '(persp-kill :wk "persp kill")
    "pK" '(persp-kill-others :wk "persp kill")

		)

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
;; hightlight yank
(use-package evil-goggles
  :straight t
  :config
  (evil-goggles-mode)
  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces)
  )
(custom-set-faces '(evil-goggles-default-face ((t (:inherit 'menu))))) ;; default is to inherit 'region
(custom-set-faces
 '(evil-goggles-paste-face ((t (:inherit 'lazy-highlight))))
 '(evil-goggles-yank-face ((t (:inherit 'menu)))))


;; Terminals
(use-package fakecygpty
  ;; Not available on elpa
  :straight (fakecygpty :host github :repo "d5884/fakecygpty")
  ;; Only required on Windows
  :if (eq system-type 'windows-nt)
  :config
  ;; Enable
  (fakecygpty-activate))

(use-package vterm
  :commands vterm
  :config
  ;; Set max scrollback
  (setq vterm-max-scrollback 5000)

  ;; Check system type and set shell accordingly
  (cond
   ;; macOS
   ((eq system-type 'darwin)
    (setq shell-file-name "/bin/zsh")) ;; Use Zsh on macOS

   ;; Windows
   ((eq system-type 'windows-nt)
    (setq shell-file-name "C:/Program Files/PowerShell/7/pwsh.exe")) ;; Use PowerShell 7 on Windows

   ;; Default case (for other systems, e.g., Linux)
   (t
    (setq shell-file-name "/bin/sh"))))

(use-package vterm-toggle
  :after vterm
  :config
  ;; When running programs in Vterm and in 'normal' mode, make sure that ESC
  ;; kills the program as it would in most standard terminal programs.
  (evil-define-key 'normal vterm-mode-map (kbd "<escape>") 'vterm--self-insert)
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.4))))

(use-package eterm-256color
  :hook (vterm-mode . eterm-256color-mode))

;; Eshell


;; which-key
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5)
  )
