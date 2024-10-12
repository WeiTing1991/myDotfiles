;; core.el

;; Gernal keybinding
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


;; Define a function to switch to the next buffer without showing messages
;; TODO check is it not working

;; TODO https://github.com/doomemacs/doomemacs/blob/master/modules/config/default/+evil-bindings.el
;; https://github.com/daviwil/dotfiles/blob/master/.emacs.d/modules/dw-keys-evil.e
(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  ;; (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)

  (setq select-enable-clipboard t)
  (defalias 'forward-evil-word 'forward-evil-symbol)

  (setq evil-shift-width 2)

  ;; (setq evil-want-fine-undo t)
  ;; (setq evil-ex-visual-char-range t)

  :config
  (evil-mode 1)


  ;; https://evil.readthedocs.io/en/latest/faq.html#underscore-is-not-a-word-character
  (define-key evil-outer-text-objects-map "w" 'evil-a-symbol)
  (define-key evil-inner-text-objects-map "w" 'evil-inner-symbol)
  (define-key evil-outer-text-objects-map "o" 'evil-a-word)
  (define-key evil-inner-text-objects-map "o" 'evil-inner-word)

  (define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-k") nil)
  (define-key evil-normal-state-map (kbd "C-k") nil)

  (define-key evil-insert-state-map (kbd "C-j") nil)
  (define-key evil-normal-state-map (kbd "C-j") nil)
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

  (define-key evil-visual-state-map (kbd "-") 'comment-dwim)
  (define-key evil-normal-state-map (kbd "-") 'comment-line)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  ;; Keybind
  ;; Define a prefix key and bind it in one line
  (define-prefix-command 'wt/window-map)
  (global-set-key (kbd "C-b") 'wt/window-map)

  (define-key evil-normal-state-map (kbd "C-b") 'wt/window-map)
  (define-key evil-visual-state-map (kbd "C-b") 'wt/window-map)

  (define-key wt/window-map (kbd "v") #'wt/split-and-follow-vertically)
  (define-key wt/window-map (kbd "h") #'wt/split-and-follow-horizontally)
  (define-key wt/window-map (kbd "r") 'eval-buffer)
  (define-key wt/window-map (kbd "s") 'persp-switch)
  (define-key wt/window-map (kbd "n") 'persp-next)
  (define-key wt/window-map (kbd "p") 'persp-prev)

    (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
        "C-b r" "Reload the buffer"
        "C-b v" "Split window Vertically"
        "C-b h" "Split window Horizontally"))

  ;; (global-set-key (kbd "c-s")   #'save-buffer)
  ;; (unless (display-graphic-p)
  ;;   (global-set-key (kbd "C-h") #'backward-kill-word))


	;; ;; project
 ;;  (wt/leader-project
 ;;    "h" '(persp-switch :wk "project switch")
 ;;    "n" '(persp-next :wk "project next")
 ;;    "p" '(persp-prev :wk "project prev")
 ;;    "k" '(persp-kill :wk "persp kill")
 ;;    "K" '(persp-kill-others :wk "persp kill")
 ;;    ;;
 ;;    "r" '(eval-buffer :wk "reload buffer")
 ;;    "t" '(wt/switch-to-eshell :wk "toggle eshell")
 ;;
 ;;   )
  (defun wt/confirm-exit ()
    (interactive)
    (if (yes-or-no-p "Are you sure you want to exit Emacs? ")
        (save-buffers-kill-terminal)  ;; Save all buffers and kill Emacs
      (message "Cancelled exit.")))  ;; Message if user cancels
  ;; (define-key evil-normal-state-map (kbd ":q") #'wt/confirm-exit)
)

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-mc
  :straight t
  :after evil
  :config
  (global-evil-mc-mode 1))

(electric-pair-mode 1)

;; (use-package evil-surround
;;   :straight t
;;   :after evil
;;   :config
;;   (global-evil-surround-mode t))

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
    "C-'" '(project-shell :wk "run shell")
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
    "o" '(next-buffer  :wk "Next buffer")
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
    "dj" '(dired-jump :wk "Open dired jump current")

		)

	;; toggle
  (wt/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "to" '(org-mode :wk "Toggle org mode")
    "tr" '(rainbow-mode :wk "Toggle rainbow mode")
    "tf" '(flycheck-mode :wk "Toggle check mode")
		)

	;;
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
;(setq evil-goggles-delete nil)
;(setq evil-goggles-duration 0.2)

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

;; ;; maybe check this https://github.com/casouri/vundo
;(use-package undo-tree
;  :straight t
;  :config
;  (global-undo-tree-mode)
;  :custom
;  ;; on windows is really slow
;  (setq undo-tree-history-directory "~/.emacs.d/undo")
;  (setq undo-tree-auto-save-history t)
;)
;
;(with-eval-after-load 'evil
;  (when (bound-and-true-p global-undo-tree-mode)
;    (define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)
;    (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo))
;  ;; (global-set-key (kbd "C-x u") 'undo-tree-visualize)
;)

(use-package diminish
  :straight t
  )

;; which-key
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.1)
  )

;; TODO
;; help fuction
;; https://github.com/Wilfred/helpful
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key helpful-at-point)
  :bind
  ;; Remap standard help commands to helpful versions
  ([remap describe-function] . helpful-function)    ;; Remaps C-h f
  ([remap describe-command] . helpful-command)      ;; Remaps C-h x
  ([remap describe-variable] . helpful-variable)    ;; Remaps C-h v
  ([remap describe-key] . helpful-key)              ;; Remaps C-h k
  ;; Bind helpful-at-point to a custom key (optional)
  ("C-h p" . helpful-at-point))                   ;; Quickly show info at point


;;; core.el code end here
