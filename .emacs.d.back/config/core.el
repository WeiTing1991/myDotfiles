;;; core.el

;; (defun ian/maybe-set-default-browser ()
;;   "When in WSL Emacs, open links in default Windows 11 browser."
;;   (cond
;;    ((eq system-type 'gnu/linux)
;;     (when (string-match "Linux.*microsoft.*Linux"
;;                         (shell-command-to-string "uname -a"))
;;       (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
;;             browse-url-generic-args '("/c" "start" "")
;;             browse-url-browser-function 'browse-url-generic)))))

;; split the windows and focus on
(defun wt/split-and-follow-vertically ()
  "Split window vertically (below)."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun wt/split-and-follow-horizontally ()
  "Split window horizontally (right)."
  (interactive)
  (split-window-right)
  (other-window 1))


;; Gernal keybinding
;; zoom in and out
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(define-key global-map (kbd "C-k") nil)
(define-key global-map (kbd "C-j") nil)

(defun open-all-recent-files ()
  "Open all recent files."
  (interactive)
  (dolist (file  recentf-list) (find-file file)))

(define-key global-map (kbd "C-w") 'open-all-recent-files)

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

;; Advise `kill-emacs`
(defun wt/advice-kill-emacs (orig-fun &rest args)
  "Advise `kill-emacs` to confirm before exit."
  (if (yes-or-no-p "Are you sure you want to exit Emacs? ")
      (apply orig-fun args)
    (message "Cancelled exit.")))

(advice-add 'kill-emacs :around #'wt/advice-kill-emacs)


;; (defun wt/find-file-preview ()
;;   (interactive)
;;   (let ((consult-ripgrep-command "rg --multiline --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
;;     (consult-ripgrep)))


;; TODO
;; https://github.com/doomemacs/doomemacs/blob/master/modules/config/default/+evil-bindings.el
;; https://github.com/daviwil/dotfiles/blob/master/.emacs.d/modules/dw-keys-evil.e
(use-package drag-stuff
  :straight t
  :after evil
  :init
  (drag-stuff-mode t)
)

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
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

  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

  (define-key evil-visual-state-map (kbd "-") 'comment-dwim)
  (define-key evil-normal-state-map (kbd "-") 'comment-line)

  (define-key evil-visual-state-map (kbd "K") 'drag-stuff-up)
  (define-key evil-visual-state-map (kbd "J") 'drag-stuff-down)

  ;; (define-key evil-visual-state-map (kbd ">") 'drag-stuff-right)
  ;; (define-key evil-visual-state-map (kbd "<") 'drag-stuff-left)

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
  (define-key wt/window-map (kbd "k") 'persp-kill)
  (define-key wt/window-map (kbd "K") 'persp-kill-others)

  (with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
      "C-b r" "Reload the buffer"
      "C-b v" "Split window Vertically"
      "C-b h" "Split window Horizontally"))

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
    "m" '(lsp-format-buffer :wk "formating")
    )

  ;; find file
  (wt/leader-keys
    "f"  '(:ignore t :wk "Files")
    "ff" '(consult-fd :wk "fd Find Files")
    "fd" '(find-file :wk "Find Files in current DIR")
    "fg" '(consult-grep :wk "Search for string in files in DIR")
    "fl" '(consult-ripgrep :wk "Search for string current file")

    "fp" '(wt/find-file-preview :wk "Search for string current file")
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

    ;; Window motions
    "h" '(evil-window-left :wk "Window left")
    "j" '(evil-window-down :wk "Window down")
    "k" '(evil-window-up :wk "Window up")
    "l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")

    ;; Move Windows
    "H" '(buf-move-left :wk "Buffer move left")
    ;; "J" '(buf-move-down :wk "Buffer move down")
    ;; "w K" '(buf-move-up :wk "Buffer move up")
    "L" '(buf-move-right :wk "Buffer move right")
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
    "tt" '(lsp-treemacs-error-list :wk "Error list")
    "to" '(org-mode :wk "Toggle org mode")
    "tr" '(rainbow-mode :wk "Toggle rainbow mode")
    "tf" '(flycheck-mode :wk "Toggle check mode")
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
  ;; (evil-goggles-use-diff-faces)
  :custom
  (setq evil-goggles-duration 0.3)
  )

(set-face-attribute 'evil-goggles-yank-face nil
                    :foreground "white"
                    :background "yellow"
                    :weight 'bold)

;; clean white space
(add-hook 'before-save-hook 'whitespace-cleanup)


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

;; TODO help fuction
;; https://github.com/Wilfred/helpful
;; (use-package helpful
;;   :defer t
;;   :commands (helpful-callable helpful-variable helpful-command helpful-key helpful-at-point)
;;   :bind
;;   ;; Remap standard help commands to helpful versions
;;   ([remap describe-function] . helpful-function)    ;; Remaps C-h f
;;   ([remap describe-command] . helpful-command)      ;; Remaps C-h x
;;   ([remap describe-variable] . helpful-variable)    ;; Remaps C-h v
;;   ([remap describe-key] . helpful-key)              ;; Remaps C-h k
;;   ;; Bind helpful-at-point to a custom key (optional)
;;   ("C-h p" . helpful-at-point)
;;   )                   ;; Quickly show info at point

(provide 'core)
;;; core.el code end here
