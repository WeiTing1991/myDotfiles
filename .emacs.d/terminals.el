;; terminals.el

;; shell setup for windows
(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

;; eshell
(defun wt/configure-eshell () ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         1000 eshell-buffer-maximum-lines 1000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :hook (eshell-first-time-mode . wt/configure-eshell)
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . wt/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim" "bash")))

  (eshell-git-prompt-use-theme 'robbyrussell))

;; vterm (not for windows)

;; eat
;; https://codeberg.org/akib/emacs-eat
(use-package eat
  :hook (eshell-first-time-mode . wt/configure-eshell)
  :straight t
  :config
  (eat-eshell-mode)
  ;; (setq eshell-visual-commands '())
)

;; ;; xterm color
(use-package eterm-256color
	:straight t
  :hook ((shell-mode . eterm-256color-mode)
         (eshell-mode . eterm-256color-mode)
         (eat-eshell-mode . eterm-256color-mode))
	)


