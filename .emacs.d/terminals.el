;; terminals.el


(setenv "TERM" "eterm-256color")
(setq-default compilation-read-command nil)

;; shell setup for windows
(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "C:/Program Files/PowerShell/7/pwsh.exe")
  (setq explicit-pwsh.exe-args '("-NoLogo" "-NoProfile"))
  )

(use-package term
  :commands term
  :config
  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  )

;; xterm color
(use-package eterm-256color
  :straight t
  :hook ((shell-mode . eterm-256color-mode)
         (term-mode . eterm-256color-mode)
         (eshell-mode . eterm-256color-mode)
         (eat-eshell-mode . eterm-256color-mode))
  )

;; eshell
(defun wt/configure-eshell () ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         1000
        eshell-buffer-maximum-lines 1000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t)
  )


(use-package eshell-git-prompt
  :hook (eshell-first-time-mode . wt/configure-eshell)
  :after eshell
  )

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim"))
  )
 )

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


;; Prevent compilation mode in vterm buffers
;; (add-hook 'compilation-mode-hook
;;           (lambda ()
;;             (unless (derived-mode-p 'shell-mode 'eshell-mode)
;;               (kill-buffer (current-buffer)))))

