;;; wt-term.el --- command for system. -*- lexical-binding: t -*-

;;; code:
(use-package term
  :straight nil
  ;; :command (term)
  :defer t
  :config
  (when (eq wt-os-type 'window)
    (setq explicit-shell-file-name "C:/Program Files/PowerShell/7/pwsh.exe")
    ;; (setq explicit-shell-file-name "C:/msys64/usr/bin/bash.exe")
    ;; (setq explicit-shell-file-name "C:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
    (setq explicit-pwsh.exe-args '())
    (setq shell-file-name explicit-shell-file-name)
    )
  (when (eq wt-os-type 'mac)
    (setq explicit-shell-file-name "/bin/zsh")
    (setq shell-file-name explicit-shell-file-name)
    )
  )

;; shell
;; NOTE
;; https://github.com/tompurl/dot-emacs/blob/master/emacs-init.org#spell-checking
;; for windows settings
(use-package xterm-color
  :straight t
  :defer t
  :after eshell
  )

(use-package eshell
  :straight t
  :bind (:map eshell-mode-map
         ([remap recenter-top-bottom] . eshell/clear))
  :config
  (setq-local tab-width 4)
  (setq comint-prompt-read-only t)

  (with-no-warnings
    (defun eshell/clear ()
      "Clear the eshell buffer."
      (interactive)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (eshell-send-input)))

  ;; (add-hook 'eshell-first-time-mode-hook #'wt/eshell-configure)
  ;; (setq eshell-directory-name "~/.emacs.d/eshell/"
  ;; eshell-aliases-file (expand-file-name "~/.emacs.d/eshell/alias"))
  )
  )

(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t)
  ;; (setq eshell-visual-commands '("htop" "zsh" "vim"))
  ;; (setq eshell-visual-commands nil)
  )

;; (add-hook 'eshell-first-time-mode-hook
;;           (lambda ()
;;             (require 'eshell)
;;             (eshell/alias "la" "ls -al")  ; Set alias for git
;;             (eshell/alias "clear" "clear 1")  ; Set alias for git
;;             ))

(use-package eat
  ;; :if (eq system-type 'darwin)
  :straight t
  :defer t
  :after eshell
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-enable-shell-prompt-annotation nil)
  :config
  (eat-eshell-mode)
  (eat-eshell-visual-command-mode)
  (setq eshell-visual-commands '())
  )

;; (defun wt/eshell-configure ()
;;   (setq eshell-terminal-type nil)
;;   (push 'eshell-tramp eshell-modules-list)
;;   (push 'xterm-color-filter eshell-preoutput-filter-functions)
;;   (delq 'eshell-handle-ansi-color eshell-output-filter-functions)


;;   ;; Save command history when commands are entered
;;   ;; (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

;;   ;; (add-hook 'eshell-before-prompt-hook
;;   ;;           (lambda ()
;;   ;;             (setq xterm-color-preserve-properties t)))

;;   ;; Truncate buffer for performance
;;   (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

;;   ;; We want to use xterm-256color when running interactive commands
;;   ;; in eshell but not during other times when we might be launching
;;   ;; a shell command to gather its output.
;;   (add-hook 'eshell-pre-command-hook
;;             (lambda () (setenv "TERM" "xterm-256color")))
;;   ;; (add-hook 'eshell-post-command-hook
;;   ;;           (lambda () (setenv "TERM" "dumb")))

;;   (corfu-mode 0)
;;   (setq completion-in-region-function #'consult-completion-in-region)
;;   ;; (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

;;   ;; Initialize the shell history
;;   ;; (eshell-hist-initialize)

;;   ;; (if (featurep 'evil)
;;   ;;     (progn
;;   ;;       (require 'evil-collection-eshell)
;;   ;;       (evil-collection-eshell-setup)
;;   ;;       (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'consult-history)
;;   ;;       (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-n") 'eshell-bol)
;;   ;;       (evil-define-key '(normal visual) eshell-mode-map (kbd "C-l") 'eshell/clear)
;;   ;;       (evil-normalize-keymaps))
;;   ;;   (define-key eshell-mode-map (kbd "C-r") 'consult-history))

;;   ;; (setenv "PAGER" "cat")

;;   (setq
;;    ;; eshell-prompt-function      'wt/eshell-prompt
;;    eshell-prompt-regexp        "| "
;;    eshell-buffer-maximum-lines 50000
;;    eshell-history-size         500
;;    eshell-buffer-maximum-lines 500
;;    eshell-cmpl-cycle-completions nil
;;    ;; eshell-hist-ignoredups t
;;    ;; eshell-highlight-prompt t
;;    ;; eshell-scroll-to-bottom-on-input t
;;    ;; eshell-prefer-lisp-functions nil
;;    )
;;   )


;; (defun wt/switch-to-eshell ()
;;   (interactive)
;;   (if (project-current)
;;       (call-interactively #'project-eshell)
;;     (call-interactively #'eshell)))


(provide 'wt-term)
;;; wt-term.el ends here
