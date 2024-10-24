;;; termainals.el

;;; code:
;; git tool
(use-package magit
  :straight t
  :defer t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package term
  :straight nil
  :config
  (when (eq system-type 'windows-nt)
    (setq explicit-shell-file-name "C:/Program Files/PowerShell/7/pwsh.exe")
    (setq shell-file-name explicit-shell-file-name)
    )
  (when (eq system-type 'darwin)
    (setq explicit-shell-file-name "/bin/zsh")
    (setq shell-file-name explicit-shell-file-name)
    )
  )
;; shell
;; TODO this https://github.com/tompurl/dot-emacs/blob/master/emacs-init.org#spell-checking
;; for windows settings
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :after eshell
  :demand t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;; Eshell
;; (defun read-file (file-path)
;;   (with-temp-buffer
;;     (insert-file-contents file-path)
;;     (buffer-string)))

;; (defun wt/get-current-package-version ()
;;   (interactive)
;;   (let ((package-json-file (concat (eshell/pwd) "/package.json")))
;;     (when (file-exists-p package-json-file)
;;       (let* ((package-json-contents (read-file package-json-file))
;;              (package-json (ignore-errors (json-parse-string package-json-contents))))
;;         (when package-json
;;           (ignore-errors (gethash "version" package-json)))))))

;; (defun wt/map-line-to-status-char (line)
;;   (cond ((string-match "^?\\? " line) "?")))

;; (defun wt/get-git-status-prompt ()
;;   (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
;;     (seq-uniq (seq-filter 'identity (mapcar 'wt/map-line-to-status-char status-lines)))))

;; (defun wt/get-prompt-path ()
;;   (let* ((current-path (eshell/pwd))
;;          (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
;;          (has-path (not (string-match "^fatal" git-output))))
;;     (if (not has-path)
;;         (abbreviate-file-name current-path)
;;       (string-remove-prefix (file-name-directory git-output) current-path))))

;; ;; that is powered by github.com/denysdovhan/spaceship-prompt.
;; (defun wt/eshell-prompt ()
;;   (let ((current-branch (magit-get-current-branch))
;;         (package-version (wt/get-current-package-version)))
;;     (concat
;;      "\n"
;;      (propertize (system-name) 'face `(:foreground "#62aeed"))
;;      (propertize " ? " 'face `(:foreground "white"))
;;      (propertize (wt/get-prompt-path) 'face `(:foreground "#82cfd3"))
;;      (when current-branch
;;        (concat
;;         (propertize " • " 'face `(:foreground "white"))
;;         (propertize (concat "|| " current-branch) 'face `(:foreground "#c475f0"))))
;;      (when package-version
;;        (concat
;;         (propertize " @ " 'face `(:foreground "white"))
;;         (propertize package-version 'face `(:foreground "#e8a206"))))
;;      (propertize " • " 'face `(:foreground "white"))
;;      (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground "#5a5b7f"))
;;      (if (= (user-uid) 0)
;;          (propertize "\n#" 'face `(:foreground "red2"))
;;        (propertize "\n?" 'face `(:foreground "#aece4a"))
;;        )
;;      (propertize " " 'face `(:foreground "white")))))

(use-package xterm-color
  :straight t
  )

(defun wt/eshell-configure ()

  (setq eshell-terminal-type nil)
  ;; (push 'eshell-tramp eshell-modules-list)
  ;; (push 'xterm-color-filter eshell-preoutput-filter-functions)
  ;; (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

  ;; Save command history when commands are entered
  ;; (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; (add-hook 'eshell-before-prompt-hook
  ;;           (lambda ()
  ;;             (setq xterm-color-preserve-properties t)))

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; We want to use xterm-256color when running interactive commands
  ;; in eshell but not during other times when we might be launching
  ;; a shell command to gather its output.
  (add-hook 'eshell-pre-command-hook
            (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            (lambda () (setenv "TERM" "dumb")))

  (corfu-mode 0)
  (setq completion-in-region-function #'consult-completion-in-region)
  ;; (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

  ;; Initialize the shell history
  ;; (eshell-hist-initialize)

  ;; (if (featurep 'evil)
  ;;     (progn
  ;;       (require 'evil-collection-eshell)
  ;;       (evil-collection-eshell-setup)
  ;;       (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'consult-history)
  ;;       (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-n") 'eshell-bol)
  ;;       (evil-define-key '(normal visual) eshell-mode-map (kbd "C-l") 'eshell/clear)
  ;;       (evil-normalize-keymaps))
  ;;   (define-key eshell-mode-map (kbd "C-r") 'consult-history))

 ;; (setenv "PAGER" "cat")

  (setq
   ;; eshell-prompt-function      'wt/eshell-prompt
   eshell-prompt-regexp        "| "
   eshell-buffer-maximum-lines 50000
   eshell-history-size         500
   eshell-buffer-maximum-lines 500
   eshell-cmpl-cycle-completions nil
   ;; eshell-hist-ignoredups t
   ;; eshell-highlight-prompt t
   ;; eshell-scroll-to-bottom-on-input t
   ;; eshell-prefer-lisp-functions nil
   )
)

;; (defun corfu-send-shell (&rest _)
;;   "Send completion candidate when inside comint/eshell."
;;   (cond
;;    ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
;;     (eshell-send-input))
;;    ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
;;     (comint-send-input))))
;; (advice-add #'corfu-insert :after #'corfu-send-shell)

(use-package eshell-syntax-highlighting
  :defer t
  :after eshell
  :hook(eshell-mode . eshell-syntax-highlighting-mode)
  ;; :demand t
  )

(use-package esh-autosuggest
  :straight t
  :defer t
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-delay 0)
  )

(use-package eshell
  :straight t
  :config
  (add-hook 'eshell-first-time-mode-hook #'wt/eshell-configure)
  ;; (setq eshell-directory-name "~/.emacs.d/eshell/"
  ;; eshell-aliases-file (expand-file-name "~/.emacs.d/eshell/alias"))
  )

;; (defun corfu-send-shell (&rest _)
;;   "Send completion candidate when inside comint/eshell."
;;   (require 'corfu)
;;   (cond
;;    ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
;;     (eshell-send-input))
;;    ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
;;     (comint-send-input))))
;; (advice-add #'corfu-insert :after #'corfu-send-shell)

(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t)
  ;; (setq eshell-visual-commands '("htop" "zsh" "vim"))
  ;; (setq eshell-visual-commands nil)
)







;; (add-hook 'eshell-mode-hook
;;           (lambda ()
;;             (require 'eshell)
;;             (eshell/alias "la" "ls -al")  ; Set alias for git
;;             (eshell/alias "clear" "clear 1")  ; Set alias for git
;;             ))

;; (defun wt/switch-to-eshell ()
;;   (interactive)
;;   (if (project-current)
;;       (call-interactively #'project-eshell)
;;     (call-interactively #'eshell)))

;; ;; (defun wt/esh-autosuggest-setup ()
;; ;;   (require 'company)
;; ;;   (set-face-foreground 'company-preview-common "#4b5668")
;; ;;   (set-face-background 'company-preview nil))

;;https://www.reddit.com/r/emacs/comments/m3cx27/powershellwindows_terminal_from_emacs/
;; ;;https://github.com/jschaf/powershell.el
;; ;; (use-package powershell
;; ;;   :if (eq system-type 'windows-nt)
;; ;;   :straight t
;; ;;   :hook (eshell-mode . powershell-mode)
;; ;;   )

;; ;; (use-package eat
;; ;;   :straight t
;; ;;   :after eshell
;; ;;   :custom
;; ;;   (eat-kill-buffer-on-exit t)
;; ;;   (eat-enable-shell-prompt-annotation nil)
;; ;;   :config
;; ;;   (eat-eshell-mode)
;; ;;   (eat-eshell-visual-command-mode)
;; ;;   (setq eshell-visual-commands '())
;; ;;   )

;; (use-package em-cmpl
;;   :straight nil
;;   :config
;;   (bind-key "C-M-i" nil eshell-cmpl-mode-map)
;;   (defun my/em-cmpl-mode-hook ()
;;     (setq completion-at-point-functions
;;           (list #'cape-history #'cape-file #'cape-dabbrev)))
;;   (add-hook 'eshell-cmpl-mode-hook #'my/em-cmpl-mode-hook)
;; )

;; (use-package corfu-terminal
;;   :if (not (display-graphic-p))
;;   :straight t
;;   :hook (eshell-mode . corfu-terminal-mode)
;;   )

;; (use-package vterm
;;   :commands vterm
;;   :config
;;   (setq vterm-max-scrollback 10000)
;;   (when (featurep 'evil)
;;     (advice-add 'evil-collection-vterm-insert :before #'vterm-reset-cursor-point)))

(provide 'terminal)

;;; terminals code end here
