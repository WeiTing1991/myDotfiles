;;; termainals.el
;; (setq eshell-destroy-buffer-when-process-dies t)

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "C:/Program Files/PowerShell/7/pwsh.exe")
  (setq shell-file-name explicit-shell-file-name)
  (setq explicit-powershell-args '())
  (setq shell-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq comint-prompt-read-only t))

(defun read-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun wt/get-current-package-version ()
  (interactive)
  (let ((package-json-file (concat (eshell/pwd) "/package.json")))
    (when (file-exists-p package-json-file)
      (let* ((package-json-contents (read-file package-json-file))
             (package-json (ignore-errors (json-parse-string package-json-contents))))
        (when package-json
          (ignore-errors (gethash "version" package-json)))))))

(defun wt/map-line-to-status-char (line)
  (cond ((string-match "^?\\? " line) "?")))

(defun wt/get-git-status-prompt ()
  (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
    (seq-uniq (seq-filter 'identity (mapcar 'wt/map-line-to-status-char status-lines)))))

(defun wt/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
        (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

;; This prompt function mostly replicates my custom zsh prompt setup
;; that is powered by github.com/denysdovhan/spaceship-prompt.
(defun wt/eshell-prompt ()
  (let ((current-branch (magit-get-current-branch))
        (package-version (wt/get-current-package-version)))
    (concat
     "\n"
     (propertize (system-name) 'face `(:foreground "#62aeed"))
     (propertize " ॐ " 'face `(:foreground "white"))
     (propertize (wt/get-prompt-path) 'face `(:foreground "#82cfd3"))
     (when current-branch
       (concat
        (propertize " • " 'face `(:foreground "white"))
        (propertize (concat " " current-branch) 'face `(:foreground "#c475f0"))))
     (when package-version
       (concat
        (propertize " @ " 'face `(:foreground "white"))
        (propertize package-version 'face `(:foreground "#e8a206"))))
     (propertize " • " 'face `(:foreground "white"))
     (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground "#5a5b7f"))
     (if (= (user-uid) 0)
         (propertize "\n#" 'face `(:foreground "red2"))
       (propertize "\nλ" 'face `(:foreground "#aece4a")))
     (propertize " " 'face `(:foreground "white")))))


(use-package xterm-color
  :straight t
)

(defun wt/eshell-configure ()
  ;; Make sure magit is loaded
  (require 'magit)

  (setq eshell-terminal-type nil)

  (push 'eshell-tramp eshell-modules-list)
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; We want to use xterm-256color when running interactive commands
  ;; in eshell but not during other times when we might be launching
  ;; a shell command to gather its output.
  (add-hook 'eshell-pre-command-hook
            (lambda () (setenv "TERM" "xterm-256color")))

  (add-hook 'eshell-post-command-hook
            (lambda () (setenv "TERM" "dumb")))

  ;; Use completion-at-point to provide completions in eshell
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

  ;; Initialize the shell history
  (eshell-hist-initialize)

  (if (featurep 'evil)
      (progn
        (require 'evil-collection-eshell)
        (evil-collection-eshell-setup)
        (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'consult-history)
        (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
        (evil-define-key '(normal visual) eshell-mode-map (kbd "C-l") 'eshell/clear)
        (evil-normalize-keymaps))
    (define-key eshell-mode-map (kbd "C-r") 'consult-history))

  (setenv "PAGER" "cat")

  (setq eshell-prompt-function      'wt/eshell-prompt
        eshell-prompt-regexp        "^λ "
        eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-highlight-prompt t
        eshell-scroll-to-bottom-on-input t
        eshell-prefer-lisp-functions nil)
  )


(use-package eshell
  :config
  (add-hook 'eshell-first-time-mode-hook #'wt/eshell-configure)
  (setq eshell-directory-name "~/.emacs.d/eshell/"
  eshell-aliases-file (expand-file-name "~/.emacs.d/eshell/alias"))
)

(add-hook 'eshell-mode-hook
          (lambda ()
            (require 'eshell)
            (eshell/alias "la" "ls -al")  ; Set alias for git
            (eshell/alias "clear" "clear 1")  ; Set alias for git
            ))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :demand t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(defun wt/switch-to-eshell ()
  (interactive)
  (if (project-current)
      (call-interactively #'project-eshell)
    (call-interactively #'eshell)))

(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-visual-commands '("htop" "zsh" "vim"))
  ;; (setq eshell-visual-commands nil)
  )

(use-package pcmpl-args
  :demand t
  :after eshell)

(use-package eshell-syntax-highlighting
  :after eshell
  :demand t
  :config
  (eshell-syntax-highlighting-global-mode +1))

(defun wt/esh-autosuggest-setup ()
  (require 'company)
  (set-face-foreground 'company-preview-common "#4b5668")
  (set-face-background 'company-preview nil))

(use-package esh-autosuggest
  :defer t
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-delay 0.5))

(use-package eat
  :after eshell
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-enable-shell-prompt-annotation nil)
  :config
  (eat-eshell-mode)
  (eat-eshell-visual-command-mode)
  (setq eshell-visual-commands '()))

;; (use-package vterm
;;   :commands vterm
;;   :config
;;   (setq vterm-max-scrollback 10000)
;;   (when (featurep 'evil)
;;     (advice-add 'evil-collection-vterm-insert :before #'vterm-reset-cursor-point)))

;;; terminals code end here
