
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

