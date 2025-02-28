;;; wt-python.el --- markdown setting.| -*- lexical-binding: t -*-

;;; code:

;; python
;; Install: pip install pyflakes autopep8
(use-package python-mode
  :straight nil
  :defer t
  :hook (python-mode . lsp-deferred)
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)

  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))
  )

(use-package python-ts-mode
  :straight nil
  :defer t
  :hook (python-ts-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3")
  ;; (when (eq system-type 'windows-nt)
  ;;   (setq python-shell-interpreter "C://Users//weitingche//anaconda//python.exe")
  ;;   )
  )

(use-package lsp-pyright
  :straight t
  :defer t
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3"))
  :custom (lsp-pyright-langserver-command "pyright")
  :hook ((python-mode python-ts-mode) . (lambda ()
                                          (require 'lsp-pyright)
                                          (lsp-deferred)))
  )

;; (use-package conda
;;   :straight t
;;   :defer t
;;   :hook(python-ts-mode . conda-env-autoactivate-mode)
;;   :config
;;   (conda-env-initialize-interactive-shells)
;;   (conda-env-initialize-eshell))

;; (use-package pyenv-mode
;;   :straight t
;;   :defer t
;;   :hook (python-mode .pyenv-mode)
;;   )

(provide 'wt-python)

;;; wt-python.el ends here
