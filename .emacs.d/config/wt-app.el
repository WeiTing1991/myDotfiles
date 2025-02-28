;;; wt-app.el --- application.| -*- lexical-binding: t -*-

;;; code:
;; Spell
;; NOTE https://github.com/redguardtoo/wucuo?tab=readme-ov-file

(setq flyspell-mode nil)
(setq flyspell-prog-mode nil)

(use-package wucuo
  :straight t
  :defer t
  :init
  (setq ispell-program-name "aspell")
  ;; You could add extra option "--camel-case" for camel case code spell checking if Aspell 0.60.8+ is installed
  ;; @see https://github.com/redguardtoo/emacs.d/issues/796
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))
  :hook ((org-mode markdown-mode) . #'wucuo-start)
  :bind ("C-c f" . #'wucuo-start)
  )

;; center mode
(use-package olivetti
  :straight t
  :defer t
  :config
  (setq
   olivetti-minimum-body-width 100
   olivetti-body-width 150
   olivetti-style 'fancy
   olivetti-margin-width 8)
  ;; (set-face-background 'fringe bella-color-base)
  )


;; TODO:
;; pandoc
;; (cond
;;  ((eq system-type 'darwin)  ;; 'darwin' is for macOS
;;   (setq pandoc-binary "/opt/homebrew/bin/pandoc")
;;   )
;;  ((eq system-type 'windows-nt)  ;; 'windows-nt' is for Windows
;;   (setq pandoc-binary "/AppData/Local/Pandoc/pandoc.exe")
;;   )
;;  )

;; https://github.com/copilot-emacs/copilot.el
;; setttings
"
Ensure your Emacs version is at least 27, the dependency package editorconfig (melpa) and jsonrpc (elpa, >= 1.0.14) are both installed.
Install Node.js v18+. (You can specify the path to node executable by setting copilot-node-executable.)
Setup copilot.el as described in the next section.
Install the copilot server by M-x copilot-install-server.
Login to Copilot by M-x copilot-login. You can also check the status by M-x copilot-diagnose (NotAuthorized means you don't have a valid subscription).
"
;; NOTE https://systemcrafters.net/live-streams/march-31-2023/
;; https://github.com/s-kostyaev/ellama
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :defer t
  :config
  ;; (define-key 'insert copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  ;; (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-l") 'copilot-accept-completion)

  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))

  )

;; keybinding
(with-eval-after-load 'evil

  (wt/leader-keys
    "mo" '(olivetti-mode :wk "writing focus mode")
    )
  (wt/system-key
    "c" '(copilot-mode :wk "Enable copilot mode")
    )
)

(provide 'wt-app)
;;; wt-app.el ends here
