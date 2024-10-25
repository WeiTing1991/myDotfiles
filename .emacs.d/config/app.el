;;; app.el

;;; code:
;; Spell
;; NOTE https://github.com/redguardtoo/wucuo?tab=readme-ov-file
(setq ispell-program-name "aspell")
;; You could add extra option "--camel-case" for camel case code spell checking if Aspell 0.60.8+ is installed
;; @see https://github.com/redguardtoo/emacs.d/issues/796
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))

(flyspell-mode -1)
(use-package wucuo
  :straight t
  :defer t
  :config
  ;; (add-hook 'prog-mode-hook #'wucuo-start)
  (add-hook 'markdon-mode-hook #'wucuo-start)
  (add-hook 'markdon-mode-hook 'flycheck-mode-hook)
  (add-hook 'org-mode-hook 'wucuo-start)
  (add-hook 'org-mode-hook 'flycheck-mode-hook)
  )

(use-package olivetti
  :straight t
  :defer t
  )
(declare-function olivetti-mode "olivetti")
(wt/leader-keys
  "mo" '(olivetti-mode :wk "writing focus mode")
  )

;; pandoc
(cond
 ((eq system-type 'darwin)  ;; 'darwin' is for macOS
  (setq pandoc-binary "/opt/homebrew/bin/pandoc")
  )
 ((eq system-type 'windows-nt)  ;; 'windows-nt' is for Windows
  (setq pandoc-binary "/AppData/Local/Pandoc/pandoc.exe")
  )
 )


;; load the config for markdown and org
(load-file (expand-file-name "./config/note/md.el" user-emacs-directory))
(load-file (expand-file-name "./config/note/org.el" user-emacs-directory))




(provide 'app)
;;; app.el ends here
