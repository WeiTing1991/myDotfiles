;;; app.el

;;; code
;; Spell
;; NOTE https://github.com/redguardtoo/wucuo?tab=readme-ov-file
(setq ispell-program-name "aspell")
;; You could add extra option "--camel-case" for camel case code spell checking if Aspell 0.60.8+ is installed
;; @see https://github.com/redguardtoo/emacs.d/issues/796
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))

(flyspell-mode -1)
(use-package wucuo
  :straight t
  :config
  ;; (add-hook 'prog-mode-hook #'wucuo-start)
  (add-hook 'org-mode-hook #'wucuo-start)
  (add-hook 'markdon-mode-hook #'wucuo-start)
  )

;; file open
;; checke this in windows
;; https://emacs.stackexchange.com/questions/3105/how-to-use-an-external-program-as-the-default-way-to-open-pdfs-from-emacs
;; Function to open a file with the system's default application
(defun my-dired-open-with-system (file)
  "Open FILE with the system's default application."
  (w32-shell-execute "open" file))
;; Function to open the marked file in Dired
(defun my-dired-open ()
  "Open the marked file with the system's default application."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if file
        (my-dired-open-with-system file)
      (message "No file selected."))))

(evil-collection-define-key 'normal 'dired-mode-map (kbd "C-o") 'my-dired-open)

;; pandoc
(setq pandoc-binary "/opt/homebrew/bin/pandoc")

;; org mode
;; https://doc.norang.ca/org-mode.html
(use-package org
  :straight nil
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  )

;; (setq org-agenda-files '("~/org"))



(provide 'app)
;;; app.el ends here
