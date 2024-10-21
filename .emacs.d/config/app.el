;;; app.el

;;; code

;; pdf
;; https://github.com/vedang/pdf-tools
;; (use-package pdf-tools
;;   :straight t
;;   :config
;;   ;; Initialize the PDF tools server
;;   (pdf-tools-install)
;;   (pdf-loader-install)
;;   ;; Automatically open PDFs in pdf-view-mode
;;   (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))

(setq doc-view-continuous t)

;; pandoc
(setq pandoc-binary "/opt/homebrew/bin/pandoc")
(setq auto-mode-alist (append '(("\\.docx\\'" . doc-view-mode)) auto-mode-alist))

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
