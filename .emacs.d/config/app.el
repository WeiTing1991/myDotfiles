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

;; (setq doc-view-continuous t)

;; pandoc
(setq pandoc-binary "/opt/homebrew/bin/pandoc")

(require 'dired)

;; Replace with the path to your PDF viewer
(setq my-pdf-viewer "C:/Program Files (x86)/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe")

(defun my-open-pdf-with-external-viewer (file)
  "Open PDF FILE with an external viewer."
  (start-process "pdf-viewer" nil my-pdf-viewer file))

;; Set Dired to use the external viewer for PDF files
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode 1) ;; Optional: hide certain files
            (define-key dired-mode-map "e" 'my-open-pdf-with-external-viewer)))

;; (setq auto-mode-alist (append '(("\\.pdf\\'" . doc-view-mode)) auto-mode-alist))

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
