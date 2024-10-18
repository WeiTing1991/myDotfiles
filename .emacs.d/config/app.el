;;; app.el
;;; code

;; pdf
;; https://github.com/vedang/pdf-tools
(use-package pdf-tools
  :straight t
  :config
  ;; Initialize the PDF tools server
  (pdf-tools-install)
  (pdf-loader-install)
  ;; Automatically open PDFs in pdf-view-mode
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))

;;pandoc
(setq pandoc-binary "/opt/homebrew/bin/pandoc")
(setq auto-mode-alist (append '(("\\.docx\\'" . doc-view-mode)) auto-mode-alist))
