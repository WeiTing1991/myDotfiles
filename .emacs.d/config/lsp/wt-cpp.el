;;; wt-cpp.el --- markdown setting.| -*- lexical-binding: t -*-

;;; code:
;; c/cpp
;; Note check here https://config.phundrak.com/emacs/packages/programming.html#caddy
;; https://github.com/emacs-exordium/exordium/blob/master/modules/init-cpp.el

;; (use-package cc-mode
;;   :defer t
;;   :straight nil
;;   :hook ((c++-mode . lsp-deferred)
;;          (c++-mode . #'tree-sitter-hl-mode)
;;          ;; (c-mode . #'tree-sitter-hl-mode)
;;          )
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;;   )

;; (use-package cmake-mode
;;   :defer t
;;   :straight t
;;   :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
;;          ("\\.cmake\\'" . cmake-mode)))

;;https://github.com/ludwigpacifici/modern-cpp-font-lock
                                        ;(use-package modern-cpp-font-lock
                                        ;  :straight t
                                        ;  :defer t
                                        ;  :diminish modern-c++-font-lock-mode
                                        ;  :hook (c++-mode . modern-c++-font-lock-mode)
                                        ;)
(message "CPP No implement")

(provide 'wt-cpp)

;;; wt-cpp.el ends here
