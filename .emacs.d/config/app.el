;;; app.el

;; (eval-and-compile
;;   (add-to-list 'load-path (expand-file-name "site-lisp/emacs-application-framework" user-emacs-directory)))

(use-package eaf
  :load-path "~/.dotfiles/.emacs.d/site-lisp/emacs-application-framework"
  :custom
  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  (defalias 'browse-web #'eaf-open-browser)
  (require 'eaf-browser)
  (require 'eaf-pdf-viewer)
  ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  ) ;; unbind, see more in the Wiki

(setq eaf-browser-chrome-browser-name "Brave")

;;;
