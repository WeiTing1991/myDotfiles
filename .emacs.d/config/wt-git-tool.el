;;; wt-git-tool.el --- markdown setting.| -*- lexical-binding: t -*-

;;; code:

(use-package magit
  :if (or (eq wt-os-type 'mac) (eq wt-os-type 'mac))
  :straight t
  :defer t
  :commands magit-status
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :init (setq magit-diff-refine-hunk t)

  :config
  ;; (when sys/win32p
  ;;   (setenv "GIT_ASKPASS" "git-gui--askpass"))

  ;; Unbind M-1, M-2, M-3, and M-4 shortcuts due to conflict with `ace-window'
  (unbind-key "M-1" magit-mode-map)
  (unbind-key "M-2" magit-mode-map)
  (unbind-key "M-3" magit-mode-map)
  (unbind-key "M-4" magit-mode-map)

  ;; Access Git forges from Magit
  (use-package forge
    :straight t
    :after magit
    :custom-face
    (forge-topic-label ((t (:inherit variable-pitch :height 0.9 :width condensed :weight regular :underline nil))))
    :init (setq forge-topic-list-columns
                '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
                  ("Title" 60 t nil title  nil)
                  ("State" 6 t nil state nil)
                  ("Updated" 10 t nil updated nil)))))

;; TODO
;; ;; Display transient in child frame
;; (when (childframe-completion-workable-p)
;;   (use-package transient-posframe
;;     :diminish
;;     :defines posframe-border-width
;;     :custom-face
;;     (transient-posframe ((t (:inherit tooltip))))
;;     (transient-posframe-border ((t (:inherit posframe-border :background unspecified))))
;;     :hook (after-init . transient-posframe-mode)
;;     :init
;;     (setq transient-posframe-border-width posframe-border-width
;;           transient-posframe-min-height nil
;;           transient-posframe-min-width 80
;;           transient-posframe-poshandler 'posframe-poshandler-frame-center
;;           transient-posframe-parameters '((left-fringe . 8)
;;                                           (right-fringe . 8)))
;;     :config
;;     (with-no-warnings
;;       ;; FIXME:https://github.com/yanghaoxie/transient-posframe/issues/5#issuecomment-1974871665
;;       (defun my-transient-posframe--show-buffer (buffer _alist)
;;         "Show BUFFER in posframe and we do not use _ALIST at this period."
;;         (when (posframe-workable-p)
;;           (let* ((posframe
;;                   (posframe-show buffer
;;                                  :height (with-current-buffer buffer (1- (count-screen-lines (point-min) (point-max))))
;;                                  :font transient-posframe-font
;;                                  :position (point)
;;                                  :poshandler transient-posframe-poshandler
;;                                  :background-color (face-attribute 'transient-posframe :background nil t)
;;                                  :foreground-color (face-attribute 'transient-posframe :foreground nil t)
;;                                  :min-width transient-posframe-min-width
;;                                  :min-height transient-posframe-min-height
;;                                  :internal-border-width transient-posframe-border-width
;;                                  :internal-border-color (face-attribute 'transient-posframe-border :background nil t)
;;                                  :override-parameters transient-posframe-parameters)))
;;             (frame-selected-window posframe))))
;;       (advice-add #'transient-posframe--show-buffer :override #'my-transient-posframe--show-buffer)

;;       (defun my-transient-posframe--hide ()
;;         "Hide transient posframe."
;;         (posframe-hide transient--buffer-name))
;;       (advice-add #'transient-posframe--delete :override #'my-transient-posframe--hide))))


(provide 'wt-git-tool)

;;; wt-git-tool.el ends here
