;;; org.el

;; nice bullets
;; (use-package org-bullets
;;   :straight nil
;;   :defer t
;;   :hook (org-mode . org-bullets-mode)
;;   :custom
;;   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


;; TODO check those link

(use-package org-superstar
  :straight t
  :defer t
  :hook (org-mode . org-superstar-mode)
  :config
  ;; Enable custom bullets for TODO items
  (setq org-superstar-special-todo-items t)
  ;; (setq org- t)
  ;; '(("TODO" . ?☐)
  ;;   ("DONE" . ?☑)
  )


;;https://github.com/daviwil/emacs-from-scratch/blob/master/Emacs.org

(setq org-roam-directory
      (cond
       ((eq system-type 'darwin)
        "/Users/weitingchen/Library/Mobile Documents/iCloud~md~obsidian/Documents/weitingchen/")
       ((eq system-type 'windows-nt)
        "~/iCloudDrive/iCloud~md~obsidian/weitingchen/")))

(use-package org-roam
  :straight t
  :after org
  :custom
  (org-roam-directory (file-truename org-roam-directory))

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config

  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)
  )

(provide 'org)
;;; org.el ends here
