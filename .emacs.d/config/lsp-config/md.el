;;; markdown

(use-package markdown-mode
  :defer t
  :straight t
  :mode (("README\\.md\\'" . markdon-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  ;; :init (setq markdown-command "multimarkdown"))
  :custom
  (markdown-header-scaling t)
  ;; (markdown-hide-urls t)
  (markdown-fontify-code-blocks-natively t)
  )

;; custom-visual mode

(custom-set-faces
 '(markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
 '(markdown-header-face-1 ((t (:height 1.8 :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face))))
 '(markdown-header-face-2 ((t (:height 1.4 :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face))))
 '(markdown-header-face-3 ((t (:height 1.2 :foreground "#D08770" :weight extra-bold :inherit markdown-header-face))))
 '(markdown-header-face-4 ((t (:height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face))))
 '(markdown-header-face-5 ((t (:height 1.1 :foreground "#b48ead" :weight bold :inherit markdown-header-face))))
 '(markdown-header-face-6 ((t (:height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face))))
 '(markdown-code-face ((t (:background "#2E2E2E" :foreground "#FFFFFF"))))  ;; Change colors as needed
 ;; '(markdown-inline-code-face ((t (:background "#2E2E2E" :foreground "#FFFFFF" :weight bold))))  ;; Inline code
 )

(defvar wt/current-line '(0 . 0)
  "(start . end) of current line in current buffer")
(make-variable-buffer-local 'wt/current-line)

(defun wt/unhide-current-line (limit)
  "Font-lock function"
  (let ((start (max (point) (car wt/current-line)))
        (end (min limit (cdr wt/current-line))))
    (when (< start end)
      (remove-text-properties start end
                              '(invisible t display "" composition ""))
      (goto-char limit)
      t)))

(defun wt/refontify-on-linemove ()
  "Post-command-hook"
  (let* ((start (line-beginning-position))
         (end (line-beginning-position 2))
         (needs-update (not (equal start (car wt/current-line)))))
    (setq wt/current-line (cons start end))
    (when needs-update
      (font-lock-fontify-block 3))))

(defun wt/markdown-unhighlight ()
  "Enable markdown concealling"
  (interactive)
  (markdown-toggle-markup-hiding 'toggle)
  (font-lock-add-keywords nil '((wt/unhide-current-line)) t)
  (add-hook 'post-command-hook #'wt/refontify-on-linemove nil t))


(defun markdown-view-mode-maybe ()
  (cond ((and (eq major-mode 'markdown-mode) buffer-read-only) (markdown-view-mode))
        ((and (eq major-mode 'markdown-view-mode) (not buffer-read-only)) (markdown-mode))))


;; (add-hook 'markdown-mode-hook #'wt/markdown-unhighlight)
(add-hook 'read-only-mode-hook 'markdown-view-mode-maybe)

(if (equal major-mode 'markdown-view-mode)
    (local-set-key (kbd "C-x C-q") 'markdown-mode))
(if (equal major-mode 'markdown-mode)
    (local-set-key (kbd "C-x C-q") 'markdown-view-mode))

;;; md.el
