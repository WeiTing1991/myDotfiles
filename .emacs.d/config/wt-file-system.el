;;; wt-file-system.el --- command file system. -*- lexical-binding: t -*-

;; code:
(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (:map dired-mode-map
              ("C-x C-i" . dired-toggle-read-only)
              ("C-x C-j" . dired-jump)
              )

  :config
  (setq dired-dwim-target t)
  (setq dired-kill-when-opening-new-dired-buffer t)

  ;; Show directory first
  (setq dired-listing-switches "-alh --group-directories-first")

  (when (eq wt-os-type 'mac)
    (if (executable-find "gls")
        (progn
          ;; Use GNU ls as `gls' from `coreutils' if available.
          (setq insert-directory-program "gls")
          ;; Using `insert-directory-program'
          (setq ls-lisp-use-insert-directory-program t))
      (progn
        ;; Suppress the warning: `ls does not support --dired'.
        (setq dired-use-ls-dired nil)
        (setq dired-listing-switches "-alh"))))

  ;;keybinding
  (evil-collection-define-key 'normal 'dired-mode-map (kbd "n") 'dired-create-empty-file)
  (evil-collection-define-key 'normal 'dired-mode-map (kbd "N") 'dired-create-directory)
  (evil-collection-define-key 'normal 'dired-mode-map (kbd "D") 'dired-do-delete)
  (evil-collection-define-key 'normal 'dired-mode-map (kbd "r") 'dired-do-rename)
  (evil-collection-define-key 'normal 'dired-mode-map (kbd "y") 'dired-do-copy)

  (evil-collection-define-key 'normal 'dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-collection-define-key 'normal 'dired-mode-map (kbd "l") 'dired-open-file)

  )

;; File open from dired
(defun my-dired-open-with-system (file)
  "Open FILE with the system's default application."
  (cond
   ((eq wt-os-type 'mac)
    ;; macOS
    (start-process "open" nil "open" file)
    )
   ((eq wt-os-type 'window)
    (w32-shell-execute "open" file))
   )
  )

(defun wt-dired-open ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if file
        (wt-dired-open-with-system file)
      (message "No file selected."))))

(evil-collection-define-key 'normal 'dired-mode-map (kbd "C-o") 'wt-dired-open)

;; NOTE
;; https://github.com/Fuco1/dired-hacks
(use-package dired-open
  :straight t
  :defer t
  :after dired
  :commands (dired dired-jump)
  )

(use-package dired-hide-dotfiles
  :straight t
  :defer t
  :after dired
  :init
  (setq dired-hide-dotfiles-mode -1)
  ;; :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map (kbd "H") 'dired-hide-dotfiles-mode)
  )

(use-package dired-preview
  :straight t
  :defer t
  :after dired
  :config
  (evil-define-key 'normal dired-mode-map (kbd "C-p") 'dired-preview-mode)

  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-preview-delay 0.1)
  (setq dired-preview-max-size (* 100 1024 1024))
  (setq dired-preview-ignored-extensions-regexp
        (concat "\\."
                "\\(gz\\|"
                "zst\\|"
                "tar\\|"
                "xz\\|"
                "rar\\|"
                "zip\\|"
                "iso\\|"
                "epub"
                "\\)")
        )
  )

;; Allow rsync from dired buffers
(use-package dired-rsync
  :straight t
  :defer t
  :after dired
  :bind (:map dired-mode-map
              ("C-x C-r" . dired-rsync))
)

;; TODO move to bella color
(use-package dired-rainbow
  :straight t
  ;; :defer t
  :after dired
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ECEFF4" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "white" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#ECEFF4" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    ))

;;; buffer
(use-package ibuffer
  :straight nil
  :config
  :init (setq ibuffer-filter-group-name-face 'bold)
  )

(use-package ibuffer-project
  :straight t
  :defer t
  :hook (ibuffer . (lambda ()
                     "Group ibuffer's list by project."
                     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
                     (unless (eq ibuffer-sorting-mode 'project-file-relative)
                       (ibuffer-do-sort-by-project-file-relative))))
  :init (setq ibuffer-project-use-cache t)
  :config
  (defun my-ibuffer-project-group-name (root type)
    "Return group name for project ROOT and TYPE."
    (if (and (stringp type) (> (length type) 0))
        (format "%s %s" type root)
      (format "%s" root)))
  (if (display-graphic-p)
      (progn
        (advice-add #'ibuffer-project-group-name :override #'my-ibuffer-project-group-name)
        (setq ibuffer-project-root-functions
              `((ibuffer-project-project-root . ,(all-the-icons-octicon "repo" :height 1.2 :face ibuffer-filter-group-name-face))
                (file-remote-p . ,(all-the-icons-octicon "radio_tower" :height 1.2 :face ibuffer-filter-group-name-face)))))
    (progn
      (advice-remove #'ibuffer-project-group-name #'my-ibuffer-project-group-name)
      (setq ibuffer-project-root-functions
            '((ibuffer-project-project-root . "Project")
              (file-remote-p . "Remote")))))
  )

;; TODO fork and do the pull request
(use-package all-the-icons-ibuffer
  :straight t
  :defer t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
  :custom
  (setq all-the-icons-ibuffer-display-predicate #'display-graphic-p)
  (setq  all-the-icons-ibuffer-human-readable-size t)
  ;; Slow Rendering
  (setq inhibit-compacting-font-caches t)
  )

;; workspace

(use-package tabspaces
  :straight (:type git :host github :repo "mclear-tools/tabspaces")
  :defer t
  ;; use this only if you want the minor-mode loaded at startup.
  :hook (after-init . tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tab-bar-show t)

  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  ;; (tab-bar-new-tab-choice "*scratch*")
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))

  ;; (tabspaces-initialize-project-with-todo t)
  ;; (tabspaces-todo-file-name "project-todo.org")
  ;; sessions
  ;; (tabspaces-session t)
  ;; (tabspaces-session-auto-restore t)
  :init
  ;; Filter Buffers for Consult-Buffer
  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; set consult-workspace buffer list
    (defvar consult--source-workspace
      (list :name     "Workspace Buffer"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))
      "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace))

  )

(with-eval-after-load 'tabspaces
  (cond
   ((eq wt-os-type 'mac)
    (defvar wt-tab-font-size 140)
    )
   ((eq wt-os-type 'linux)
    (defvar wt-tab-font-size 160)
    )
   ((eq wt-os-type 'windows)
    (defvar wt-tab-font-size 100)
    )
   )
  (set-face-attribute 'tab-bar nil
                      ;; :font "RobotoMono Nerd Font"
                      :weight 'bold
                      :background bella-color-black
                      :foreground bella-color-white
                      :height wt-tab-font-size)

  ;; Customize the appearance of active tabs
  (set-face-attribute 'tab-bar-tab nil
                      :background bella-color-black-blue
                      :foreground bella-color-white
                      :weight 'bold
                      :box nil
                      )
  ;; Customize the appearance of inactive tabs
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :background bella-color-black
                      :foreground bella-color-base
                      :box nil
                      )
  )
(provide 'wt-file-system)
;;; wt-file-system.el ends here
