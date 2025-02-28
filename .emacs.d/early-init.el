;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <https://www.gnu.org/licenses/>.

;;; Code:

;; Performance
(setq package-enable-at-startup nil)

(setq gc-cons-threshold (* 512 1024 1024))
(setq gc-cons-threshold most-positive-fixnum)

;; (add-hook 'emacs-startup-hook
;;     (lambda ()
;;       (setq gc-cons-threshold (* 512 1024 1024)
;;             gc-cons-percentage 1.0
;;             )
;;       )
;;     )

(setq idle-update-delay 0.500)

(setq native-comp-async-report-warnings-errors 'silent)

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (setq native-comp-deferred-compilation t)
  (setq package-native-compile t)
  (setq load-prefer-newer noninteractive)
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; less noise when compiling elisp
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(setq native-comp-async-report-warnings-errors nil)

;; (setq byte-compile-warnings '(not obsolete))
;; (setq warning-suppress-log-types '((comp) (bytecomp)))


;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)
(pixel-scroll-precision-mode 1) ;; enable smooth scrolling

;; memory collection
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold (* 128 1024 1024)))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; Faster to disable these here (before they've been initialized)
;; Minimal UI
;; Define a list of new frame parameters
(setq-default mode-line-format nil)
(setq wt-frame-params
      '(
        (min-height . 1)
        (min-width  . 1)
        (height     . 80)
        (width      . 150)

        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (internal-border-width . 24)

        (left-fringe    . 5)
        (right-fringe   . 5)

        (tool-bar-lines . 0)
        (menu-bar-lines . 0)
      ))

(dolist (param wt-frame-params)
  (push param default-frame-alist))

(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (push '(ns-appearance . dark) default-frame-alist)
  )


(setq inhibit-splash-screen t
      use-file-dialog nil
      ;; use-dialog-box nil

      inhibit-startup-screen t
      inhibit-startup-message ""
      inhibit-startup-echo-area-message nil
      initial-scratch-message nil

      tab-bar-new-button-show nil
      tab-bar-close-button-show nil
      tab-line-close-button-show nil
      )

(provide 'early-init)
;;; early-init.el ends here
