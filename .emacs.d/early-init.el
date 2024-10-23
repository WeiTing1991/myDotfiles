;;; early-init.el ---   -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

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

;;; Commentary:

;;

;;; Code:
(setq package-enable-at-startup nil)

(defvar my-computer-has-smaller-memory-p nil
  "Installing&Compiling many packages could cost too much memory.")

(setq gc-cons-threshold (* 1024 1024 1024))

(unless my-computer-has-smaller-memory-p
  (setq gc-cons-percentage 0.6)
  (setq gc-cons-threshold most-positive-fixnum))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1)))

(setq idle-update-delay 1.0)

(setq native-comp-async-report-warnings-errors 'silent)

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (setq native-comp-deferred-compilation t)
  (setq package-native-compile t)
  (setq load-prefer-newer noninteractive)
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; minimal UI
(menu-bar-mode -1) ;; disables menubar
(tool-bar-mode -1) ;; disables toolbar
(scroll-bar-mode -1) ;; disables scrollbar
(pixel-scroll-precision-mode 1) ;; enable smooth scrolling

(setq inhibit-splash-screen t ;; no thanks
      use-file-dialog nil ;; don't use system file dialog
      tab-bar-new-button-show nil ;; don't show new tab button
      tab-bar-close-button-show nil ;; don't show tab close button
      tab-line-close-button-show nil) ;; don't show tab close button

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)


(provide 'early-init)
;;; early-init.el ends here
