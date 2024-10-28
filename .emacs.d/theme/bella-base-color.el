;;; bella-base-color.el

;; ---------------------------------------------------------------------
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2024
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------

;;; code:

;; https://www.nordtheme.com/#palettes-modularity
;; https://github.com/thongpv87/rose-pine-emacs/blob/master/rose-pine-moon-theme.el

(defun bella-color-set ()
  "Apply dark bella theme base."
  ;; Colors from Nord theme at https://www.nordtheme.com
  ;; Colors from Rose Pine theme at https://rosepinetheme.com/palette/

  (defvar bella-color-black "#0D0907")
  (defvar bella-color-text-light "#ECEFF4")
  (defvar bella-color-base "#232136")

  ;; Rose Pine Moon
  ;; (text       "#e0def4")  ;; foreground
  ;; (base       "#232136")  ;; background
  ;; (high       "#393552")  ;; highlight
  ;; (gold       "#f6c177")  ;; critical
  ;; (iris       "#c4a7e7")  ;; salient
  ;; (surface    "#6e6a86")  ;; strong
  ;; (love       "#eb6f92")  ;; popout
  ;; (subtle     "#2a273f")  ;; subtle
  ;; (faded      "#6e6a86")  ;; faded
  ;; (cursor     "#c4a7e7")) ;; cursor

  ;; (nord0 "#2E3440" nil) )
  ;; (nord1 "#3B4252" "black"))
  ;; (nord2 "#434C5E" "#434C5E"))
  ;; (nord3 "#4C566A" "brightblack"))
  ;; (nord4 "#D8DEE9" "#D8DEE9"))
  ;; (nord5 "#E5E9F0" "white"))
  ;; (nord6 "#ECEFF4" "brightwhite"))
  ;; (nord7 "#8FBCBB" "cyan"))
  ;; (nord8 "#88C0D0" "brightcyan"))
  ;; (nord9 "#81A1C1" "blue"))
  ;; (nord10 "#5E81AC" "brightblue"))
  ;; (nord11 "#BF616A" "red"))
  ;; (nord12 "#D08770" "brightyellow"))
  ;; (nord13 "#EBCB8B" "yellow"))
  ;; (nord14 "#A3BE8C" "green"))
  ;; (nord15 "#B48EAD" "magenta"))
  ;; (nord-annotation "#D08770" "brightyellow"))
  ;; (nord-attribute "#8FBCBB" "cyan"))
  ;; (nord-class "#8FBCBB" "cyan"))
  ;; ;; (nord-comment (if (nord-display-truecolor-or-graphic-p) (nord-theme--brightened-comment-color nord-comment-brightness) "brightblack"))
  ;; (nord-escape "#D08770" "brightyellow"))
  ;; (nord-method "#88C0D0" "brightcyan"))
  ;; (nord-keyword "#81A1C1" "blue"))
  ;; (nord-numeric "#B48EAD" "magenta"))
  ;; (nord-operator "#81A1C1" "blue"))
  ;; (nord-preprocessor "#5E81AC" "brightblue"))
  ;; (nord-punctuation "#D8DEE9" "#D8DEE9"))
  ;; (nord-regexp "#EBCB8B" "yellow"))
  ;; (nord-string "#A3BE8C" "green"))
  ;; (nord-tag "#81A1C1" "blue"))
  ;; (nord-variable "#D8DEE9" "#D8DEE9"))
  )
(bella-color-set)

(provide 'bella-base-color)
;;; bella-base-collor.el ends here
