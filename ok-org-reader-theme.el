;;; ok-org-reader-theme.el --- ok-org-reader-theme  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'ok-themes)

(deftheme ok-org-reader
  "OK Org reader theme."
  :family 'ok
  :kind 'color-scheme
  :background-mode 'light)

(custom-theme-set-faces
 'ok-org-reader
 (let ((class '((class color) (min-colors 89))))
   `(org-level-1 ((,class ( :height 1.3 ))))
   `(org-level-2 ((,class ( :height 1.2 ))))
   `(org-level-3 ((,class ( :height 1.1 ))))
   `(org-level-4 ((,class ( :height 1.0 ))))
   `(org-level-5 ((,class ( :height 1.0 ))))
   `(org-level-6 ((,class ( :height 1.0 ))))
   `(org-level-7 ((,class ( :height 1.0 ))))
   `(org-level-8 ((,class ( :height 1.0 ))))))

;; (custom-theme-set-variables)

(provide-theme 'ok-org-reader)

(provide 'ok-org-reader-theme)
;;; ok-org-reader-theme.el ends here
