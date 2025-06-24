;;; ok-org-themes.el --- Okome Studio Org mode themes  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/ok-org-themes.el
;; Version: 0.1.1
;; Keywords: theme, faces
;; Package-Requires: ((emacs "30.1") (org "9.7"))
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
;; Okome Studio's collection of Emacs themes for Org mode.
;;
;;; Code:

(defgroup ok-org-themes nil
  "Okome Studio themes for Org mode."
  :group 'faces
  :prefix "ok-org-themes-"
  :tag "Okome Studio Org Themes")

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'ok-org-themes)
;;; ok-org-themes.el ends here
