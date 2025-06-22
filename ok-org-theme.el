;;; ok-org-theme.el --- ok-org-theme  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
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
;; The `ok-org-theme` is an Okome Studio theme for `org-mode'. The
;; goal is to make the overall appearance of Org documents modern and
;; visually appealing, rather than enforcing a fixed color scheme.
;; Hence, the theme is meant to be used along side a color-focused
;; theme. Just make sure that you load and enable this after the main
;; theme is loaded and enabled.
;;
;;; Code:

(require 'ok-themes)

(require 'org)
(condition-case nil
    (require 'org-modern)
  (error (error "Must install org-modern to use this theme")))
(condition-case nil
    (require 'org-modern-indent)
  (error (error "Must install org-modern-indent to use this theme")))
(condition-case nil
    (require 'nerd-icons)
  (error (warn "nerd-icons not found; proceeding without it")))
(condition-case nil
    (require 'org-hide-drawers)
  (error (warn "org-hide-drawers not found; proceeding without it")))
(condition-case nil
    (require 'valign)
  (error (warn "valign not found; proceeding without it")))

(deftheme ok-org
  "Okome Studio theme for Org mode."
  :family 'ok
  :background-mode 'light)

(defface ok-org-fixed-pitch '((t :inherit fixed-pitch))
  "Fixed-pitch face.")

(defface ok-org-variable-pitch '((t :inherit variable-pitch))
  "Variable-pitch face.")

(defface ok-org-default '((t :inherit ok-org-variable-pitch))
  "Default face.")

(defface ok-org-outline '((t :inherit ok-org-default))
  "Org outline face.")

(let ((cls t)              ; '((class color) (min-colors 89))
      (fg (face-attribute 'ok-org-default :foreground))
      (fg-em (face-attribute 'secondary-selection :foreground))
      (bg (face-attribute 'ok-org-default :background))
      (bg-em (face-attribute 'secondary-selection :background)))
  (custom-theme-set-faces
   'ok-org
   `(link ((,cls ( :weight unspecified ))))

   `(corfu-default ((,cls ,(when (featurep 'corfu)
                             '( :inherit ok-org-fixed-pitch )))))

   `(org-block ((,cls ( :inherit ok-org-fixed-pitch ))))
   `(org-block-begin-line ((,cls ( :inherit ok-org-fixed-pitch :background ,bg ))))
   `(org-block-end-line ((,cls ( :inherit ok-org-fixed-pitch :background ,bg ))))
   `(org-checkbox ((,cls ( :inherit ok-org-fixed-pitch ))))
   `(org-code ((,cls ( :inherit ok-org-fixed-pitch ))))
   `(org-document-info ((,cls ( :inherit ok-org-fixed-pitch ))))
   `(org-document-info-keyword
     ((,cls ( :inherit ok-org-fixed-pitch
              :foreground ,(face-attribute 'org-drawer :foreground)
              :background ,(face-attribute 'org-drawer :background) ))))
   `(org-document-title ((,cls ( :inherit ok-org-outline :height 1.3 :weight bold ))))
   `(org-drawer ((,cls ( :inherit ok-org-fixed-pitch
                         :foreground ,(face-attribute 'shadow :foreground) ))))
   `(org-formula ((,cls ( :inherit ok-org-fixed-pitch ))))
   `(org-hide ((,cls ( :foreground ,bg :background ,bg ))))
   `(org-indent ((,cls ( :inherit org-hide ))))
   `(org-latex-and-related ((,cls ( :inherit ok-org-fixed-pitch ))))
   `(org-level-1 ((,cls ( :inherit ok-org-outline :height 1.3 :weight bold ))))
   `(org-level-2 ((,cls ( :inherit ok-org-outline :height 1.15 ))))
   `(org-level-3 ((,cls ( :inherit ok-org-outline :height 1.0 ))))
   `(org-level-4 ((,cls ( :inherit ok-org-outline :height 1.0 ))))
   `(org-level-5 ((,cls ( :inherit ok-org-outline :height 1.0 ))))
   `(org-level-6 ((,cls ( :inherit ok-org-outline :height 1.0 ))))
   `(org-level-7 ((,cls ( :inherit ok-org-outline :height 1.0 ))))
   `(org-level-8 ((,cls ( :inherit ok-org-outline :height 1.0 ))))
   `(org-meta-line
     ((,cls ( :inherit ok-org-fixed-pitch
              :foreground ,(face-attribute 'org-drawer :foreground)
              :background ,(face-attribute 'org-drawer :background) ))))
   `(org-property-value ((,cls ( :inherit ok-org-fixed-pitch ))))
   `(org-special-keyword ((,cls ( :inherit ok-org-fixed-pitch ))))
   `(org-table ((,cls ( :inherit ok-org-fixed-pitch ))))
   `(org-table-header ((,cls ( :inherit ok-org-fixed-pitch ))))
   `(org-table-row ((,cls ( :inherit ok-org-fixed-pitch ))))
   `(org-verbatim ((,cls ( :inherit ok-org-fixed-pitch ))))

   `(org-modern-bracket-line ((,cls ,(when (featurep 'org-modern)
                                       '( :inherit ok-org-fixed-pitch )))))
   `(org-modern-tag ((,cls ,(when (featurep 'org-modern)
                              `( :inherit (org-modern-label ok-org-fixed-pitch)
                                 :foreground ,fg-em
                                 :background ,bg-em )))))
   `(org-modern-done ((,cls ,(when (featurep 'org-modern)
                               `( :inherit (org-modern-label ok-org-fixed-pitch)
                                  :inverse-video t )))))
   `(org-modern-todo ((,cls ,(when (featurep 'org-modern)
                               `( :inherit (org-todo org-modern-label ok-org-fixed-pitch)
                                  :inverse-video t )))))

   `(org-modern-indent-bracket-line
     ((,cls ,(when (featurep 'org-modern-indent)
               '( :inherit org-block-begin-line ))))))

  (custom-theme-set-variables
   'ok-org
   ;; org-modern
   '(org-modern-block-name t)   ; use `org-modern-indent'
   '(org-modern-checkbox '((?X . #("▢𐄂" 0 2 (composition ((2)))))
                           (?- . #("▢–" 0 2 (composition ((2)))))
                           (?\s . #("▢" 0 1 (composition ((1)))))))
   '(org-modern-hide-stars nil)
   `(org-modern-keyword
     ',(append
        (when (featurep 'nerd-icons)
          `(("property" . ,(nerd-icons-mdicon "nf-md-alpha_p_box_outline"))
            ("title" . ,(concat (nerd-icons-mdicon "nf-md-note_edit_outline")
                                " "))
            ("filetags" . ,(nerd-icons-mdicon "nf-md-tag_outline"))))
        '((t . "‣ "))))
   '(org-modern-list '((?+ . "🞊")
                       (?- . "⏺")
                       (?* . "🞉")))
   '(org-modern-priority t)
   '(org-modern-replace-stars "🞛🞕🞛🞕■") ; "◉🞛○▷"
   '(org-modern-star 'replace)
   '(org-modern-statistics t)
   '(org-modern-table nil)
   '(org-modern-tag t)
   '(org-modern-timestamp t)
   '(org-modern-todo t)

   ;; org-hide-drawers
   `(org-hide-drawers-display-strings
     `((top-level-property-drawer
        ,(if (featurep 'nerd-icons)
             (nerd-icons-mdicon "nf-md-file_cog_outline")
           "⚙"))
       (drawer-regexp ,(propertize "[PROP...]" 'face 'shadow)
                      ,(rx (0+ anychar)))
       (property-drawer-regexp
        ,(concat " " (if (featurep 'nerd-icons)
                         (nerd-icons-mdicon "nf-md-file_cog_outline")
                       "⚙"))
        ,(rx (0+ anychar)))))))

(defun ok-org--buffer-face ()
  "Set the default face and activate `buffer-face-mode'."
  (when-let* ((height (ok-face-text-scale-mode-height)))
    (set-face-attribute 'ok-org-default nil :height height))
  (setq-local buffer-face-mode-face 'ok-org-default)
  (buffer-face-mode 1))

(defun ok-org-theme--run-when-visible (fun &rest args)
  "Run FUN with ARGS only if current buffer window is visible.
This is an advice to reduce unnecessary rendering."
  (when (get-buffer-window (current-buffer) t)
    (apply fun args)))

(defvar ok-org-theme--run-when-visible '(org-indent-refresh-maybe
                                         org-modern-indent--refresh-watch))

;; org-modern-indent

(defun ok-org-theme-org-modern-indent--fix ()
  "Fix for github.com/jdtsmith/org-modern-indent/issues/10."
  (if org-indent--text-line-prefixes
      (aset org-indent--text-line-prefixes
            0 (propertize " " 'face 'org-indent))))

;; valign

(defcustom ok-org-valign-max-buffer-size 100000
  "Default max-buffer-size above which `valign-mode' will not activate."
  :group 'ok)

(defun ok-org-valign--maybe-activate ()
  "Activate `valign' only if conditions are met."
  (when (<= (buffer-size) ok-org-valign-max-buffer-size)
    (valign-mode 1)))

;;; Setup & Teardown

(defun ok-org-theme--setup (theme)
  (when (eq theme 'ok-org)
    (when (featurep 'org-hide-drawers)
      (add-hook 'org-mode-hook #'org-hide-drawers-mode))
    (when (featurep 'valign)
      (add-hook 'org-mode-hook #'ok-org-valign--maybe-activate))
    (add-hook 'org-indent-mode-hook #'ok-org-theme-org-modern-indent--fix)
    (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
    (add-hook 'org-mode-hook #'org-modern-indent-mode)
    (add-hook 'org-mode-hook #'org-modern-mode)
    (add-hook 'org-mode-hook #'org-indent-mode)
    (add-hook 'org-mode-hook #'visual-line-mode)
    (add-hook 'org-mode-hook #'ok-org--buffer-face)

    (dolist (fun ok-org-theme--run-when-visible)
      (advice-add fun :around #'ok-org-theme--run-when-visible))))

(defun ok-org-theme--teardown (theme)
  (when (eq theme 'ok-org)
    (dolist (fun (reverse ok-org-theme--run-when-visible))
      (advice-add fun :around #'ok-org-theme--run-when-visible))

    (remove-hook 'org-mode-hook #'ok-org--buffer-face)
    (remove-hook 'org-mode-hook #'visual-line-mode)
    (remove-hook 'org-mode-hook #'org-indent-mode)
    (remove-hook 'org-mode-hook #'org-modern-mode)
    (remove-hook 'org-mode-hook #'org-modern-indent-mode)
    (remove-hook 'org-agenda-finalize-hook #'org-modern-agenda)
    (remove-hook 'org-indent-mode-hook #'ok-org-theme-org-modern-indent--fix)
    (when (featurep 'valign)
      (remove-hook 'org-mode-hook #'ok-org-valign--maybe-activate))
    (when (featurep 'org-hide-drawers)
      (remove-hook 'org-mode-hook #'org-hide-drawers-mode))))

(add-hook 'enable-theme-functions #'ok-org-theme--setup)
(add-hook 'disable-theme-functions #'ok-org-theme--teardown)

(provide-theme 'ok-org)

(provide 'ok-org-theme)
;;; ok-org-theme.el ends here
