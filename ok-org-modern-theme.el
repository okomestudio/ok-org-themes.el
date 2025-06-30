;;; ok-org-modern-theme.el --- A modern variant of Okome Studio Org mode theme  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/ok-org-modern-theme.el
;; Version: 0.1.2
;; Keywords: theme, faces
;; Package-Requires: ((emacs "30.1") (org "9.7") (org-modern "1.9") (org-modern-indent "0.5.1"))
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
;; The `ok-org-modern-theme` is a modern variant of Org mode theme.
;; The goal is to make Org documents visually more appealing through
;; `org-modern'. The theme does not enforce a fixed color scheme; it
;; should be used alongside a main, color-focused theme. Enable this
;; theme after the main theme is enabled.
;;
;;; Code:

(eval-and-compile
  (require 'ok-org-themes)

  (dolist (pkg '(org org-modern org-modern-indent))
    (condition-case nil
        (require pkg)
      (error (error "The package `%s' is required" pkg))))

  (dolist (pkg '(nerd-icons org-hide-drawers valign))
    (condition-case nil
        (require pkg)
      (error (message "The package `%s' is not found; proceeding" pkg))))

;;;###theme-autoload
  (deftheme ok-org-modern
    "Okome Studio theme for Org mode."
    :background-mode 'light
    :family 'ok-org)

  (defface ok-org-modern-fixed-pitch '((t :inherit fixed-pitch))
    "Fixed-pitch face.")

  (defface ok-org-modern-variable-pitch '((t :inherit variable-pitch))
    "Variable-pitch face.")

  (defface ok-org-modern-default '((t :inherit ok-org-modern-variable-pitch))
    "Default face.")

  (defface ok-org-modern-outline '((t :inherit ok-org-modern-default))
    "Org outline face.")

  (let ((cls t)               ; '((class color) (min-colors 89))
        (fg (face-attribute 'ok-org-modern-default :foreground))
        (bg (face-attribute 'ok-org-modern-default :background))
        (fg-em (face-attribute 'secondary-selection :foreground))
        (bg-em (face-attribute 'secondary-selection :background))
        (fg-de (face-attribute 'shadow :foreground))
        (bg-de (face-attribute 'shadow :background)))
    (custom-theme-set-faces
     'ok-org-modern
     `(font-lock-comment-face ((,cls ( :inherit ok-org-modern-fixed-pitch ))))
     ;; TODO(2025-06-30): Configure the rest of font-lock-* faces.

     `(link ((,cls ( :weight unspecified ))))

     `(corfu-default ((,cls ,(when (featurep 'corfu)
                               '( :inherit ok-org-modern-fixed-pitch )))))

     `(org-block ((,cls ( :inherit ok-org-modern-fixed-pitch ))))
     `(org-block-begin-line ((,cls ( :inherit ok-org-modern-fixed-pitch
                                     :background ,bg ))))
     `(org-block-end-line ((,cls ( :inherit ok-org-modern-fixed-pitch
                                   :background ,bg ))))
     `(org-checkbox ((,cls ( :inherit ok-org-modern-fixed-pitch ))))
     `(org-code ((,cls ( :inherit ok-org-modern-fixed-pitch ))))
     `(org-document-info ((,cls ( :inherit ok-org-modern-fixed-pitch
                                  :foreground ,fg-de
                                  :background: ,bg-de ))))
     `(org-document-info-keyword ((,cls ( :inherit ok-org-modern-fixed-pitch
                                          :foreground ,fg-de
                                          :background ,bg-de ))))
     `(org-document-title ((,cls ( :inherit ok-org-modern-outline
                                   :height 1.2
                                   :weight bold
                                   :underline unspecified ))))
     `(org-drawer ((,cls ( :inherit ok-org-modern-fixed-pitch
                           :foreground ,fg-de
                           :background ,bg-de))))
     `(org-formula ((,cls ( :inherit ok-org-modern-fixed-pitch ))))
     `(org-hide ((,cls ( :foreground ,bg :background ,bg ))))
     `(org-indent ((,cls ( :inherit (org-hide ok-org-modern-fixed-pitch) ))))
     `(org-latex-and-related ((,cls ( :inherit ok-org-modern-fixed-pitch ))))
     `(org-level-1 ((,cls ( :inherit ok-org-modern-outline
                            :height 1.1
                            :weight bold ))))
     `(org-level-2 ((,cls ( :inherit ok-org-modern-outline :height 1.0 ))))
     `(org-level-3 ((,cls ( :inherit ok-org-modern-outline :height 1.0 ))))
     `(org-level-4 ((,cls ( :inherit ok-org-modern-outline :height 1.0 ))))
     `(org-level-5 ((,cls ( :inherit ok-org-modern-outline :height 1.0 ))))
     `(org-level-6 ((,cls ( :inherit ok-org-modern-outline :height 1.0 ))))
     `(org-level-7 ((,cls ( :inherit ok-org-modern-outline :height 1.0 ))))
     `(org-level-8 ((,cls ( :inherit ok-org-modern-outline :height 1.0 ))))
     `(org-meta-line ((,cls ( :inherit ok-org-modern-fixed-pitch
                              :foreground ,fg-de
                              :background ,bg-de ))))
     `(org-property-value ((,cls ( :inherit ok-org-modern-fixed-pitch ))))
     `(org-special-keyword ((,cls ( :inherit ok-org-modern-fixed-pitch ))))
     `(org-table ((,cls ( :inherit ok-org-modern-fixed-pitch ))))
     `(org-table-header ((,cls ( :inherit ok-org-modern-fixed-pitch ))))
     `(org-table-row ((,cls ( :inherit ok-org-modern-fixed-pitch ))))
     `(org-verbatim ((,cls ( :inherit ok-org-modern-fixed-pitch ))))

     `(org-modern-bracket-line
       ((,cls ,(when (featurep 'org-modern)
                 '( :inherit ok-org-modern-fixed-pitch )))))
     `(org-modern-tag
       ((,cls ,(when (featurep 'org-modern)
                 `( :inherit (org-modern-label ok-org-modern-fixed-pitch)
                    :foreground ,fg-em
                    :background ,bg-em )))))
     `(org-modern-done
       ((,cls ,(when (featurep 'org-modern)
                 `( :inherit (org-modern-label ok-org-modern-fixed-pitch)
                    :inverse-video t )))))
     `(org-modern-todo
       ((,cls ,(when (featurep 'org-modern)
                 `( :inherit (org-todo
                              org-modern-label
                              ok-org-modern-fixed-pitch)
                    :inverse-video t )))))
     `(org-modern-indent-bracket-line
       ((,cls ,(when (featurep 'org-modern-indent)
                 '( :inherit org-block-begin-line ))))))

    (custom-theme-set-variables
     'ok-org-modern
     '(org-indent-indentation-per-level 2)

     ;; org-modern
     '(org-modern-block-name t) ; use `org-modern-indent'
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
     '(org-modern-list '((?+ . "✧")
                         (?- . "‒")
                         (?* . "✦")))
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

  (defun ok-org-modern--buffer-face ()
    "Set the default face and activate `buffer-face-mode'."
    (when-let* ((height (ok-face-text-scale-mode-height)))
      (set-face-attribute 'ok-org-modern-default nil :height height))
    (setq-local buffer-face-mode-face 'ok-org-modern-default)
    (buffer-face-mode 1))

  (defun ok-org-modern--run-when-visible (fun &rest args)
    "Run FUN with ARGS only if current buffer window is visible.
This is an advice to reduce unnecessary rendering."
    (when (get-buffer-window (current-buffer) t)
      (apply fun args)))

  (defvar ok-org-modern--run-when-visible '(org-indent-refresh-maybe
                                            org-modern-indent--refresh-watch))

  ;; org-modern-indent

  (defun ok-org-modern-indent--fix ()
    "Fix for github.com/jdtsmith/org-modern-indent/issues/10."
    (if org-indent--text-line-prefixes
        (aset org-indent--text-line-prefixes
              0 (propertize " " 'face 'org-indent))))

  ;; valign

  (defcustom ok-org-modern-valign-max-buffer-size 100000
    "Default max-buffer-size above which `valign-mode' will not activate."
    :group 'ok)

  (defun ok-org-modern-valign--maybe-activate ()
    "Activate `valign' only if conditions are met."
    (when (<= (buffer-size) ok-org-modern-valign-max-buffer-size)
      (valign-mode 1)))

;;; Setup & Teardown

  (defun ok-org-modern--setup (theme)
    "Set up the THEME."
    (when (eq theme 'ok-org-modern)
      (when (featurep 'org-hide-drawers)
        (add-hook 'org-mode-hook #'org-hide-drawers-mode))
      (when (featurep 'valign)
        (add-hook 'org-mode-hook #'ok-org-modern-valign--maybe-activate))
      (add-hook 'org-indent-mode-hook #'ok-org-modern-indent--fix)
      (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
      (add-hook 'org-mode-hook #'org-modern-indent-mode)
      (add-hook 'org-mode-hook #'org-modern-mode)
      (add-hook 'org-mode-hook #'org-indent-mode)
      (add-hook 'org-mode-hook #'visual-line-mode)
      (add-hook 'org-mode-hook #'ok-org-modern--buffer-face)

      (dolist (fun ok-org-modern--run-when-visible)
        (advice-add fun :around #'ok-org-modern--run-when-visible))))

  (defun ok-org-modern--teardown (theme)
    "Tear down the THEME."
    (when (eq theme 'ok-org-modern)
      (dolist (fun (reverse ok-org-modern--run-when-visible))
        (advice-add fun :around #'ok-org-modern--run-when-visible))

      (remove-hook 'org-mode-hook #'ok-org-modern--buffer-face)
      (remove-hook 'org-mode-hook #'visual-line-mode)
      (remove-hook 'org-mode-hook #'org-indent-mode)
      (remove-hook 'org-mode-hook #'org-modern-mode)
      (remove-hook 'org-mode-hook #'org-modern-indent-mode)
      (remove-hook 'org-agenda-finalize-hook #'org-modern-agenda)
      (remove-hook 'org-indent-mode-hook #'ok-org-modern-indent--fix)
      (when (featurep 'valign)
        (remove-hook 'org-mode-hook #'ok-org-modern-valign--maybe-activate))
      (when (featurep 'org-hide-drawers)
        (remove-hook 'org-mode-hook #'org-hide-drawers-mode))))

  (add-hook 'enable-theme-functions #'ok-org-modern--setup)
  (add-hook 'disable-theme-functions #'ok-org-modern--teardown)

  (provide-theme 'ok-org-modern))

;;; ok-org-modern-theme.el ends here
