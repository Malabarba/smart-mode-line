;;; smart-mode-line-light-theme.el --- Light theme for smart-mode-line

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/smart-mode-line
;; Separator: -

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 

;;; Change Log:
;; 0.1a - 2014/05/14 - Created File.
;;; Code:

(deftheme smart-mode-line-light "Light theme for smart-mode-line.")

(custom-theme-set-faces
 'smart-mode-line-light
 '(mode-line-buffer-id ((t :inherit sml/filename :foreground "unspecified" :background "unspecified")))
 '(mode-line-inactive ((t :foreground "grey20" :background "#fdf6e3" :inverse-video nil)))
 '(mode-line     ((t :foreground "black" :background "grey85" :inverse-video nil)))
 '(sml/global    ((t :foreground "gray20" :inverse-video nil)))
 '(sml/modes     ((t :inherit sml/global :foreground "Black")))
 '(sml/filename  ((t :inherit sml/global :foreground "Blue"      :weight bold)))
 '(sml/prefix    ((t :inherit sml/global :foreground "#5b2507"   :weight bold)))
 '(sml/read-only ((t :inherit sml/not-modified :foreground "DarkGreen" :weight bold))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'smart-mode-line-light)
;;; smart-mode-line-light-theme.el ends here.
