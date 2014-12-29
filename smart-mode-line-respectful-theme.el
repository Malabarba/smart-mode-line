;;; smart-mode-line-respectful-theme.el --- Respectful theme for smart-mode-line

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/smart-mode-line
;; Version: 0.1a
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

(deftheme smart-mode-line-respectful
  "Respectful theme for smart-mode-line.
Tries to respect the colors chosen by your global theme.
Results may vary.")

(custom-theme-set-faces
 'smart-mode-line-respectful
 '(mode-line-inactive ((t :inverse-video nil)))
 '(mode-line     ((t :inverse-video nil)))
 '(sml/global    ((t :inherit font-lock-preprocessor-face)))
 '(sml/filename  ((t :inherit mode-line-buffer-id)))
 '(sml/prefix    ((t :inherit (font-lock-variable-name-face sml/global))))
 '(sml/read-only ((t :inherit (font-lock-type-face sml/not-modified))))
 '(sml/modes     ((t :foreground nil :inherit sml/filename :weight normal))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'smart-mode-line-respectful)
;;; smart-mode-line-respectful-theme.el ends here.
