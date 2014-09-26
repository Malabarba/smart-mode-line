;;; smart-mode-line-powerline-theme.el --- smart-mode-line theme that mimics the powerline appearance.

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/smart-mode-line
;; Version: 0.1a
;; Package-Requires: ((emacs "24.3") (powerline "2.3") (smart-mode-line "2.5"))
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
;; 0.1a - 2014/05/15 - powerline-theme essentially finished.
;; 0.1a - 2014/05/14 - Created File.
;;; Code:

(deftheme smart-mode-line-powerline
  "Powerline theme for smart-mode-line.
Mimics the appearance of powerline.")

(require 'powerline)

(set-face-attribute 'powerline-active2 nil :inherit 'sml/global)
(set-face-attribute 'powerline-active1 nil :inherit 'sml/global)
(let ((l0 "black")
      (l3 (or (face-background 'powerline-active1) "Grey30"))
      (l8 (or (face-background 'powerline-active2) "Grey80"))
      (separator-left 
       '(intern (format "powerline-%s-%s"
                        powerline-default-separator
                        (car powerline-default-separator-dir))))
      (separator-right 
       '(intern (format "powerline-%s-%s"
                        powerline-default-separator
                        (cdr powerline-default-separator-dir)))))
  (custom-theme-set-faces
   'smart-mode-line-powerline
   `(mode-line-buffer-id ((t :inherit sml/filename :foreground nil :background nil))) 
   `(mode-line-inactive ((((background dark)) :foreground "gray60" :background "Black"
                          :slant italic :box (:line-width -3 :color "black"))
                         (((background light)) :foreground "gray60" :background "Black"
                          :slant italic :box (:line-width -2 :color "white"))))
   `(mode-line     ((t :foreground "gray60" :background "black" :box (:line-width -1 :color "Black"))))
   `(sml/global    ((t :foreground "gray50" :inverse-video nil)))
   
   ;; Layer 0
   `(sml/line-number         ((t :foreground "White" :inherit sml/global :weight bold :background ,l0)))
   `(sml/remote              ((t :inherit sml/global :background ,l0)))
   `(sml/col-number          ((t :inherit sml/global :background ,l0)))
   `(sml/numbers-separator   ((t :inherit sml/col-number :background ,l0)))
   `(sml/client              ((t :inherit sml/prefix :background ,l0)))
   `(sml/mule-info           ((t :inherit sml/global :background ,l0)))
   `(sml/not-modified        ((t :inherit sml/global :background ,l0)))
   '(sml/read-only           ((t :inherit sml/not-modified :foreground "Cyan")))

   ;; 3
   `(sml/prefix    ((t :background ,l3 :inherit sml/global :foreground "#bf6000")))
   `(sml/filename  ((t :background ,l3 :inherit sml/global :foreground "gold")))
   `(sml/sudo      ((t :background ,l3 :inherit sml/outside-modified)))
   `(sml/git       ((t :background ,l3 :inherit (sml/read-only sml/prefix))))
   `(sml/folder    ((t :background ,l3 :inherit sml/global :weight normal :foreground "Black")))
   
   ;; 8
   `(sml/name-filling        ((t :background ,l8 :inherit sml/prefix :weight normal)))
   `(sml/position-percentage ((t :background ,l8 :inherit sml/prefix :weight normal :foreground "#330000")))
   `(sml/modes               ((t :background ,l8 :inherit sml/global :foreground "Black")))
   `(sml/process             ((t :background ,l8 :inherit sml/prefix)))
   `(sml/vc                  ((t :background ,l8 :inherit sml/git :foreground "#0000aa")))
   `(sml/vc-edited           ((t :background ,l8 :inherit sml/prefix :foreground "#330000")))

   ;; 3
   ;; minor modes

   ;; 0
   `(sml/discharging         ((t :background ,l0 :inherit sml/global :foreground "Red")))
   `(sml/time                ((t :background ,l0 :inherit sml/modes)))

   `(persp-selected-face ((t :foreground "ForestGreen" :inherit sml/filename)))
   `(helm-candidate-number ((t :foreground nil :background nil :inherit sml/filename))))
  (custom-theme-set-variables
   'smart-mode-line-powerline
   '(sml/mode-width (if (eq powerline-default-separator 'arrow) 'right 'full))
   `(sml/pre-id-separator
     '(""
       (:propertize " " face sml/global)
       (:eval (propertize " " 'display (funcall ,separator-left nil 'powerline-active1)))
       (:propertize " " face powerline-active1)))
   `(sml/pos-id-separator
     '(""
       (:propertize " " face powerline-active1)
       (:eval (propertize " " 'display (funcall ,separator-left 'powerline-active1 'powerline-active2)))
       (:propertize " " face powerline-active2)))
   `(sml/pre-minor-modes-separator
     '("" (:propertize " " face powerline-active2)
       (:eval (propertize " " 'display (funcall ,separator-right 'powerline-active2 'powerline-active1)))
       (:propertize " " face powerline-active1)))
   `(sml/pos-minor-modes-separator
     '("" (:propertize " " face powerline-active1)
       (:eval (propertize " " 'display (funcall ,separator-right 'powerline-active1 nil)))
       (:propertize " " face sml/global)))
   '(sml/pre-modes-separator
     (propertize " " 'face 'sml/modes))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'smart-mode-line-powerline)
;;; smart-mode-line-powerline-theme.el ends here.
