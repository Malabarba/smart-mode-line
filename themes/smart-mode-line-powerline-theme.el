;;; smart-mode-line-powerline-theme.el --- Powerline theme for smart-mode-line

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

(deftheme smart-mode-line-powerline
  "Powerline theme for smart-mode-line.
Mimics the appearance of powerline.")

(require 'powerline)

(let ((l0 "Black")
      (l1 "grey10")
      (l2 "grey20")
      (l3 "grey30")
      (l4 "grey40")
      (l5 "grey50")
      (l6 "grey60")
      (l7 "grey70")
      (l8 "grey80")
      (l9 "grey90")
      (l10 "grey100"))
  (custom-theme-set-faces
   'smart-mode-line-powerline
   `(mode-line-buffer-id ((t :inherit sml/filename :foreground nil :background nil))) 
   `(mode-line-inactive ((t :foreground "gray60" :background "Black" :slant italic :box (:line-width -2 :color "white"))))
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
   `(sml/filename  ((t :background ,l3 :inherit sml/global :foreground "gold" :weight bold)))
   `(sml/sudo      ((t :background ,l3 :inherit sml/outside-modified)))
   `(sml/git       ((t :background ,l3 :inherit (sml/read-only sml/prefix))))
   `(sml/folder    ((t :background ,l3 :inherit sml/global :weight normal :foreground "Black")))
   
   ;; 8
   `(sml/name-filling        ((t :background ,l8 :inherit sml/prefix :weight normal)))
   `(sml/position-percentage ((t :background ,l8 :inherit sml/prefix :weight normal)))
   `(sml/modes               ((t :background ,l8 :inherit sml/global :foreground "Black")))
   `(sml/process             ((t :background ,l8 :inherit sml/prefix)))
   `(sml/vc                  ((t :background ,l8 :inherit sml/git)))
   `(sml/vc-edited           ((t :background ,l8 :inherit sml/prefix)))

   ;; 3
   ;; minor modes

   ;; 0
   `(sml/discharging         ((t :background ,l0 :inherit sml/global :foreground "Red")))
   `(sml/time                ((t :background ,l0 :inherit sml/modes)))

   `(persp-selected-face ((t :foreground "ForestGreen" :inherit sml/filename)))
   `(helm-candidate-number ((t :foreground nil :background nil :inherit sml/filename))))
  (custom-theme-set-variables
   'smart-mode-line-powerline
   '(sml/mode-width 'right)
   '(sml/post-id-separator                                     
     '(:eval (propertize "x" 'display (powerline-arrow-left 'sml/filename 'sml/position-percentage))))
   '(sml/pre-id-separator  
     '(:eval (propertize " " 'display (powerline-arrow-left 'sml/not-modified 'sml/filename))))
   '(sml/pre-minor-modes-separator
     '(:eval (propertize " " 'display (powerline-arrow-right 'sml/position-percentage 'sml/folder))))
   '(sml/pos-minor-modes-separator
     '(:eval (propertize " " 'display (powerline-arrow-right 'sml/folder nil))))
   '(sml/pre-modes-separator
     (propertize " " 'face 'sml/modes))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'smart-mode-line-powerline)
;;; smart-mode-line-powerline-theme.el ends here.
