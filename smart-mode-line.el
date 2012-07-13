;;; smart-mode-line.el --- A color coded smart mode-line.

;; Copyright (C) 2012 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/smart-mode-line
;; Version: 1.6
;; Keywords: faces frames

;;; Commentary:

;; Smart Mode Line is a mode-line format that aims to be easy to
;; read from small to large monitors by using a prefix feature and
;; smart truncation.  

;; Your mode-line will be color coded, smartly truncated (if you
;; want), easily customizable, and will have a few extra fancy
;; features like file path prefixes and minor mode filtering.  

;;; Instructions:

;; INSTALLATION

;;	Make sure "smart-mode-line.el" is in your load path, then place
;; 	this code in your .emacs file:
;;		(require 'smart-mode-line)
;; 		(sml/setup)

;; DESCRIPTION

;;  Its main features are:

;; 		1) Color coded:
;; 			Highlights the most important information for you
;; 			(buffer name, modified state, line number).  Don't
;;			like the colors? See item 4)!

;; 		2) Fixed width (if you want):
;;			Lets you set a maxium width for the path name and mode
;;			names, and truncated intelligently (truncates the
;;			directory, not the buffer name).

;;		3) Fancy features:
;; 			Prefix feature saves a LOT of space.  e.g. "~/.emacs.d/"
;; 			is translated to ":ED:" in the path (open a file inside
;; 			this folder to see it in action). Long path names you
;; 			are commmonly working on are displayed as short
;; 			abbreviations.  Set your own prefixes to make best use
;; 			of it (by configuring `sml/replacer-regexp-list').  Mousing
;;			over the abbreviated path will show you the full
;;			path.  See below for examples.

;; 			Hidden-modes feature saves even more space.  Select
;; 			which minor modes you don't want to see listed by
;; 			customizing the `sml/hidden-modes' variable.  This will
;; 			filter out the modes you don't care about and unclutter
;; 			the modes list (mousing over the modes list still shows
;; 			the full list).

;; 		4) Very easy to configure:
;;			All fonts are in the `smart-mode-line-faces'
;;			customization group, and all other options are in
;;			`smart-mode-line'.  Just run `sml/customize' and see
;;			what's in there.  If you feel anything is missing send me
;;			an e-mail.

;; 		5) Compatible with `battery-display-mode':
;; 			Just turn the mode on to have the battery level
;; 			displayed. sml uses a very short syntax for the
;; 			battery. Only the battery level is displayed (with no %
;; 			symbol), and green/red font means charging/discharging
;; 			respectively.

;; Variables
;; 
;; 	All variables can be edited by running `sml/customize', and the
;; 	documentations are mostly self explanatory, I list here only the
;; 	most important ones.

;; 	`sml/shorten-directory' and `sml/shorten-modes'
;; 		Setting both of these to t garantees a fixed width mode-line
;; 		(directory name and modes list will be truncated to fit).  To
;; 		actually define the width, see below.

;;	`sml/name-width' and `sml/mode-width'
;;		Customize these according to the width of your Emacs
;;		frame.  I set them to 40 and 30 respectively, and the
;;		mode-line fits perfectly when the frame is split in two even
;;		on my laptop's small 17" monitor.

;; 	`sml/replacer-regexp-list'
;; 		This variable is a list of (REGEXP REPLACEMENT) that is used
;; 		to parse the path.  The replacements are applied
;; 		sequentially.  This allows you to greatly abbreviate the path
;; 		that's shown in the mode-line.  If this abbreviation is of
;; 		the form ":SOMETHING:", it is considered a prefix and get's
;; 		a different color (you can change what's considered a prefix
;; 		by customizing `sml/prefix-regexp').

;;		For example, if you do a lot of work on a folder called
;;		"~/Dropbox/Projects/In-Development/" almost half the
;;		mode-line would be occupied just by the folder name, which
;;		is much less important than the buffer name.  But, you can't
;;		just hide the folder name, since editting a file in
;;		"~/Dropbox/Projects/In-Development/Source" is VERY different
;;		from editting a file in "~/Dropbox/Projects/Source".  By
;;		setting up a prefix for your commonly used folders, you get
;;		all that information without wasting all that space.  In this
;;		example you could set the replacement to ":ProjDev:" or just
;;		":InDev:", so the path shown in the mode-line will be
;;		":ProjDev:Source/" (saves a lot of space without hiding
;;		information).

;;		Here go some more useful examples:
;; 	(add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/Projects/In-Development/" ":ProjDev:"))
;;	(add-to-list 'sml/replacer-regexp-list '("^~/Documents/Work/" ":Work:))
;;	;; Added in the right order, they even work sequentially:
;;	(add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:"))
;;	(add-to-list 'sml/replacer-regexp-list '("^:DB:Documents" ":DDocs:"))

;;; License:

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

;; 1.6 - 20120709 - NEW FEATURE: Customizable faces for the prefix, see `sml/prefix-face-list'.
;; 1.5.4 - 20120628 - Optimized regexp-replacer.
;; 1.5.3 - 20120620 - Remove prefix and folder for non-files. Color the :Git prefix.
;; 1.5.2 - 20120614 - Saner default widths and mode-name fix for Term.

;; 1.5.1 - 20120612 - Fixed battery font for corner cases.

;; 1.5 - 20120611 - Added support for display-battery-mode. See the
;; description for more

;;; Code:

(eval-when-compile (require 'cl))

(defun sml/customize ()
  "Open the customization menu the `smart-mode-line' group."
  (interactive)
  (customize-group 'smart-mode-line t))
(defun sml/customize-faces ()
  "Open the customization menu the `smart-mode-line-faces' group."
  (interactive)
  (customize-group 'smart-mode-line-faces t))

(defgroup smart-mode-line '()
  "Customization group for the `smart-mode-line.el' package.")

(defgroup smart-mode-line-faces '()
  "Font (face) colors for the `smart-mode-line.el' package.

You can fully customize any of the fonts to match the color you
want. You can also set properties like bold with ':weight bold'."
  :group 'smart-mode-line)

(defcustom sml/show-client nil
  "Whether to show an \"@\" for emacsclient frames."
  :type 'boolean
  :group 'smart-mode-line)

(defcustom sml/show-time nil
  "Whether to show the time at the end of the mode-line."
  :type 'boolean
  :group 'smart-mode-line)

(defcustom sml/show-battery t
  "Whether to show the battery percentage at the end of the mode-line."
  :type 'boolean
  :group 'smart-mode-line)

(defcustom sml/line-number-format "%3l"
  "Format used to display line number.

Empty it to hide the number."
  :type 'string
  :group 'smart-mode-line)

(defcustom sml/col-number-format "%2c"
  "Format used to display column number.

Empty it to hide the number."
  :type 'string
  :group 'smart-mode-line)

(defcustom sml/numbers-separator ":"
  "Separator between line and column number."
  :type 'string
  :group 'smart-mode-line)

(defcustom sml/time-format " %H:%M"
  "Format used to display the time in the mode-line.

Only relevant if `sml/show-time' is not nil."
  :type 'string
  :group 'smart-mode-line)

(defcustom sml/battery-format " %p"
  "Format used to display the battery in the mode-line.

Only relevant if using `display-battery-mode'. See that function
for the syntax."
  :type 'string
  :group 'smart-mode-line)

(defun sml/set-shortener-func (sym val)
  "Configure `sml/shortener-func' according to `sml/shorten-directory'."
  (set-default sym val)
  (if val (setq sml/shortener-func 'sml/do-shorten-directory)
    (setq sml/shortener-func 'sml/not-shorten-directory)))

(defvar sml/shortener-func 'sml/do-shorten-directory
  "Function used to shorten the directory name.

Value is a funcallable symbol that takes two arguments: the
string to be shortened and the maximum size. This is set
automatically when `sml/shorten-directory' is changed via the
customization menu or via the `sml/toggle-shorten-directory'
function (which are the only ways you should change it).")

(defun sml/toggle-shorten-directory (&rest val)
  "Toggle the variable `sml/shorten-directory'.

If given an argument the variable is set to the argument,
otherwise it is toggled. This can be used as an alternative to
customizing the variable with `customize-group'. Setting the
variable with `setq' will NOT work and should be avoided."
  (interactive)
  (sml/set-shortener-func 'sml/shorten-directory
                          (if val (car val)
                            (not sml/shorten-directory))))

(defcustom sml/shorten-directory t
  "Should directory name be shortened to fit width?

When the buffer+directory name is longer than
`sml/name-width':
	if nil the rest of the mode-line is pushed right;
	otherwise the directory name is shortened to fit."
  :type 'boolean
  :group 'smart-mode-line
  :set 'sml/set-shortener-func)

(defun sml/toggle-shorten-modes (&rest val)
  "Toggle the variable `sml/shorten-modes'.

If given an argument the variable is set to the argument,
otherwise it is toggled. This can be used as an alternative to
customizing the variable with `customize-group'. Equivalent to
setting the variable with `setq'."
  (interactive)
  (setq sml/shorten-modes (if val (car val)
                            (not sml/shorten-modes))))

(defcustom sml/shorten-modes t
  "Should modes list be shortened to fit width?

When the modes list is longer than `sml/mode-width':
	if nil the rest of the mode-line is pushed right;
	otherwise the list is shortened to fit."
  :type 'boolean
  :group 'smart-mode-line)

(defcustom sml/hidden-modes '(" hl-p")
  "List of minor modes you want to hide, or empty.

If empty (or nil), all minor modes are shown in the
mode-line. Otherwise this is a list of REGEXP's that will be
replaced by \"\" in the minor-modes list."
  :type '(repeat string)
  :group 'smart-mode-line)


(defcustom sml/prefix-regexp '(":\\(.*:\\)" "~/")
  "List of Regexps used to identify prefixes.

A prefix is anything at the begining of a line that matches any
of these regexps. Don't start these regexps with \"^\", the
parser applies that for you."
  :type '(repeat regexp)
  :group 'smart-mode-line)

(defcustom sml/replacer-regexp-list '(("^~/\\.emacs\\.d/" ":ED:") ("^/sudo:.*:" ":SU:"))
  "List of pairs of strings used by `sml/replacer'.

The first string of each pair is a regular expression, the second
is a replacement. These replacements are sequentially called on
the filename to replace portions of it. To be considered a prefix
a string must start and end with \":\" (see the default as an
example).

You can also set custom colors (faces) for these prefixes, just
set `sml/prefix-face-list' accordingly."
  :type '(repeat (list regexp string))
  :group 'smart-mode-line)

(defcustom sml/prefix-face-list '((":SU:" sml/sudo)
                                  (":G" sml/git)
                                  ("" sml/prefix))
  "List of (STRING FACE) pairs used by `sml/propertize-prefix'."
  :type '(repeat (list string face))
  :group 'smart-mode-line)

(defcustom sml/name-width 44
  "Minimum and maximum size of the file name in the mode-line.

If `sml/shorten-directory' is nil, this is the minimum width.
Otherwise, this is both the minimum and maximum width."
  :type 'integer
  :group 'smart-mode-line)

(defcustom sml/mode-width 24
  "Maximum and minimum size of the modes list in the mode-line.

If `sml/shorten-modes' is nil, this is the minimum width.
Otherwise, this is both the minimum and maximum width."
  :type 'integer
  :group 'smart-mode-line)

(defcustom sml/modified-time-string "Modified on %T %Y-%m-%d."
  "String format used for displaying the modified time.

This is shown in the tooltip when hovering over the \"modified
file\" character (which is usually a * right before the file
name."
  :type 'string
  :group 'smart-mode-line)

;;;###autoload
(defun sml/setup (&optional arg)
  "Setup the mode-line, or revert it.

If argument is a non-positive integer, revert any changes made.
Otherwise, setup the mode-line."
  (interactive)
  (if (and (integerp arg) (< arg 1))
      (sml/revert)
    (sml/set-face-color nil nil)
    (setq battery-mode-line-format " %p")
    (setq-default
     mode-line-format
     '(
       (:propertize "%e" face sml/warning)
       ;; emacsclient
       (:eval (if sml/show-client (if (frame-parameter nil 'client)
                                      (propertize "@"
                                                  'face 'sml/client
                                                  'help-echo "emacsclient frame")
                                    " ")))
       
       ;; Position
       (:eval (propertize sml/col-number-format
                          'face 'sml/col-number
                          'help-echo (format-mode-line "Buffer size:\n\t%IB")))
       (:eval (propertize sml/numbers-separator
                          'face 'sml/numbers-separator
                          'help-echo (format-mode-line "Buffer size:\n\t%IB")))
       (:eval (propertize sml/line-number-format
                          'face 'sml/line-number
                          'help-echo (format-mode-line "Buffer size:\n\t%IB")))

       ;; Modified status
       (:eval
        (cond ((not (verify-visited-file-modtime))
               (propertize "M"
                           'face 'sml/outside-modified
                           'help-echo "Modified outside Emacs!\nRevert first!"))

              (buffer-read-only
               (propertize "R"
                           'face 'sml/read-only
                           'help-echo "Read-Only Buffer"))
              
              ((buffer-modified-p)
               (propertize "×"
                           'face 'sml/modified
                           'help-echo (if (buffer-file-name)
                                          (format-time-string
                                           sml/modified-time-string
                                           (nth 5 (file-attributes (buffer-file-name))))
                                        "Buffer Modified")))
              
              (t
               (propertize " "
                           'face 'sml/not-modified
                           'help-echo "Buffer Not Modified"))))

       ;; Full path to buffer/file name
       (:eval
        (let* ((prefix (sml/get-prefix (sml/replacer (abbreviate-file-name (sml/get-directory)))))
               (bufname (buffer-name))
               ;; (if (and (buffer-file-name) (file-directory-p (buffer-file-name)))
               ;; 			   "" (buffer-name))
               (dirsize (max 4 (- (abs sml/name-width) (length prefix) (length bufname))))
               (dirstring (funcall sml/shortener-func (sml/get-directory) dirsize)))

          (propertize (concat (sml/propertize-prefix prefix)
                              (propertize dirstring 'face 'sml/folder)
                              (propertize bufname 'face 'sml/filename)
                              (make-string (max 0 (- dirsize (length dirstring))) ?\ ))
                      'help-echo (buffer-file-name))))
       
       ;; The modes list 
       (:eval (propertize mode-name
                          'mouse-face 'mode-line-highlight
                          'face       'sml/modes
                          'local-map  mode-line-major-mode-keymap))
       ;; The mode line process, doesn't get counted into the width
       ;; limit. The only mode I know that uses this is Term.
       (:propertize ("" mode-line-process)
                    'mouse-face 'mode-line-highlight
                    'face       'sml/modes)
       (:eval (sml/extract-minor-modes minor-mode-alist))
       ;; (let ((minor (sml/extract-minor-modes minor-mode-alist)))
       ;;    (propertize (sml/trim-modes major (sml/format-minor-list minor))
       ;;                'help-echo (concat "Major: " mode-name		"\n"
       ;;                                   "Minor:" minor)))
       

       (:propertize battery-mode-line-string
                    face sml/battery)
       
       ;; add the time, with the date and the emacs uptime in the tooltip
       (:eval (if sml/show-time
                  (propertize (format-time-string sml/time-format)
                              'face 'sml/time
                              'help-echo (concat (format-time-string "%c;")
                                                 (emacs-uptime "\nUptime: %hh")))))))))

(defun sml/extract-minor-modes (ml)
  "Extracts all rich strings necessary for the minor mode list."
  (let ((nameList nil))
    (dolist (cur ml nameList)
      (if (eval (car cur)) 
          (add-to-list 'nameList (eval (nth 1 cur)))))
    (let ((out nil)
          (helpString (concat "Full list:\n  "
                              (mapconcat 'identity nameList "\n  "))))
      (dolist (name nameList out)
        (unless (find name sml/hidden-modes :test #'equal)
          (add-to-list 'out (propertize name
                      'help-echo helpString
                      'mouse-face 'mode-line-highlight
                      'face 'sml/folder
                      'local-map mode-line-minor-mode-keymap)))))))

(defun sml/filter-and-propertize-modes (helpstring)
  "Takes a mode name. If should be hidden return nil, otherwise
propertizes the name and returns it."
  (lambda (name)
    (unless (find name sml/hidden-modes :test #'equal)
      (propertize name
                  'help-echo helpstring
                  'mouse-face 'mode-line-highlight
                  'face 'sml/folder
                  'local-map mode-line-minor-mode-keymap)))) 

(defun sml/propertize-prefix (prefix)
  "Set the color of the prefix according to its contents."
  (let ((out prefix))
    (dolist (pair sml/prefix-face-list)
      (if (search (car pair) prefix)
	(return (propertize prefix 'face (car (cdr pair))))))))

(defun sml/trim-modes (major minor)
  "Maybe trim the modes list."
  (let ((out (concat major minor))
        (N sml/mode-width))
    (if sml/shorten-modes
        (if (> (length out) N)
            (concat (substring out 0 (- N 3)) "...")
          (concat out (make-string (- N (length out)) ?\ )))
      (concat out (make-string (max 0 (- N (length out))) ?\ )))))

(defun sml/revert ()
  "Called by `sml/setup' with arg = -1."
  (copy-face 'sml/active-backup 'mode-line)
  (copy-face 'sml/inactive-backup 'mode-line-inactive)
  (setq-default mode-line-format sml/format-backup)
  (setq battery-mode-line-format sml/battery-format-backup)
  )

(defun sml/get-directory ()
  "Decide if we want directory shown. If so, return it."
  (cond ((buffer-file-name) default-directory)
        ((search "Dired" mode-name :start1)
         (replace-regexp-in-string "/[^/]*/$" "/" default-directory))
        (t "")))

(defun sml/set-battery-font ()
  "Set `sml/battery' face depending on battery state."
  (interactive)
  (let ((data (and battery-status-function (funcall battery-status-function))))
    (if  (string-equal "AC" (cdr (assoc 76 data)))
        (copy-face 'sml/charging 'sml/battery)
      (copy-face 'sml/discharging 'sml/battery))))

(defadvice battery-update (before sml/set-battery-font activate)
  (sml/set-battery-font))

(defun sml/format-minor-list (mml)
  "Cleans and fontifies the minor mode list."
  (let ((case-fold-search nil))
    (if sml/hidden-modes
        (replace-regexp-in-string (concat " \\(" (mapconcat 'identity sml/hidden-modes "\\|") "\\)")
                                  ""
                                  mml)
      mml)))

(defun sml/replacer (in)
  "Runs the replacements specified in `sml/replacer-regexp-list'.

Used by `sml/strip-prefix' and `sml/get-prefix'."
  (let ((out in))
    (dolist (cur sml/replacer-regexp-list)
      (setq out (replace-regexp-in-string (car cur)
                                          (car (cdr cur))
                                          out)))
    out))

(defun sml/regexp-composer (getter)
  "Prepares the actual regexp using `sml/prefix-regexp'."
  (let ((left "^\\(")
        (right (if getter "\\|\\).*" "\\)")))
    (if (stringp sml/prefix-regexp) 
        (if (search "\\(" sml/prefix-regexp) 
            sml/prefix-regexp
          (concat left sml/prefix-regexp right)) 
      (concat left (mapconcat 'identity sml/prefix-regexp "\\|") right))))

(defun sml/strip-prefix (path)
  "Remove prefix from string.

A prefix is anything at the begining of the line that matches a
regexp in `sml/prefix-regexp'."
  (replace-regexp-in-string (sml/regexp-composer nil) "" path))

(defun sml/get-prefix (path)
  "Get prefix from string.

A prefix is anything at the begining of the line that matches a
regexp in `sml/prefix-regexp'."
  (replace-regexp-in-string (sml/regexp-composer t) "\\1" path))

(defun sml/not-shorten-directory (dir ml)
  "Dummy function. Just returns abbreviated dir."
  (sml/strip-prefix (sml/replacer (abbreviate-file-name dir))))

(defun sml/do-shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((longname (sml/strip-prefix (sml/replacer (abbreviate-file-name dir)))))
    ;; If it fits, return the string.
    (if (<= (length longname) max-length) longname
      ;; If it doesn't, shorten it
      (let ((path (reverse (split-string longname "/")))
            (output ""))
        (when (and path (equal "" (car path)))
          (setq path (cdr path)))
        ;; Concat as many levels as possible, leaving 4 chars for safety.
        (while (and path (< (length (concat (car path) "/" output)) (- max-length 3)))
          (setq output (concat (car path) "/" output))
          (setq path (cdr path)))
        ;; If we had to shorten, prepend .../
        (when path
          (setq output (concat ".../" output)))
        output
        ))))

;; Color definitions

(defun sml/set-face-color (sym val)
  (if sym (set-default sym val))
  (set-face-attribute 'mode-line nil
                      :foreground sml/active-foreground-color
                      :background sml/active-background-color)
  (set-face-attribute 'mode-line-inactive nil
                      :background sml/inactive-background-color
                      :foreground sml/inactive-foreground-color))

(defcustom sml/active-foreground-color "gray60"
  "Foreground mode-line color for the active frame."
  :type 'color
  :group 'smart-mode-line-faces
  :set 'sml/set-face-color
  :initialize 'set-default)
(defcustom sml/active-background-color "black" "Background mode-line color for the active frame."
  :type 'color :group 'smart-mode-line-faces
  :set 'sml/set-face-color
  :initialize 'set-default)


(defcustom sml/inactive-foreground-color "gray60" "Foreground mode-line color for the inactive frame."
  :type 'color :group 'smart-mode-line-faces
  :set 'sml/set-face-color
  :initialize 'set-default)
(defcustom sml/inactive-background-color "#404045" "Background mode-line color for the inactive frame."
  :type 'color :group 'smart-mode-line-faces
  :set 'sml/set-face-color
  :initialize 'set-default)

;; Face definitions

(defface sml/global
  '((t
     :foreground "gray40"
     ))
  ""
  :group 'smart-mode-line-faces)

(defface sml/warning
  '((t
     :inherit sml/global
     :foreground "#bf0000"
     :weight bold
     ))
  ""
  :group 'smart-mode-line-faces)

(defface sml/line-number
  '((t
     :inherit sml/global
     :foreground "white"
     :weight bold
     ))
  ""
  :group 'smart-mode-line-faces)

(defface sml/col-number
  '((t
     :inherit sml/global
     ))
  ""
  :group 'smart-mode-line-faces)

(defface sml/numbers-separator
  '((t
     :inherit sml/global
     ))
  ""
  :group 'smart-mode-line-faces)

(defface sml/client
  '((t
     :inherit sml/global ;;sml/active-foreground-color
     ))
  ""
  :group 'smart-mode-line-faces)

(defface sml/not-modified
  '((t
     :inherit sml/global ;;sml/active-foreground-color
     ))
  ""
  :group 'smart-mode-line-faces)


(defface sml/read-only
  '((t
     :inherit sml/global
     :foreground "#4271ae"
     ))
  ""
  :group 'smart-mode-line-faces)



(defface sml/outside-modified
  '((t
     :inherit sml/global
     :foreground "#ffffff"
     :background "#c82829"
     ))
  ""
  :group 'smart-mode-line-faces)




(defface sml/modified
  '((t
     :inherit sml/global
     :foreground "#c82829"
     :weight bold
     ))
  ""
  :group 'smart-mode-line-faces)



(defface sml/prefix
  '((t
     :inherit sml/global
     :foreground "#bf6000"
     ;; :weight bold ;; not sure if it's best bold or not
     ))
  ""
  :group 'smart-mode-line-faces)



(defface sml/sudo
  '((t
     :inherit sml/warning
     ))
  ""
  :group 'smart-mode-line-faces)



(defface sml/git
  '((t
     :foreground "DeepSkyBlue"
     :inherit sml/prefix
     ))
  ""
  :group 'smart-mode-line-faces)


(defface sml/folder
  '((t
     :inherit sml/global
     ;; :foreground "gray40"
     ))
  ""
  :group 'smart-mode-line-faces)



(defface sml/filename
  '((t
     :inherit sml/global
     :foreground "#eab700"
     :weight bold
     ))
  ""
  :group 'smart-mode-line-faces)



(defface sml/modes
  '((t
     :inherit sml/global
     :foreground "gray80"
     ))
  ""
  :group 'smart-mode-line-faces)

(defface sml/charging
  '((t
     :inherit sml/global 
     :foreground "green"
     ))
  ""
  :group 'smart-mode-line-faces)

(defface sml/discharging
  '((t
     :inherit sml/global 
     :foreground "red"
     ))
  ""
  :group 'smart-mode-line-faces)

(defface sml/time
  '((t
     :inherit sml/filename
     ))
  ""
  :group 'smart-mode-line-faces)






;; Backup the original configs, just in case.
(defconst sml/format-backup mode-line-format
  "Backs up the `mode-line-format' before SML was required.")

(defconst sml/battery-format-backup (if (boundp 'battery-mode-line-format) battery-mode-line-format "")
  "Backs up the `battery-mode-line-format' before SML was required.")

(copy-face 'mode-line 'sml/active-backup)
(copy-face 'mode-line-inactive 'sml/inactive-backup)


(provide 'smart-mode-line)



;;; smart-mode-line.el ends here

