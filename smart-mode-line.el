;;; smart-mode-line.el --- A color coded smart mode-line.

;; Copyright (C) 2012 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/smart-mode-line
;; Version: 1.30
;; Keywords: faces frames
;; Prefix: sml
;; Separator: /

;;; Commentary:

;; Smart Mode Line is a mode-line format that aims to be easy to
;; read from small to large monitors by using a prefix feature and
;; smart truncation.

;; Your mode-line will be color coded, smartly truncated (if you
;; want), easily customizable, and will have a few extra fancy
;; features like file path prefixes and minor mode filtering.

;;; Instructions:

;; INSTALLATION
;;
;; Make sure "smart-mode-line.el" is in your load path, then place
;; this code in your .emacs file:
;;     (require 'smart-mode-line)
;;     (if after-init-time (sml/setup)
;;       (add-hook 'after-init-hook 'sml/setup))

;; DESCRIPTION
;;
;; Its main features are:
;;
;; 1) Color coded:
;; Highlights the most important information for you
;; (buffer name, modified state, line number). Don't
;; like the colors? See item 4)!
;;
;; 2) Fixed width (if you want):
;; Lets you set a maxium width for the path name and mode
;; names, and truncated intelligently (truncates the
;; directory, not the buffer name).
;;
;; 3) Fancy features:
;; Prefix feature saves a LOT of space. e.g. "~/.emacs.d/"
;; is translated to ":ED:" in the path (open a file inside
;; this folder to see it in action). Long path names you
;; are commmonly working on are displayed as short
;; abbreviations. Set your own prefixes to make best use
;; of it (by configuring `sml/replacer-regexp-list'). Mousing
;; over the abbreviated path will show you the full
;; path. See below for examples.
;;
;; Hidden-modes feature saves even more space. Select
;; which minor modes you don't want to see listed by
;; customizing the `sml/hidden-modes' variable. This will
;; filter out the modes you don't care about and unclutter
;; the modes list (mousing over the modes list still shows
;; the full list).
;;
;; 4) Very easy to configure:
;; All fonts are in the `smart-mode-line-faces'
;; customization group, and all other options are in
;; `smart-mode-line'. Just run `sml/customize' and see
;; what's in there. If you feel anything is missing send me
;; an e-mail.
;;
;; 5) Compatible with `battery-display-mode':
;; Just turn the mode on to have the battery level
;; displayed. sml uses a very short syntax for the
;; battery. Only the battery level is displayed (with no %
;; symbol), and green/red font means charging/discharging
;; respectively.

;; VARIABLES
;;
;; All variables can be edited by running `sml/customize', and the
;; documentations are mostly self explanatory, I list here only the
;; most important ones.
;;
;; *Note:* We use an `after-init-hook` in the installation because we
;; need sml/setup to override the theme's colors for the mode-line. See
;; the documentattion on the `sml/override-theme` variable for more
;; information.
;;
;; 	`sml/shorten-directory' and `sml/shorten-modes'
;; 		Setting both of these to t garantees a fixed width mode-line
;; 		(directory name and modes list will be truncated to fit).  To
;; 		actually define the width, see below.
;;
;;	`sml/name-width' and `sml/mode-width'
;;		Customize these according to the width of your Emacs
;;		frame.  I set them to 40 and 'full respectively, and the
;;		mode-line fits perfectly when the frame is split in two even
;;		on my laptop's small 17" monitor.
;;
;; 	`sml/replacer-regexp-list'
;; 		This variable is a list of (REGEXP REPLACEMENT) that is used
;; 		to parse the path.  The replacements are applied
;; 		sequentially.  This allows you to greatly abbreviate the path
;; 		that's shown in the mode-line.  If this abbreviation is of
;; 		the form ":SOMETHING:", it is considered a prefix and get's
;; 		a different color (you can change what's considered a prefix
;; 		by customizing `sml/prefix-regexp').
;;
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
;;
;;		Here go some more useful examples:
;;
;; 	(add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/Projects/In-Development/"	":ProjDev:"))
;;	(add-to-list 'sml/replacer-regexp-list '("^~/Documents/Work/"			":Work:"))
;;
;;	;; Added in the right order, they even work sequentially:
;;	(add-to-list 'sml/replacer-regexp-list '("^:DB:Documents"			":DDocs:"))
;;	(add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/"				":DB:"))
;;	(add-to-list 'sml/replacer-regexp-list '("^:Git:\\(.*\\)/src/main/java/"	":G/\\1/SMJ:"))
;;	(add-to-list 'sml/replacer-regexp-list '("^~/Git-Projects/"			":Git:"))

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
;; 1.30 - 20131013 - Click mode list to toggle minor-mode hiding.
;; 1.29.2 - 20131002 - Different default position-percentage face.
;; 1.29.1 - 20130822 - Fix hang introduced with last update.
;; 1.29 - 20130811 - Fixed lag with remote files.
;; 1.28.1 - 20130811 - Fix for the erc fix.
;; 1.28 - 20130811 - Fixing erc notifications.
;; 1.27 - 20130810 - Changed default value of sml/mode-width to a number. 'full didn't work for everyone.
;; 1.27 - 20130810 - Doc bug.
;; 1.26 - 20130718 - Fix for % in the process string.
;; 1.25 - 20130716 - sml/override-theme also tries to set good colors for the text (not just the background).
;; 1.24 - 20130716 - sml/mule-info face changed to be less important.
;; 1.23.2 - 20130715 - Changed doc of sml/replacer-regexp-list.
;; 1.23.1 - 20130715 - moved perspective variable to eval-after-load.
;; 1.23 - 20130715 - added an icon to mew-format.
;; 1.23 - 20130715 - obsolete sml/show-time.
;; 1.23 - 20130715 - fixed a bug which required emacs restart for changes to take effect.
;; 1.22 - 20130715 - sml/vc-mode-show-backend implemented.
;; 1.22 - 20130715 - move mew-support variable.
;; 1.22 - 20130715 - Changed default value of sml/replacer-regexp-list.
;; 1.21 - 20130714 - Encoding description.
;; 1.21 - 20130714 - Reestructured some of the present functions.
;; 1.21 - 20130714 - New position indicator.
;; 1.20 - 20130714 - vc-mode support.
;; 1.19 - 20130714 - Reorganized groups.
;; 1.18 - 20130712 - mew variables only get created if mew is loaded.
;; 1.18 - 20130712 - Reformulated the simplified mode-line.
;; 1.18 - 20130712 - Added number of lines to mouse tooltip of position.
;; 1.17 - 20130710 - Fallback 'modified' string.
;; 1.16 - 20130708 - Changed implementation of battery display.
;; 1.16 - 20130708 - Fixed battery-display.
;; 1.15 - 20130706 - Implemented sml-modeline support.
;; 1.14 - 20130625 - Slightly reduced the default value of extra-filler.
;; 1.13 - 20130610 - removed 'cl requirement.
;; 1.13 - 20130610 - Advice to mew-biff-clear.
;; 1.12 - 20130606 - Gigantic typo fix. Sorry about that.
;; 1.11 - 20130605 - Added biff support.
;; 1.10 - 20130524 - Fix for buffer name with '%'.
;; 1.9 - 20130513 - Now uses file name instead of buffer-name by default, controled by `sml/show-file-name'.
;; 1.9 - 20130513 - When showing buffer name, can strip the <N> part by setting `sml/show-trailing-N'.
;; 1.8.3 - 20130421 - Fixed first line of docs.
;; 1.8.2 - 20130418 - added empty anchors throughout the mode-line.
;; 1.8.2 - 20130418 - evil-mode support.
;; 1.8.1 - 20130417 - sml/bug-report function.
;; 1.8.1 - 20130417 - sml/override-theme variable.
;; 1.8.1 - 20130417 - Changed install instruction to override theme settings.
;; 1.8 - 20130414 - sml/mode-width can now be 'full.
;; 1.7.1 - 20121117 - Perspective support.
;; 1.7 - 20121114 - Fixed some modes not showing in the minor mode list - Thanks Constantin.
;; 1.7 - 20121114 - Fixed infinite loop.  - Thanks Constantin.
;; 1.7 - 20121114 - Fixed for dired-mode.
;; 1.7 - 20121114 - Added parent customize groups.
;; 1.6.2 - 20120713 - Fixed mode shortenning.
;; 1.6.1 - 20120712 - NEW FEATURE: Modes list now fully supports clicking.
;; 1.6.1 - 20120712 - NEW FEATURE: `sml/version' constant.
;; 1.6.1 - 20120712 - `sml/hidden-modes' is now a list of strings (not regexps).
;; 1.6 - 20120709 - NEW FEATURE: Customizable faces for the prefix, see `sml/prefix-face-list'.
;; 1.5.4 - 20120628 - Optimized regexp-replacer.
;; 1.5.3 - 20120620 - Remove prefix and folder for non-files. Color the :Git prefix.
;; 1.5.2 - 20120614 - Saner default widths and mode-name fix for Term.
;; 1.5.1 - 20120612 - Fixed battery font for corner cases.
;; 1.5 - 20120611 - Added support for display-battery-mode. See the description for more.

;;; Code:

;; (eval-when-compile (require 'cl))

(defconst sml/version "1.30" "Version of the smart-mode-line.el package.")

(defconst sml/version-int 34 "Version of the smart-mode-line.el package, as an integer.")

(defun sml/bug-report ()
  "Opens github issues page in a web browser. Please send me any bugs you find, and please inclue your emacs and sml versions."
  (interactive)
  (browse-url "https://github.com/Bruce-Connor/smart-mode-line/issues/new")
  (message "Your sml/version is: %s, and your emacs version is: %s.\nPlease include this in your report!" sml/version emacs-version))

(defun sml/customize ()
  "Open the customization menu the `smart-mode-line' group."
  (interactive)
  (customize-group 'smart-mode-line t))

(defun sml/customize-faces ()
  "Open the customization menu the `smart-mode-line-faces' group."
  (interactive)
  (customize-group 'smart-mode-line-faces t))

(defgroup smart-mode-line '()
  "Customization group for the `smart-mode-line.el' package."
  :group 'convenience)
;; (defgroup smart-mode-line-mode-line '()
;;   "Group for editing the mode-line created by `sml/setup'."
;;   :group 'smart-mode-line)
(defgroup smart-mode-line-position '()
  "Group for editing the major/minor mode list."
  :group 'smart-mode-line)
(defgroup smart-mode-line-path&prefix '()
  "Group for editing the path, buffer-name, and prefix."
  :group 'smart-mode-line)
(defgroup smart-mode-line-mode-list '()
  "Group for editing the major/minor mode list."
  :group 'smart-mode-line)
(defgroup smart-mode-line-others '()
  "Group for editing the major/minor mode list."
  :group 'smart-mode-line)

(defgroup smart-mode-line-faces '()
  "Font (face) colors for the `smart-mode-line.el' package.

You can fully customize any of the fonts to match the color you
want. You can also set properties like bold with ':weight bold'.

Note that, by default, smart-mode-line overrides your theme's
settings for the background and foreground color of the modeline
face. We need to override, otherwise some elements become
unreadable on lighter themes. If you'd rather configure these
unreadable colors yourself and keep your theme's settings, just
set `sml/override-theme' to nil."
  :group 'smart-mode-line
  :group 'faces)


(defvar sml/shortener-func 'sml/do-shorten-directory
  "Function used to shorten the directory name.

Value is a funcallable symbol that takes two arguments: the
string to be shortened and the maximum size. This is set
automatically when `sml/shorten-directory' is changed via the
customization menu or via the `sml/toggle-shorten-directory'
function (which are the only ways you should change it).")

(defun sml/set-shortener-func (sym val)
  "Configure `sml/shortener-func' according to `sml/shorten-directory'."
  (set-default sym val)
  (if val (setq sml/shortener-func 'sml/do-shorten-directory)
    (setq sml/shortener-func 'sml/not-shorten-directory)))

(defcustom sml/override-theme t
  "For any value other than nil, sml will override your theme's foreground and background colors for the modeline. 

If this is nil many modeline elements will use standard font-lock
faces, in an attempt to still look nice. The result will vary
depending on your theme, but that's what you're asking for if you
set this to nil. ;-)

You HAVE to set this BEFORE loading `smart-mode-line', otherwise
it has no effect.

Changing this only has effect after restarting emacs."
  :type 'boolean
  :group 'smart-mode-line-faces
  :group 'smart-mode-line)

(defcustom sml/position-percentage-format " %p"
  "Format used to display position in the buffer.

Empty it to hide the number."
  :type 'string
  :group 'smart-mode-line-position)

(defcustom sml/line-number-format "%3l"
  "Format used to display line number.

Empty it or disable `line-number-mode' to hide the number."
  :type 'string
  :group 'smart-mode-line-position)

(defcustom sml/col-number-format "%2c"
  "Format used to display column number.

Empty it or disable `column-number-mode' to hide the number."
  :type 'string
  :group 'smart-mode-line-position)

(defcustom sml/numbers-separator ":"
  "Separator between line and column number.

Since we use different faces for line and column number, you can
just set this to \"\" to save an extra charof space."
  :type 'string
  :group 'smart-mode-line-position)

(defcustom sml/show-client nil
  "Whether to show an \"@\" for emacsclient frames."
  :type 'boolean
  :group 'smart-mode-line-others)

(defcustom sml/modified-char (char-to-string (if (char-displayable-p ?×) ?× ?*))
  "String that indicates if buffer is modified. Should be one SINGLE char."
  :type 'string
  :group 'smart-mode-line-others
  :package-version '(smart-mode-line . "1.16"))

(defcustom sml/show-trailing-N t
  "Whether the \"<N>\" suffix in buffer names should be displayed in the mode-line."
  :type 'boolean
  :group 'smart-mode-line-path&prefix)

(defcustom sml/show-file-name t
  "Unless nil: show file name instead of buffer name on the mode-line."
  :type 'boolean
  :group 'smart-mode-line-path&prefix)

(defcustom sml/fill-char ?\ 
  "The char to be used for filling."
  :type 'char
  :group 'smart-mode-line-path&prefix)

(defcustom sml/replacer-regexp-list
  '(("^~/\\.emacs\\.d/" ":ED:")
    ("^/sudo:.*:" ":SU:")
    ("^~/Documents/" ":Doc:")
    ("^~/Dropbox/" ":DB:")
    ("^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
    ("^~/[Gg]it/" ":Git:")
    ("^~/[Gg]it[Hh]ub/" ":Git:")
    ("^~/[Gg]it-?[Pp]rojects/" ":Git:"))
  "List of pairs of strings used (by `sml/replacer') to create prefixes.

The first string of each pair is a regular expression, the second
is a replacement. These pairs are sequentially applied on the
file path to replace portions of it, turning them into prefixes.
For instance, \"~/.emacs.d/\" is replaced by \":ED:\", which is
shorter but easily identified.

The replacement strings can really be anything, but to be colored
as a prefix a string must start and end with \":\" (see the
default as an example, as an exception \"~/\" is also a prefix).

Replacement doesn't stop on first match, so you can have stacking replacements:

    (add-to-list 'sml/replacer-regexp-list '(\"^:DB:Org/\" \":Org:\") t)

Remember that `add-to-list' adds items to the FRONT, and you'll
usually want to add them to the back (thus the `t' at the end).

You can also set custom colors (faces) for these prefixes, just
set `sml/prefix-face-list' accordingly."
  :type '(repeat (list regexp string))
  :group 'smart-mode-line-path&prefix
  :package-version '(smart-mode-line . "1.22"))

(defcustom sml/prefix-regexp '(":\\(.*:\\)" "~/")
  "List of Regexps used to identify prefixes.

A prefix is anything at the begining of a line that matches any
of these regexps. Don't start these regexps with \"^\", the
parser applies that for you."
  :type '(repeat regexp)
  :group 'smart-mode-line-path&prefix)

(defcustom sml/prefix-face-list '((":SU:" sml/sudo)
                                  (":G" sml/git)
                                  ("" sml/prefix))
  "List of (STRING FACE) pairs used by `sml/propertize-prefix'.

After the file path is constructed, the prefix contained in it is
colored according to this list. The elements are checked one by
one and, if the prefix contains the STRING part of the pair, then
FACE is applied to it (and checking stops there)."
  :type '(repeat (list string face))
  :group 'smart-mode-line-path&prefix)

(defcustom sml/name-width 44
  "Minimum and maximum size of the file name in the mode-line.

If `sml/shorten-directory' is nil, this is the minimum width.
Otherwise, this is both the minimum and maximum width."
  :type 'integer
  :group 'smart-mode-line-path&prefix)

(defcustom sml/shorten-directory t
  "Should directory name be shortened to fit width?

When the buffer+directory name is longer than
`sml/name-width':
	if nil the rest of the mode-line is pushed right;
	otherwise the directory name is shortened to fit."
  :type 'boolean
  :group 'smart-mode-line-path&prefix
  :set 'sml/set-shortener-func)

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

(defun sml/toggle-shorten-modes (&rest val)
  "Toggle the variable `sml/shorten-modes'.

If given an argument the variable is set to the argument,
otherwise it is toggled. This can be used as an alternative to
customizing the variable with `customize-group'. Equivalent to
setting the variable with `setq'."
  (interactive)
  (setq sml/shorten-modes (if val (car val)
                            (not sml/shorten-modes)))
  (force-mode-line-update))

(defcustom sml/hidden-modes '(" hl-p")
  "List of minor modes you want to hide, or empty.

If empty (or nil), all minor modes are shown in the
mode-line. Otherwise this is a list of minor mode names that will be
hidden in the minor-modes list. 

Don't forget to start with a blank space."
  :type '(repeat string)
  :group 'smart-mode-line-mode-list)

(defcustom sml/mode-width 30
  "Integer or symbol representing the maximum and/or minimum size of the modes list in the mode-line.

If it is an integer, then the modes list width is that many
characters.

If it is the symbol `full', then the mode-list fills all the
empty space is available in the mode-line (this has the effect of
indenting right anything after the mode-list).

If `sml/shorten-modes' is nil, this is the minimum width.
Otherwise, this is both the minimum and maximum width."
  :type '(choice integer symbol)
  :group 'smart-mode-line-mode-list)

(defcustom sml/full-mode-string " +"
  "String that's appended to the minor-mode list when it's full."
  :type 'string
  :group 'smart-mode-line-mode-list)

(defcustom sml/shorten-mode-string " -"
  "String that's appended to the minor-mode list when all modes are displayed."
  :type 'string
  :group 'smart-mode-line-mode-list)

(defcustom sml/shorten-modes t
  "Should modes list be shortened to fit width?

When the modes list is longer than `sml/mode-width':
	if nil the rest of the mode-line is pushed right;
	otherwise the list is shortened to fit."
  :type 'boolean
  :group 'smart-mode-line-mode-list)

(defcustom sml/battery-format " %p"
  "Format used to display the battery in the mode-line.

Only relevant if using `display-battery-mode'. See that function
for the syntax."
  :type 'string
  :group 'smart-mode-line-others)

(defvaralias 'sml/time-format 'display-time-format
  "This variable is now obsolete. Use the standard `display-time-format'.")
;; (defcustom sml/time-format " %H:%M"
;;   "Format used to display the time in the mode-line.

;; Only relevant if `sml/show-time' is not nil."
;;   :type 'string
;;   :group 'smart-mode-line-others)

(defvaralias 'sml/show-time 'display-time-mode
  "This variable is now obsolete. Use the standard `display-time-mode'.")
;; (defcustom sml/show-time nil
;;   "Whether to show the time at the end of the mode-line."
;;   :type 'boolean
;;   :group 'smart-mode-line-others)

(defcustom sml/modified-time-string "Modified on %T %Y-%m-%d."
  "String format used for displaying the modified time.

This is shown in the tooltip when hovering over the \"modified
file\" character (which is usually a * right before the file
name."
  :type 'string
  :group 'smart-mode-line-others)

(defconst sml/major-help-echo
  "Mouse-1: mode menu.\nMouse-2: mode help.\nMouse-3: toggle minor modes.")

(defcustom sml/show-warning t
  "Should `sml/setup' warn you about baddly formated variables?"
  :type 'boolean
  :group 'smart-mode-line)

(defcustom sml/extra-filler 0
  "The number of extra filling chars to use. It comes into play when `sml/mode-width' is set to 'full.

This is necessary because the mode-line width (which we need but
don't have acess to) is larger than the window-width (which we
have access to).

Decrease this if right indentation seems to be going too far (or
if you just want to fine-tune it)."
  :type 'integer
  :group 'smart-mode-line-mode-list)

;; Color definitions
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
(defface sml/global '((t :foreground "gray50"))  
  "" :group 'smart-mode-line-faces)
(defface sml/warning (if sml/override-theme
                         '((t :inherit sml/global
                              :foreground "#bf0000"
                              :weight bold))
                       '((t :inherit font-lock-warning-face
                            :weight bold)))
  "" :group 'smart-mode-line-faces)
(defface sml/line-number '((t :inherit sml/modes
                              :weight bold))
  "" :group 'smart-mode-line-faces)
(defface sml/position-percentage '((t :inherit sml/prefix
                                      :weight normal))
  "" :group 'smart-mode-line-faces)
(defface sml/col-number '((t :inherit sml/global))
  "" :group 'smart-mode-line-faces)
(defface sml/numbers-separator '((t :inherit sml/col-number))
  "" :group 'smart-mode-line-faces)
(defface sml/client '((t :inherit sml/prefix))
  "" :group 'smart-mode-line-faces)
(defface sml/not-modified '((t :inherit sml/global))
  "" :group 'smart-mode-line-faces)
(defface sml/read-only (if sml/override-theme
                           '((t :inherit sml/global
                                :foreground "DeepSkyBlue"))
                         '((t :inherit font-lock-function-name-face)))
  "" :group 'smart-mode-line-faces)
(defface sml/outside-modified '((t :inherit sml/global
                                   :foreground "#ffffff"
                                   :background "#c82829"))
  "" :group 'smart-mode-line-faces)
(defface sml/modified '((t :inherit sml/warning))
  "" :group 'smart-mode-line-faces)
(defface sml/mule-info '((t :inherit sml/global))
  "" :group 'smart-mode-line-faces)
(defface sml/prefix (if sml/override-theme
                        '((t :inherit sml/global
                             :foreground "#bf6000"))
                      '((t :inherit font-lock-variable-name-face)))
  "" :group 'smart-mode-line-faces)
(defface sml/sudo '((t :inherit sml/outside-modified))
  "" :group 'smart-mode-line-faces)
(defface sml/git '((t :inherit sml/read-only))
  "" :group 'smart-mode-line-faces)
(defface sml/folder '((t :inherit sml/global))
  "" :group 'smart-mode-line-faces)
(defface sml/filename (if sml/override-theme
                          '((t :inherit sml/global
                               :foreground "#eab700"
                               :weight bold))
                        '((t :inherit font-lock-keyword-face
                             ;:foreground "Red"
                             :weight bold)))
  "" :group 'smart-mode-line-faces)
(defface sml/modes (if sml/override-theme
                       '((t :inherit sml/global
                            :foreground "White"))
                     '((t :inherit modeline)))
  "" :group 'smart-mode-line-faces)
(defface sml/process '((t :inherit sml/prefix))
  "" :group 'smart-mode-line-faces)
(defface sml/vc '((t :inherit sml/git))
  "" :group 'smart-mode-line-faces)
(defface sml/vc-edited '((t :inherit sml/prefix))
  "" :group 'smart-mode-line-faces)
(defface sml/charging '((t :inherit sml/global 
                           :foreground "ForestGreen"))
  "" :group 'smart-mode-line-faces)
(defface sml/discharging '((t :inherit sml/global 
                              :foreground "Red"))
  "" :group 'smart-mode-line-faces)
(defface sml/time '((t :inherit sml/modes))
  "" :group 'smart-mode-line-faces)

;; Anchors
(defconst sml/anchor-beginning '() "Anchor so that other packages can find specific positions in the mode-line.")
(defconst sml/anchor-after-status '() "Anchor so that other packages can find specific positions in the mode-line.")
(defconst sml/anchor-before-major-mode '() "Anchor so that other packages can find specific positions in the mode-line.")
(defconst sml/anchor-after-minor-modes '() "Anchor so that other packages can find specific positions in the mode-line.")
(defconst sml/simplified-mode-line-patchy-fix ""
  "Fix for filling to work with packages that manually edit the mode-line.")

(defcustom sml/show-encoding nil
  "Whether to display the buffer encoding in the mode-line."
  :type 'boolean
  :group 'smart-mode-line-others
  :package-version '(smart-mode-line . "1.20"))

(defcustom sml/show-eol nil
  "Whether to display the buffer EOL in the mode-line."
  :type 'boolean
  :group 'smart-mode-line-others)

(defcustom sml/outside-modified-char "M"
  "Char to display if buffer needs to be reverted."
  :type 'string
  :group 'smart-mode-line-others
  :package-version '(smart-mode-line . "1.20"))

(defvaralias 'sml/encoding-format 'sml/mule-info)
(defcustom sml/mule-info " %z"
  "Multilingual information. Set this to nil to hide it."
  :type '(choice string (const :tag "Don't display." nil))
  :group 'smart-mode-line-others
  :package-version '(smart-mode-line . "1.22"))

(defcustom sml/read-only-char "R"
  "Displayed when buffer is readonly."
  :type 'string
  :group 'smart-mode-line-others
  :package-version '(smart-mode-line . "1.20"))

(defcustom sml/vc-mode-show-backend nil
  "Whether to show or the backend in vc-mode's mode-line description.

I think most people only use one backend, so this defaults to nil.
If you want it to show the backend, just set it to t."
  :type 'boolean
  :group 'smart-mode-line-others    
  :package-version '(smart-mode-line . "1.22"))

(defcustom sml/mode-line-format
  `(;; This is used for some error that I've never seen happen.
    (:propertize "%e" face sml/warning)

    ;; Anchor
    sml/anchor-beginning
    
    ;; emacsclient
    (sml/show-client
     (:eval (if (frame-parameter nil 'client)
                ,(propertize "@" 'face 'sml/client 'help-echo "emacsclient frame")
              " ")))
    
    ;; Position
    (:eval
     (let ((hText (format-mode-line (format "Buffer size:\n\t%%IB\nNumber of Lines:\n\t%s\nCurrent Line:\n\t%%l"
                                            (line-number-at-pos (point-max))))))
       `((column-number-mode (:eval (propertize sml/col-number-format  'face 'sml/col-number        'help-echo ,hText)))
         (column-number-mode
          (line-number-mode  (:eval (propertize sml/numbers-separator  'face 'sml/numbers-separator 'help-echo ,hText))))
         (line-number-mode   (:eval (propertize sml/line-number-format 'face 'sml/line-number       'help-echo ,hText))))))
    
    ;; Encoding
    (sml/show-encoding
     (:eval (propertize sml/mule-info
                        'face 'sml/mule-info
                        'help-echo 'mode-line-mule-info-help-echo
                        'mouse-face 'mode-line-highlight
                        'local-map mode-line-coding-system-map))) 

    ;; EOL
    (sml/show-eol
     (:eval (propertize (mode-line-eol-desc)
                        'face 'sml/mule-info)))
    
    ;; Modified status
    (:eval
     (cond ;; ((file-remote-p (buffer-file-name)) "%1+")
      ((not (or (and (buffer-file-name) (file-remote-p buffer-file-name))
                (verify-visited-file-modtime (current-buffer))))
       (propertize sml/outside-modified-char 'face 'sml/outside-modified
                   'help-echo "Modified outside Emacs!\nRevert first!"))
      (buffer-read-only (propertize sml/read-only-char
                                    'face 'sml/read-only
                                    'help-echo "Read-Only Buffer"))
      ((buffer-modified-p)
       (propertize sml/modified-char
                   'face 'sml/modified
                   'help-echo (if (buffer-file-name)
                                  (format-time-string
                                   sml/modified-time-string
                                   (nth 5 (file-attributes (buffer-file-name))))
                                "Buffer Modified")
                   'local-map '(keymap (mode-line keymap (mouse-1 . save-buffer)))))
      (t " ")))

    
    ;; Anchor
    sml/anchor-after-status
    
    ;; Full path to buffer/file name
    (:eval
     (let* ((prefix (sml/get-prefix (sml/replacer (abbreviate-file-name (sml/get-directory)))))
            (bufname (sml/buffer-name))
            ;; (if (and (buffer-file-name) (file-directory-p (buffer-file-name)))
            ;; 			   "" (buffer-name))
            (dirsize (max 0 (- (abs sml/name-width) (length prefix) (length bufname))))
            (dirstring (funcall sml/shortener-func (sml/get-directory) dirsize)))
       
       (propertize (concat (sml/propertize-prefix (replace-regexp-in-string "%" "%%" prefix))
                           (propertize (replace-regexp-in-string "%" "%%" dirstring) 'face 'sml/folder)
                           (propertize (replace-regexp-in-string "%" "%%" bufname) 'face 'sml/filename)
                           (make-string (max 0 (- dirsize (length dirstring))) ?\ ))
                   'help-echo (format "%s\n\nmouse-1: Previous buffer\nmouse-3: Next buffer"
                                      (or (buffer-file-name) (buffer-name)))
                   'mouse-face 'mode-line-highlight
                   'local-map   mode-line-buffer-identification-keymap)))
    
    (-4 (:eval (propertize sml/position-percentage-format 'face 'sml/position-percentage 'help-echo
                           (format-mode-line (format "Buffer size:\n\t%%IB\nNumber of Lines:\n\t%s\nCurrent Line:\n\t%%l"
                                                     (line-number-at-pos (point-max)))))))
    
    ;; Anchor
    sml/anchor-before-major-mode
    
    " "
    ;; The modes list
    (:eval (propertize (format-mode-line mode-name)
                       'mouse-face  'mode-line-highlight
                       'face        'sml/modes
                       'local-map   mode-line-major-mode-keymap
                       'help-echo   sml/major-help-echo))

    ;; vc-mode
    (vc-mode vc-mode)
    
    ;; The mode line process, doesn't get counted into the width
    ;; limit.
    (:eval (propertize (format-mode-line
                        (if (stringp mode-line-process)
                            (replace-regexp-in-string "%" "%%%%" mode-line-process)
                          mode-line-process)) ;("" mode-line-process)
                       'face       'sml/process))
    
    ;; Minor modes list
    (:eval (sml/extract-minor-modes minor-mode-alist sml/mode-width))
    
    ;; Anchor
    sml/anchor-after-minor-modes
    
    ;; Extra strings. I know that at least perpective, mew, and battery use this ;; global-mode-string 
    mode-line-misc-info
    
    ;; add the time, with the date and the emacs uptime in the tooltip
    ;; (sml/show-time
    ;;  (:eval (propertize (format-time-string sml/time-format)
    ;;                     'face 'sml/time
    ;;                     'help-echo (concat (format-time-string "%c;")
    ;;                                        (emacs-uptime "\nUptime: %hh")))))
    )
  "Mode-line format to be applied when you activate `sml/setup'."
  :type 'list
  :group 'smart-mode-line;-mode-line
  :package-version '(smart-mode-line . "1.18"))

;;;###autoload
(defun sml/setup (&optional arg)
  "Setup the mode-line, or revert it.

If argument is a non-positive integer, revert any changes made.
Otherwise, setup the mode-line.

This should be called after any themes have been applied, which
is why it is better to add as an `after-init-hook' than to be
called straight from your init file."
  (interactive)
  (setq sml/simplified nil)
  (if (and (integerp arg) (< arg 1))
      (sml/revert)
    ;; We use this to make the mode-line readable in lighter backgrounds
    (when sml/override-theme (sml/set-face-color nil nil))
    ;; This is a warning for people not to use the old syntax. Should probably remove this eventually.
    (when sml/show-warning (sml/check-hidden-modes))

    (setq battery-mode-line-format sml/battery-format)

    ;;; And this is where the magic happens.
    ;; Set the mode-line
    (setq-default mode-line-format sml/mode-line-format)

    ;;;; And here comes support for a bunch of extra stuff. Some of
    ;;;; these are just needed for coloring, and some are needed
    ;;;; because the package would manually edit the mode-line (and
    ;;;; thus be invisible to us).
    
    ;; Display time
    (add-hook 'display-time-hook
              (lambda () (when (stringp display-time-string)
                           (setq display-time-string
                                 (propertize display-time-string
                                             'face 'sml/time)))))
    ;; Battery support
    (eval-after-load 'battery
      '(defadvice battery-update (after sml/after-battery-update-advice () activate)
         "Change battery color."
         (when battery-mode-line-string
           (setq battery-mode-line-string
                 (propertize battery-mode-line-string
                             'face 'sml/battery)))))
    
    ;; Perspective support
    (eval-after-load "perspective"
      '(progn
         (defcustom sml/persp-selected-color "Green"
           "Replace `persp-selected-color', otherwise it's unreadable."
           :type 'string
           :group 'smart-mode-line-others)
         (set-face-foreground 'persp-selected-face sml/persp-selected-color)))
    
    ;; vc-mode
    (eval-after-load "vc-hooks"
      '(defadvice vc-mode-line (after sml/after-vc-mode-line-advice () activate)
         "Color `vc-mode'."
         (when (stringp vc-mode)
           (let ((noback (replace-regexp-in-string (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode)))
             (setq vc-mode
                   (propertize (if sml/vc-mode-show-backend vc-mode noback)
                               'face (cond ((string-match "^ -" noback)    'sml/vc)
                                           ((string-match "^ [:@]" noback) 'sml/vc-edited)
                                           ((string-match "^ [!\\?]" noback) 'sml/warning))))))))
    
    ;; evil support
    (eval-after-load "evil-core"
      '(sml/fix-evil-mode))
    
    ;; Mew support
    (eval-after-load "mew-net"
      '(progn
         (defgroup smart-mode-line-mew '() "Group for editing the mew-support variables." :group 'smart-mode-line)
         (defcustom sml/mew-support t
           "Whether to flash the mode-line when mew detects new mail."
           :type 'boolean :group 'smart-mode-line-mew
           :package-version '(smart-mode-line . "1.11"))
         (defcustom sml/new-mail-background-color "#110000"
           "When new mail arrives, mode-line background will be tinted this color.

Only works with mew-biff. Right now it stays colored until you
read the mail, so this color should probably be something sutil.
Might implement a quick flash eventually."
           :type 'color :group 'smart-mode-line-mew
           :package-version '(smart-mode-line . "1.11"))
         (defcustom sml/mew-biff-format (concat "%2d" (if (char-displayable-p ?✉) "✉" "M"))
           "Format used for new-mail notifications if you use mew with biff."
           :type 'string :group 'smart-mode-line-mew
           :package-version '(smart-mode-line . "1.11"))
         (defadvice mew-biff-clear (around sml/mew-biff-clear-advice activate)
           "Advice used to customize mew-biff-bark to fit sml's style."
           ad-do-it 
           (when sml/mew-support
             ;; Remove the color
             (set-face-attribute 'mode-line nil :background sml/active-background-color)))
         (defadvice mew-biff-bark (around sml/mew-biff-bark-advice (n) activate)
           "Advice used to customize mew-biff-bark to fit sml's style."
           ad-do-it
           (when sml/mew-support
             ;; Remove the color if mail has been read.
             (if (= n 0) (set-face-attribute 'mode-line nil :background sml/active-background-color)
               ;; Apply color if there's mail. (mew-biff-bark 100)
               (set-face-attribute 'mode-line nil :background sml/new-mail-background-color)
               (setq mew-biff-string (format sml/mew-biff-format n)))))))

    ;; sml-modeline support
    (eval-after-load "sml-modeline"
      '(sml/sml-modeline-support))

    ;; sml-modeline support
    (eval-after-load "nyan-mode"
      '(sml/nyan-support))

    (unless (and (boundp 'erc-track-position-in-mode-line)
                 (null erc-track-position-in-mode-line))
      (setq erc-track-position-in-mode-line t))))

(defun sml/sml-modeline-support ()
  "Create a variable regarding `sml-modeline-mode' and insert `sml-modeline-create' in one of the anchors."
  ;; Define the variable which specifies the position
  (defcustom sml/sml-modeline-position 'sml/anchor-after-minor-modes
    "In which anchor should sml-modeline be inserted?

Value must be a symbol, the name of the anchor. Possible anchors are:
`sml/anchor-beginning'
`sml/anchor-after-status'
`sml/anchor-before-major-mode'
`sml/anchor-after-minor-modes'"
    :type 'symbol
    :group 'smart-mode-line
    :package-version '(smart-mode-line . "1.14"))
  ;; If the mode is already activated, insert the creator
  (when sml-modeline-mode
    (add-to-list sml/sml-modeline-position '(:eval (sml-modeline-create))))
  ;; Remove and insert the creator when the mode is toggled
  (defadvice sml-modeline-mode (after sml/after-sml-modeline-mode-advice () activate)
    "Insert and remove `sml-modeline-create' from the anchor specified in `sml/sml-modeline-position'."
    (if sml-modeline-mode
        (add-to-list sml/sml-modeline-position '(:eval (sml-modeline-create)))
      (setq sml/anchor-beginning (delete '(:eval (sml-modeline-create)) sml/anchor-beginning))
      (setq sml/anchor-after-status (delete '(:eval (sml-modeline-create)) sml/anchor-after-status))
      (setq sml/anchor-before-major-mode (delete '(:eval (sml-modeline-create)) sml/anchor-before-major-mode))
      (setq sml/anchor-after-minor-modes (delete '(:eval (sml-modeline-create)) sml/anchor-after-minor-modes)))))

(defun sml/nyan-support ()
  "Create a variable regarding `nyan-mode' and insert `nyan-create' in one of the anchors."
  ;; Define the variable which specifies the position
  (defcustom sml/nyan-position 'sml/anchor-before-major-mode
    "In which anchor should nyan be inserted?

Value must be a symbol, the name of the anchor. Possible anchors are:
`sml/anchor-beginning'
`sml/anchor-after-status'
`sml/anchor-before-major-mode'
`sml/anchor-after-minor-modes'"
    :type 'symbol
    :group 'smart-mode-line
    :package-version '(smart-mode-line . "1.14"))
  ;; If the mode is already activated, insert the creator
  (when nyan-mode
    (add-to-list sml/nyan-position '(:eval (nyan-create))))
  ;; Remove and insert the creator when the mode is toggled
  (defadvice nyan-mode (after sml/after-nyan-mode-advice () activate)
    "Insert and remove `nyan-create' from the anchor specified in `sml/nyan-position'."
    (if nyan-mode
        (add-to-list sml/nyan-position '(:eval (nyan-create)))
      (setq sml/anchor-beginning (delete '(:eval (nyan-create)) sml/anchor-beginning))
      (setq sml/anchor-after-status (delete '(:eval (nyan-create)) sml/anchor-after-status))
      (setq sml/anchor-before-major-mode (delete '(:eval (nyan-create)) sml/anchor-before-major-mode))
      (setq sml/anchor-after-minor-modes (delete '(:eval (nyan-create)) sml/anchor-after-minor-modes)))))

(defun sml/buffer-name ()
  "Uses `sml/show-file-name' to decide between buffer name or file name to show on the mode-line.

Unless `sml/strip-N' is nil, prevents the \"<N>\" (used in
duplicated buffer names) from being displayed."
  (if (and sml/show-file-name (buffer-file-name))
      (file-name-nondirectory (buffer-file-name))
    (if sml/show-trailing-N
        (buffer-name)
      (replace-regexp-in-string "<[0-9]+>$" "" (buffer-name)))))

(defun sml/simplified-extract-minor-modes (ml maxSize)
  "Simplified version of `sml/extract-minor-modes'. Used for width calculation."
  (if (and (integerp maxSize) sml/shorten-modes)
      (make-string (max 0 maxSize) ?\ )
    (if (equal maxSize 'full) 
        ""
      (let* ((nameList (sml/mode-list-to-string-list (reverse ml)))
             (out nil))
        (dolist (name nameList out)
          (unless (member name sml/hidden-modes) ; :test #'equal
            ;; Append the next one.
            (add-to-list 'out name)))))))

(defun sml/fill-width-available ()
  "Return the size available for filling."
  (max 0
       (+ sml/extra-filler 
          (- (window-width)
             (let ((sml/simplified t))
               (length (format-mode-line mode-line-format)))))))

(defun sml/check-hidden-modes ()
  "Checks if `sml/hidden-modes' is using the new syntax. 

New syntax means the items should start with a space."
  (dolist (cur sml/hidden-modes)
    (unless (eq ?\  (string-to-char cur))
      (warn "[sml]Strings in `sml/hidden-modes' should start with a space (\" \").\nTo stop showing this message, toggle `sml/show-warning.'")
      (return)))) 

(defun sml/mode-list-to-string-list (ml) ;;Credits to Constantin
  "Try to read the mode-list (which contains almost anything) and return a sensible list of strings."     
  (case (type-of ml)
    ('string (list ml))
    ('symbol
     (if ml
         (sml/mode-list-to-string-list (symbol-value ml) )
       nil))
    (('function 'subr) (sml/mode-list-to-string-list (list (funcall ml))))
    ('cons
     (let ((kar (car ml))
           (kdr (cdr ml)))
       (case (type-of kar)
         ('symbol
          (let ((val (symbol-value kar))
                (kadr (if (listp kdr) (car kdr) nil)))
            (case val
              (:eval (sml/mode-list-to-string-list (eval kadr) ))
              ;; properties now not handlet properly
              (:propertize (sml/mode-list-to-string-list kdr ))
              (t
               (if val
                   (sml/mode-list-to-string-list kadr)
                 (sml/mode-list-to-string-list (cdr kdr)))))))
         ('integer
          ;; heh, now do nothing, must reduce max width if < 0 or do padding if > 0
          (sml/mode-list-to-string-list kdr ))
         (t (append (sml/mode-list-to-string-list kar ) (sml/mode-list-to-string-list kdr ))))))
    ;; unknown
    (t ;;(message "mode-list-to-string-error Unknown: type: %s;\nval: %s" ml (type-of ml))
     (list (format "%s" ml)))))

(defun sml/extract-minor-modes (ml maxSize)
  "Extracts all rich strings necessary for the minor mode list."
  (if sml/simplified
      (sml/simplified-extract-minor-modes ml maxSize)
    (let* ((nameList (sml/mode-list-to-string-list (reverse ml)))
           (out nil)
           (size (if (equal maxSize 'full) (sml/fill-width-available)
                   maxSize))
           (helpString (concat "Full list:\n  "
                               (mapconcat 'identity nameList "\n  ")))
           (keymap '(keymap (mode-line keymap (mouse-1 . sml/toggle-shorten-modes))))
           (propertized-full-mode-string (propertize sml/full-mode-string
                                                     'help-echo "mouse-1: Show all modes"
                                                     'face 'sml/folder
                                                     'local-map keymap
                                                     'mouse-face 'mode-line-highlight))
           (propertized-shorten-mode-string (propertize sml/shorten-mode-string
                                                        'help-echo "mouse-1: Shorten minor modes"
                                                        'face 'sml/folder
                                                        'local-map keymap
                                                        'mouse-face 'mode-line-highlight)))
      (dolist (name nameList out)
        (unless (member name sml/hidden-modes) ; :test #'equal
          ;; If we're shortenning, check if it fits
          (when (and sml/shorten-modes (< size (length name)))
            ;; If the remaining size is too small even for the
            ;; `sml/full-mode-string', get rid of the last string.
            ;; (This won't work perfectly if the last string is
            ;; smaller then `sml/full-mode-string', but that should be
            ;; rare.)
            (when (< size (length sml/full-mode-string)) (setq out (cdr out)))
            (add-to-list 'out propertized-full-mode-string t)
            (decf size (length sml/full-mode-string))
            (return))
          ;; If it fits or we're not shortenning, append the next one.
          (decf size (length name))
          (add-to-list 'out (propertize name
                                        'help-echo helpString
                                        'mouse-face 'mode-line-highlight
                                        'face 'sml/folder
                                        'local-map mode-line-minor-mode-keymap))))
      ;; If we're not shortenning, at the " -" at the end.
      (unless sml/shorten-modes
        (decf size (length propertized-shorten-mode-string))
        (add-to-list 'out propertized-shorten-mode-string t))
      ;; Fill with spaces, unless size is negative.
      (append out (list (propertize (make-string (max 0 size) ?\ )
                                    'help-echo helpString
                                    'face 'sml/folder))))))

(defun sml/propertize-prefix (prefix)
  "Set the color of the prefix according to its contents."
  (let ((out prefix))
    (dolist (pair sml/prefix-face-list)
      (if (string-match (car pair) prefix)
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
        ((and (listp mode-name) (string-match "Dired" (car mode-name)))
         (replace-regexp-in-string "/[^/]*/$" "/" default-directory))
        (t "")))

(defun sml/set-battery-font ()
  "Set `sml/battery' face depending on battery state."
  (let ((data (and (boundp battery-status-function) battery-status-function (funcall battery-status-function))))
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
        (if (string-match "\\(" sml/prefix-regexp) 
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
        output))))

(defun sml/fix-evil-mode ()
  "Fix for the way evil-mode implements their 'before and 'after positions."
  ;; (setq sml/simplified-mode-line-patchy-fix " <E> ")
  (if (eq evil-mode-line-format 'before)
      (setq evil-mode-line-format '(before . sml/anchor-beginning))
    (if (eq evil-mode-line-format 'after)
        (setq evil-mode-line-format '(after . sml/anchor-after-minor-modes))))
  ;; (when (eq sml/mode-width 'full)
  ;; (warn "Setting `sml/mode-width' to full does not work well with evil-mode automatically. You need to either set "))
  )

(defun sml/set-face-color (sym val)
  (if sym (set-default sym val))
  (set-face-attribute 'mode-line nil
                      :foreground sml/active-foreground-color
                      :background sml/active-background-color)
  (set-face-attribute 'mode-line-inactive nil
                      :background sml/inactive-background-color
                      :foreground sml/inactive-foreground-color))
;; Backup the original configs, just in case.
(defconst sml/format-backup mode-line-format
  "Backs up the `mode-line-format' before SML was required.")

(defconst sml/battery-format-backup (if (boundp 'battery-mode-line-format) battery-mode-line-format "")
  "Backs up the `battery-mode-line-format' before SML was required.")

(copy-face 'mode-line 'sml/active-backup)
(copy-face 'mode-line-inactive 'sml/inactive-backup)


(provide 'smart-mode-line)



;;; smart-mode-line.el ends here

