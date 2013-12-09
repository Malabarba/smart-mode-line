;;; smart-mode-line.el --- A color coded smart mode-line.

;; Copyright (C) 2012 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/smart-mode-line
;; Version: 2.3.3
;; Package-Requires: ((emacs "24.3") (dash "2.2.0"))
;; Keywords: faces frames
;; Prefix: sml
;; Separator: /

;;; Commentary:
;;
;; Smart Mode Line is a sexy mode-line for Emacs, that aims to be easy to
;; read from small to large monitors by using a *prefix feature* and
;; *smart truncation*.
;;
;; Installation
;; ===
;; Make sure "smart-mode-line.el" is in your load path, then place
;; this code in your .emacs file:
;;
;;     (setq sml/theme 'dark)
;;     (require 'smart-mode-line)
;;     (sml/setup)
;;
;; Features
;; ===
;; Its main features include:
;;
;;  1. **Color coded**:
;;     Highlights the most important information for you
;;     (buffer name, modified state, line number). Don't
;;     like the colors? See item 5)!
;;
;;  2. **Fixed width** (if you want):
;;     Lets you set a maxium width for the path name and mode names, and
;;     truncates them intelligently (truncates the directory, not the
;;     buffer name). Also let's you **right indent** strings in the
;;     mode-line (see `sml/mode-width').
;;
;;  3. **Directory as Prefixes**:
;;     Prefix feature saves a LOT of space. e.g. "~/.emacs.d/"
;;     is translated to ":ED:" in the path (open a file inside
;;     this folder to see it in action). Long path names you
;;     are commmonly working on are displayed as short
;;     abbreviations. Set your own prefixes to make best use
;;     of it (by configuring `sml/replacer-regexp-list'). Mousing
;;     over the abbreviated path will show you the full
;;     path. See below for examples.
;;
;;  4. **Hide minor-modes**:__
;;     Hidden-modes feature saves even more space. Select
;;     which minor modes you don't want to see listed by
;;     customizing the `sml/hidden-modes' variable. This will
;;     filter out the modes you don't care about and unclutter
;;     the modes list (mousing over the modes list still shows
;;     the full list).
;;
;;  5. **Very easy to configure**:
;;     All colors and variables are customizable. You can change the
;;     whole theme with `sml/apply-theme', or just customize anything
;;     manually with `sml/customize' and `sml/customize-faces'. There are
;;     *DOZENS* of variables to customize your mode-line, just pop over
;;     there and have a look!
;;
;;  6. **Compatible with absolutely anything**:
;;     I'm serious. Versions 2.0 and above should be compatible with
;;     **any** other packages that display information in the mode-line
;;     (evil, nyan-mode, elscreen, display-battery-mode, etc). If you
;;     find *ANYTHING* that does not appear as it should, file a bug report
;;     and I'll get to it.
;;
;; Important Variables:
;; ===
;; All variables can be edited by running `sml/customize', and the
;; documentations are mostly self explanatory, I list here only the
;; most important ones.
;;
;;  1. `sml/shorten-directory' and `sml/shorten-modes'
;;   Choose what theme ou want to use for the mode-line colors. For now
;;   there are 3 different themes: `dark', `light', and `respectful'.
;;
;;  1. `sml/shorten-directory' and `sml/shorten-modes'
;;   Setting both of these to `t' garantees a fixed width mode-line
;;   (directory name and minor-modes list will be truncated to fit). To
;;   actually define the width, see below.
;;
;;  2. `sml/name-width' and `sml/mode-width'
;;   Customize these according to the width of your emacs frame. I set
;;   them to `40' and `full' respectively, and the mode-line fits
;;   perfectly when the frame is split in two even on my laptop's small
;;   17" monitor. `full' means everything after the minor-modes will be
;;   right-indented.
;;
;;  3. `sml/replacer-regexp-list'
;;   This variable is a list of (REGEXP REPLACEMENT) that is used
;;   to parse the path. The replacements are applied
;;   sequentially. This allows you to greatly abbreviate the path
;;   that's shown in the mode-line. If this abbreviation is of
;;   the form ":SOMETHING:", it is considered a prefix and get's
;;   a different color (you can change what's considered a prefix
;;   by customizing `sml/prefix-regexp').
;;   For example, if you do a lot of work on a folder called
;;   "~/Dropbox/Projects/In-Development/" almost half the
;;   mode-line would be occupied just by the folder name, which
;;   is much less important than the buffer name. But, you can't
;;   just hide the folder name, since editting a file in
;;   "~/Dropbox/Projects/In-Development/Source" is VERY different
;;   from editting a file in "~/Dropbox/Projects/Source". By
;;   setting up a prefix for your commonly used folders, you get
;;   all that information without wasting all that space. In this
;;   example you could set the replacement to ":ProjDev:" or just
;;   ":InDev:", so the path shown in the mode-line will be
;;   ":ProjDev:Source/" (saves a lot of space without hiding
;;   information).
;;
;; Here go some more useful examples:
;;
;;     (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/Projects/In-Development/" ":ProjDev:"))
;;     (add-to-list 'sml/replacer-regexp-list '("^~/Documents/Work/" ":Work:"))
;;
;;     ;; Added in the right order, they even work sequentially:
;;     (add-to-list 'sml/replacer-regexp-list '("^:DB:Documents" ":DDocs:"))
;;     (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:"))
;;     (add-to-list 'sml/replacer-regexp-list '("^:Git:\\(.*\\)/src/main/java/" ":G/\\1/SMJ:"))
;;     (add-to-list 'sml/replacer-regexp-list '("^~/Git-Projects/" ":Git:"))

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
;; 2.3.3   - 2013/12/09 - Fix sml/get-directory for files attached to mails - Thanks tsdh.
;; 2.3.2   - 2013/12/07 - Fix for themes which set :inverse-video t in the mode-line.
;; 2.3.1   - 2013/12/04 - sml/show-frame-identification now always defaults to nil.
;; 2.3.1   - 2013/12/04 - Fix for sml/show-client not working.
;; 2.3     - 2013/12/04 - sml/show-frame-identification only t for terminals.
;; 2.3     - 2013/12/03 - Mark boolean's as safe-local-variables.
;; 2.2.3   - 2013/12/03 - Fix possible recursion in sml/apply-theme.
;; 2.2.2   - 2013/11/27 - Fix sml/apply-theme to consider saved faces.
;; 2.2.1   - 2013/11/27 - Fix doc for sml/show-frame-identification.
;; 2.2     - 2013/11/26 - Better minor list and some fixes.
;; 2.0.5   - 2013/11/24 - sml/revert no longer available.
;; 2.0.4   - 2013/11/24 - Improved faces a little.
;; 2.0.3.4 - 2013/11/15 - Workaround to prevent core dump.
;; 2.0.3.3 - 2013/11/13 - Small fix on sml/generate-buffer-identification for man pages.
;; 2.0.3.2 - 2013/11/12 - sml/filter-mode-line-list now uses remove nil.
;; 2.0.3.1 - 2013/11/08 - Quick fix sml/get-directory.
;; 2.0.3   - 2013/11/07 - sml/show-frame-identification.
;; 2.0.3   - 2013/11/07 - Improvements to sml/parse-mode-line-elements.
;; 2.0.3   - 2013/11/07 - sml/compile-position-construct.
;; 2.0.3   - 2013/11/07 - Line-number removed from sml/generate-position-help.
;; 2.0.3   - 2013/11/07 - Position optimization with sml/position-construct.
;; 2.0.3   - 2013/11/07 - Performance optimization thanks to sml/buffer-identification.
;; 2.0.2   - 2013/11/05 - better sml/replacer-regexp-list.
;; 2.0.2   - 2013/11/05 - sml/mule-info also hides input system.
;; 2.0.2   - 2013/11/05 - show-encoding is now alias for sml/mule-info.
;; 2.0.2   - 2013/11/05 - Removed anchors.
;; 2.0.1   - 2013/11/04 - Slight fix on sml/apply-theme
;; 2.0     - 2013/11/04 - Remove unnecessary functions.
;; 2.0     - 2013/11/04 - Many other internal improvements.
;; 2.0     - 2013/11/02 - Remove sml/mode-line-format
;; 2.0     - 2013/11/02 - Reduce huge spaces in mode-line-format
;; 2.0     - 2013/11/02 - Redesign the format to use mode-line-mule-info.
;; 2.0     - 2013/11/02 - Redesign the format to use mode-line-client.
;; 2.0     - 2013/11/02 - Redesign the format to use mode-line-modified.
;; 2.0     - 2013/11/02 - Redesign the format to use mode-line-remote.
;; 2.0     - 2013/11/02 - Manually edits mode-line-front-space.
;; 2.0     - 2013/11/02 - Manually edits mode-line-frame-identification.
;; 2.0     - 2013/11/02 - Manually edits mode-line-buffer-identification.
;; 2.0     - 2013/11/02 - Manually edits mode-line-end-spaces.
;; 2.0     - 2013/11/02 - Redesign the format to use mode-line-modes.
;; 2.0     - 2013/11/01 - Redesign the format to use mode-line-position.
;; 1.30.1  - 2013/10/21 - eval-when-compile cl
;; 1.30    - 2013/10/13 - Click mode list to toggle minor-mode hiding.
;; 1.29.2  - 2013/10/02 - Different default position-percentage face.
;; 1.29.1  - 2013/08/22 - Fix hang introduced with last update.
;; 1.29    - 2013/08/11 - Fixed lag with remote files.
;; 1.28.1  - 2013/08/11 - Fix for the erc fix.
;; 1.28    - 2013/08/11 - Fixing erc notifications.
;; 1.27    - 2013/08/10 - Changed default value of sml/mode-width to a number. 'full didn't work for everyone.
;; 1.27    - 2013/08/10 - Doc bug.
;; 1.26    - 2013/07/18 - Fix for % in the process string.
;; 1.25    - 2013/07/16 - sml/override-theme also tries to set good colors for the text (not just the background).
;; 1.24    - 2013/07/16 - sml/mule-info face changed to be less important.
;; 1.23.2  - 2013/07/15 - Changed doc of sml/replacer-regexp-list.
;; 1.23.1  - 2013/07/15 - moved perspective variable to eval-after-load.
;; 1.23    - 2013/07/15 - added an icon to mew-format.
;; 1.23    - 2013/07/15 - obsolete sml/show-time.
;; 1.23    - 2013/07/15 - fixed a bug which required emacs restart for changes to take effect.
;; 1.22    - 2013/07/15 - sml/vc-mode-show-backend implemented.
;; 1.22    - 2013/07/15 - move mew-support variable.
;; 1.22    - 2013/07/15 - Changed default value of sml/replacer-regexp-list.
;; 1.21    - 2013/07/14 - Encoding description.
;; 1.21    - 2013/07/14 - Reestructured some of the present functions.
;; 1.21    - 2013/07/14 - New position indicator.
;; 1.20    - 2013/07/14 - vc-mode support.
;; 1.19    - 2013/07/14 - Reorganized groups.
;; 1.18    - 2013/07/12 - mew variables only get created if mew is loaded.
;; 1.18    - 2013/07/12 - Reformulated the simplified mode-line.
;; 1.18    - 2013/07/12 - Added number of lines to mouse tooltip of position.
;; 1.17    - 2013/07/10 - Fallback 'modified' string.
;; 1.16    - 2013/07/08 - Changed implementation of battery display.
;; 1.16    - 2013/07/08 - Fixed battery-display.
;; 1.15    - 2013/07/06 - Implemented sml-modeline support.
;; 1.14    - 2013/06/25 - Slightly reduced the default value of extra-filler.
;; 1.13    - 2013/06/10 - removed 'cl requirement.
;; 1.13    - 2013/06/10 - Advice to mew-biff-clear.
;; 1.12    - 2013/06/06 - Gigantic typo fix. Sorry about that.
;; 1.11    - 2013/06/05 - Added biff support.
;; 1.10    - 2013/05/24 - Fix for buffer name with '%'.
;; 1.9     - 2013/05/13 - Now uses file name instead of buffer-name by default, controled by `sml/show-file-name'.
;; 1.9     - 2013/05/13 - When showing buffer name, can strip the <N> part by setting `sml/show-trailing-N'.
;; 1.8.3   - 2013/04/21 - Fixed first line of docs.
;; 1.8.2   - 2013/04/18 - added empty anchors throughout the mode-line.
;; 1.8.2   - 2013/04/18 - evil-mode support.
;; 1.8.1   - 2013/04/17 - sml/bug-report function.
;; 1.8.1   - 2013/04/17 - sml/override-theme variable.
;; 1.8.1   - 2013/04/17 - Changed install instruction to override theme settings.
;; 1.8     - 2013/04/14 - sml/mode-width can now be 'full.
;; 1.7.1   - 2012/11/17 - Perspective support.
;; 1.7     - 2012/11/14 - Fixed some modes not showing in the minor mode list - Thanks Constantin.
;; 1.7     - 2012/11/14 - Fixed infinite loop.  - Thanks Constantin.
;; 1.7     - 2012/11/14 - Fixed for dired-mode.
;; 1.7     - 2012/11/14 - Added parent customize groups.
;; 1.6.2   - 2012/07/13 - Fixed mode shortenning.
;; 1.6.1   - 2012/07/12 - NEW FEATURE: Modes list now fully supports clicking.
;; 1.6.1   - 2012/07/12 - NEW FEATURE: `sml/version' constant.
;; 1.6.1   - 2012/07/12 - `sml/hidden-modes' is now a list of strings (not regexps).
;; 1.6     - 2012/07/09 - NEW FEATURE: Customizable faces for the prefix, see `sml/prefix-face-list'.
;; 1.5.4   - 2012/06/28 - Optimized regexp-replacer.
;; 1.5.3   - 2012/06/20 - Remove prefix and folder for non-files. Color the :Git prefix.
;; 1.5.2   - 2012/06/14 - Saner default widths and mode-name fix for Term.
;; 1.5.1   - 2012/06/12 - Fixed battery font for corner cases.
;; 1.5     - 2012/06/11 - Added support for display-battery-mode. See the description for more.
;;; Code:

(eval-when-compile (require 'cl))
(require 'dash)
(require 'custom)
(require 'cus-face)

(defconst sml/version "2.3.3" "Version of the smart-mode-line.el package.")
(defconst sml/version-int 55 "Version of the smart-mode-line.el package, as an integer.")
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
  :group 'convenience
  :prefix 'sml)
(defgroup smart-mode-line-position '()
  "Group for editing the major/minor mode list."
  :group 'smart-mode-line
  :prefix 'sml)
(defgroup smart-mode-line-path&prefix '()
  "Group for editing the path, buffer-name, and prefix."
  :group 'smart-mode-line
  :prefix 'sml)
(defgroup smart-mode-line-mode-list '()
  "Group for editing the major/minor mode list."
  :group 'smart-mode-line
  :prefix 'sml)
(defgroup smart-mode-line-others '()
  "Group for editing the major/minor mode list."
  :group 'smart-mode-line
  :prefix 'sml)

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
  :prefix 'sml
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

(define-obsolete-variable-alias 'sml/time-format 'display-time-format)
(define-obsolete-variable-alias 'sml/show-time 'display-time-mode)
(define-obsolete-variable-alias 'sml/override-theme 'sml/theme)

(defcustom sml/theme 'dark
  "Defines which theme `smart-mode-line' should use.

This can be 'dark, 'light or 'respectful.

Setting this to 'light and 'dark will apply some predefined
colors to the mode-line, which are designed to be easy to read.

Setting it to 'respectful will try to use the colors defined by
your current emacs theme (emphasis on the \"try\"). This will
make the mode-line colors more consistent with buffer colors, but
it's a bit of a shot in the dark. The result will vary for each
color theme, and you may get colors that don't read well.

But don't forget, ALL COLORS ARE CUSTOMIZABLE!
`sml/customize-faces'
Any color you change manually won't get affected by this
variable.

Setting this variable via `setq' only has effect BEFORE loading
`smart-mode-line'. If smart-mode-line is already loaded, use
either `sml/apply-theme' or the customization interface
instead (M-x customize-variable RET sml/theme RET)."
  :type '(choice (const :tag "Use a dark color-theme. (Default)" dark)
                 (const :tag "Use a light color-theme." light)
                 (const :tag "Respect the color-theme's colors." nil))
  :set 'sml/apply-theme
  :initialize 'custom-initialize-default
  :group 'smart-mode-line-faces :group 'smart-mode-line)

(defcustom sml/position-percentage-format "%p"
  "Format used to display position in the buffer.

Set it to nil to hide the number."
  :type 'string
  :group 'smart-mode-line-position
  :package-version '(smart-mode-line . "2.0"))
(put 'sml/position-percentage-format 'risky-local-variable t)

(defcustom sml/line-number-format "%3l"
  "Format used to display line number.

Empty it or disable `line-number-mode' to hide the number."
  :type 'string
  :group 'smart-mode-line-position
  :set 'sml/compile-position-construct
  :initialize 'custom-initialize-default)
(put 'sml/line-number-format 'risky-local-variable t)

(defcustom sml/size-indication-format "%I "
  "Format to display buffer size when `size-indication-mode' is on."
  :type 'string
  :group 'smart-mode-line-position
  :package-version '(smart-mode-line . "2.0")
  :set 'sml/compile-position-construct
  :initialize 'custom-initialize-default)
(put 'sml/size-indication-format 'risky-local-variable t)

(defcustom sml/col-number-format "%2c"
  "Format used to display column number.

Empty it or disable `column-number-mode' to hide the number."
  :type 'string
  :group 'smart-mode-line-position
  :set 'sml/compile-position-construct
  :initialize 'custom-initialize-default)
(put 'sml/col-number-format 'risky-local-variable t)

(defcustom sml/numbers-separator ":"
  "Separator between line and column number.

Since we use different faces for line and column number, you can
just set this to \"\" to save an extra char of space."
  :type 'string
  :group 'smart-mode-line-position)

(defcustom sml/show-client nil
  "Whether to show an \"@\" for emacsclient frames."
  :type 'boolean
  :group 'smart-mode-line-others)
(put 'sml/show-client 'safe-local-variable 'booleanp)

(defcustom sml/modified-char (char-to-string (if (char-displayable-p ?×) ?× ?*))
  "String that indicates if buffer is modified. Should be one SINGLE char."
  :type 'string
  :group 'smart-mode-line-others
  :package-version '(smart-mode-line . "1.16"))

(defcustom sml/show-trailing-N t
  "Whether the \"<N>\" suffix in buffer names should be displayed in the mode-line."
  :type 'boolean
  :group 'smart-mode-line-path&prefix)
(put 'sml/show-trailing-N 'safe-local-variable 'booleanp)

(defcustom sml/show-file-name t
  "Unless nil: show file name instead of buffer name on the mode-line."
  :type 'boolean
  :group 'smart-mode-line-path&prefix)
(put 'sml/show-file-name 'safe-local-variable 'booleanp)

(defcustom sml/fill-char ?\ 
  "The char to be used for filling."
  :type 'char
  :group 'smart-mode-line-path&prefix)

(defcustom sml/replacer-regexp-list
  `((,(concat "^" (if (boundp 'org-directory) (regexp-quote org-directory) "~/org/")) ":Org:")
    ("^~/\\.emacs\\.d/" ":ED:")
    ("^/sudo:.*:" ":SU:")
    ("^~/Documents/" ":Doc:")
    ("^~/Dropbox/" ":DB:")
    ("^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
    ("^~/[Gg]it/" ":Git:")
    ("^~/[Gg]it[Hh]ub/" ":Git:")
    ("^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:"))
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
(put 'sml/shorten-directory 'safe-local-variable 'booleanp)

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
(put 'sml/shorten-modes 'safe-local-variable 'booleanp)

(defcustom sml/battery-format " %p"
  "Format used to display the battery in the mode-line.

Only relevant if using `display-battery-mode'. See that function
for the syntax."
  :type 'string
  :group 'smart-mode-line-others)

(defcustom sml/modified-time-string "Modified on %T %Y-%m-%d."
  "String format used for displaying the modified time.

This is shown in the tooltip when hovering over the \"modified
file\" character (which is usually a * right before the file
name."
  :type 'string
  :group 'smart-mode-line-others)

(defconst sml/major-help-echo
  "Mouse-1: Mode Menu.\nMouse-2: Mode Help.\nMouse-3: Toggle Minor Modes.")

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
(defcustom sml/active-foreground-color "gray60" "Foreground mode-line color for the active frame."
  :type 'color :group 'smart-mode-line-faces :set 'sml/set-face-color :initialize 'custom-initialize-default)
(defcustom sml/active-background-color "black" "Background mode-line color for the active frame."
  :type 'color :group 'smart-mode-line-faces :set 'sml/set-face-color :initialize 'custom-initialize-default)
(defcustom sml/inactive-foreground-color "gray60" "Foreground mode-line color for the inactive frame."
  :type 'color :group 'smart-mode-line-faces :set 'sml/set-face-color :initialize 'custom-initialize-default)
(defcustom sml/inactive-background-color "#404045" "Background mode-line color for the inactive frame."
  :type 'color :group 'smart-mode-line-faces :set 'sml/set-face-color :initialize 'custom-initialize-default)

;; Face definitions
(defface sml/global           '((t :foreground "gray50" :inverse-video nil))
  "" :group 'smart-mode-line-faces)
(defface sml/modes            '((t :inherit sml/global :foreground "White"))
  "" :group 'smart-mode-line-faces)
(defface sml/filename         '((t :inherit sml/global :foreground "#eab700" :weight bold))
  "" :group 'smart-mode-line-faces)
(defface sml/prefix           '((t :inherit sml/global :foreground "#bf6000"))
  "" :group 'smart-mode-line-faces)
(defface sml/read-only        '((t :inherit sml/global :foreground "DeepSkyBlue"))
  "" :group 'smart-mode-line-faces)
(defface sml/outside-modified '((t :inherit sml/global :foreground "#ffffff" :background "#c82829"))
  "" :group 'smart-mode-line-faces)
(defface sml/modified         '((t :inherit sml/global :foreground "Red" :weight bold))
  "" :group 'smart-mode-line-faces)

(defface sml/line-number         '((t :inherit sml/modes :weight bold))               "" :group 'smart-mode-line-faces)
(defface sml/position-percentage '((t :inherit sml/prefix :weight normal))            "" :group 'smart-mode-line-faces)
(defface sml/col-number          '((t :inherit sml/global))                           "" :group 'smart-mode-line-faces)
(defface sml/numbers-separator   '((t :inherit sml/col-number))                       "" :group 'smart-mode-line-faces)
(defface sml/client              '((t :inherit sml/prefix))                           "" :group 'smart-mode-line-faces)
(defface sml/not-modified        '((t :inherit sml/global))                           "" :group 'smart-mode-line-faces)
(defface sml/mule-info           '((t :inherit sml/global))                           "" :group 'smart-mode-line-faces)
(defface sml/sudo                '((t :inherit sml/outside-modified))                 "" :group 'smart-mode-line-faces)
(defface sml/git                 '((t :inherit sml/read-only))                        "" :group 'smart-mode-line-faces)
(defface sml/folder              '((t :inherit sml/global :weight normal))            "" :group 'smart-mode-line-faces)
(defface sml/process             '((t :inherit sml/prefix))                           "" :group 'smart-mode-line-faces)
(defface sml/vc                  '((t :inherit sml/git))                              "" :group 'smart-mode-line-faces)
(defface sml/vc-edited           '((t :inherit sml/prefix))                           "" :group 'smart-mode-line-faces)
(defface sml/charging            '((t :inherit sml/global :foreground "ForestGreen")) "" :group 'smart-mode-line-faces)
(defface sml/discharging         '((t :inherit sml/global :foreground "Red"))         "" :group 'smart-mode-line-faces)
(defface sml/time                '((t :inherit sml/modes))                            "" :group 'smart-mode-line-faces)

;;; For changing between themes.
(defconst sml/mode-line-active-foreground-original (internal-get-lisp-face-attribute 'mode-line :foreground))
(defconst sml/mode-line-active-background-original (internal-get-lisp-face-attribute 'mode-line :background))
(defconst sml/mode-line-inactive-foreground-original (internal-get-lisp-face-attribute 'mode-line-inactive :foreground))
(defconst sml/mode-line-inactive-background-original (internal-get-lisp-face-attribute 'mode-line-inactive :background))

(deftheme smart-mode-line)
(defun sml/set-mode-line-buffer-id-face ()
  "Re-apply our face to major-modes that change mode-line-buffer-id."
  ;; Our buffer-file-name display.
  ;; For buffers which edit mode-line-identification, make sure they use OUR color.
  (ignore-errors
    (set-face-attribute
     'mode-line-buffer-id nil
     :foreground (internal-get-lisp-face-attribute 'sml/filename :foreground)
     :background (internal-get-lisp-face-attribute 'sml/filename :background)
     :weight     (internal-get-lisp-face-attribute 'sml/filename :weight)
     :underline  (internal-get-lisp-face-attribute 'sml/filename :underline)
     :overline   (internal-get-lisp-face-attribute 'sml/filename :overline))))

(defun sml/apply-theme (theme &optional value)
  "Apply THEME.

THEME can be one of the symbols: respectful, dark, or light.

This also sets the `sml/theme' variable, see its documentation
for more information on each value.

The second argument (VALUE) is for internal use only, don't use it."
  (message "[sml] %s set to %s" 'sml/theme (or value theme))
  (unless (eq sml/theme (or value theme))  
    (if value (setq-default sml/theme value)
      (if theme
          (setq-default sml/theme theme)
        (setq-default sml/theme 'respectful)))
    (case sml/theme
      ('respectful (custom-theme-set-variables
                    'smart-mode-line
                    `(sml/active-foreground-color ,sml/mode-line-active-foreground-original)
                    `(sml/active-background-color ,sml/mode-line-active-background-original)
                    `(sml/inactive-foreground-color ,sml/mode-line-inactive-foreground-original)
                    `(sml/inactive-background-color ,sml/mode-line-inactive-background-original))
                   (custom-theme-set-faces
                    'smart-mode-line
                    '(sml/global    ((t :inherit font-lock-preprocessor-face)))
                    `(sml/filename  ((t :inherit (font-lock-function-name-face sml/global) :weight bold
                                        :foreground ,(internal-get-lisp-face-attribute 'default :foreground))))
                    '(sml/prefix    ((t :inherit (font-lock-variable-name-face sml/global))))
                    '(sml/read-only ((t :inherit (font-lock-type-face sml/global))))
                    `(sml/modes     ((t :inherit sml/global :foreground ,sml/active-foreground-color)))))
      ('light (custom-theme-set-variables
               'smart-mode-line
               '(sml/active-foreground-color "black")
               '(sml/active-background-color "grey85")
               '(sml/inactive-foreground-color "grey20")
               '(sml/inactive-background-color "#fdf6e3"))
              (custom-theme-set-faces
               'smart-mode-line
               '(sml/global    ((t :foreground "gray20" :inverse-video nil)))
               '(sml/modes     ((t :inherit sml/global :foreground "Black")))
               '(sml/filename  ((t :inherit sml/global :foreground "Blue" :weight bold)))
               '(sml/prefix    ((t :inherit sml/global :foreground "#5b2507" :weight bold)))
               '(sml/read-only ((t :inherit sml/global :foreground "DarkGreen" :weight bold)))))
      ((dark t)
       (custom-theme-set-variables
        'smart-mode-line
        '(sml/active-foreground-color "gray60")
        '(sml/active-background-color "black")
        '(sml/inactive-foreground-color "gray60")
        '(sml/inactive-background-color "#404045"))
       (custom-theme-set-faces
        'smart-mode-line
        '(sml/global    ((t :foreground "gray50" :inverse-video nil)))
        '(sml/modes     ((t :inherit sml/global :foreground "White")))
        '(sml/filename  ((t :inherit sml/global :foreground "#eab700" :weight bold)))
        '(sml/prefix    ((t :inherit sml/global :foreground "#bf6000")))
        '(sml/read-only ((t :inherit sml/global :foreground "DeepSkyBlue"))))
       (if (eq sml/theme t)
           (message "[WARNING] smart-mode-line: setting `sml/override-theme' to t is obsolete.
Use the `sml/theme' variable instead."))))
    
    (sml/set-mode-line-buffer-id-face)
    (enable-theme 'user)))

(defvaralias 'sml/show-encoding 'sml/mule-info)

(defcustom sml/show-eol nil
  "Whether to display the buffer EOL in the mode-line."
  :type 'boolean
  :group 'smart-mode-line-others)
(put 'sml/show-eol 'safe-local-variable 'booleanp)

(defcustom sml/outside-modified-char "M"
  "Char to display if buffer needs to be reverted."
  :type 'string
  :group 'smart-mode-line-others
  :package-version '(smart-mode-line . "1.20"))

(defvaralias 'sml/encoding-format 'sml/mule-info)
(defcustom sml/mule-info "%z"
  "Format for multilingual information. Set this to nil to hide buffer encoding."
  :type '(choice string (const :tag "Don't display." nil))
  :group 'smart-mode-line-others
  :package-version '(smart-mode-line . "2.0"))

(defcustom sml/read-only-char "R"
  "Displayed when buffer is readonly."
  :type 'string
  :group 'smart-mode-line-others
  :package-version '(smart-mode-line . "1.20"))

(defcustom sml/show-frame-identification nil
  "Whether to show frame identification or not.

In some systems this doesn't even display anything. It's most useful
on terminals, but you might want to disable it anyway.

Just set this to nil, and frame identification won't be displayed."
  :type 'boolean
  :group 'smart-mode-line-others
  :package-version '(smart-mode-line . "2.0.3"))
(put 'sml/show-frame-identification 'safe-local-variable 'booleanp)

(defcustom sml/vc-mode-show-backend nil
  "Whether to show or not the backend in vc-mode's mode-line description.

I think most people only use one backend, so this defaults to nil.
If you want it to show the backend, just set it to t."
  :type 'boolean
  :group 'smart-mode-line-others
  :package-version '(smart-mode-line . "1.22"))
(put 'sml/vc-mode-show-backend 'safe-local-variable 'booleanp)


(defvar sml/position-construct nil "Used for recycling position information.")
(put 'sml/position-construct 'risky-local-variable t)

(defvar sml/position-help-text nil "Help-text for position information.")
(make-variable-buffer-local 'sml/position-help-text)

(defvar sml/buffer-identification-filling nil
  "Filling generated by `sml/fill-for-buffer-identification'.")
(make-variable-buffer-local 'sml/buffer-identification-filling)
(put 'sml/buffer-identification-filling 'risky-local-variable t)
(defvar sml/buffer-identification nil
  "Used for recycling buffer identification without having to recompute it.")
(make-variable-buffer-local 'sml/buffer-identification)
(put 'sml/buffer-identification 'risky-local-variable t)
(defadvice rename-buffer (after sml/after-rename-buffer-advice ())
  "Regenerate buffer-identification after rename-buffer."
  (sml/generate-buffer-identification))
(defadvice set-visited-file-name (after sml/after-set-visited-file-name-advice ())
  "Regenerate buffer-identification after set-visited-file-name."
  (sml/generate-buffer-identification))
(defadvice set-buffer-modified-p (after sml/after-set-buffer-modified-p-advice ())
  "Regenerate buffer-identification after set-buffer-modified-p."
  (sml/generate-buffer-identification))

(defvar sml/mode-line-client
  `(sml/show-client
    (:eval (if (frame-parameter nil 'client)
               ,(propertize "@" 'face 'sml/client 'help-echo (purecopy "emacsclient frame"))
             " ")))
  "Construct that replaces `mode-line-client'.")

;;;###autoload
(defun sml/setup (&optional arg)
  "Setup the mode-line to be smart and sexy.

Argument is ignored. Just call this function in your init file,
and it will be evaluated after emacs finished initializing (we do
this to make sure that we are loaded after any themes)."
  (interactive)
  ;; Just a couple of useful variables
  (setq sml/simplified nil)
  (setq battery-mode-line-format sml/battery-format)

  ;; Set the theme the user requested.
  (sml/apply-theme sml/theme)

  ;; Make sure the user's theme doesn't override the main faces
  ;; (mode-line and mode-line-inactive)
  (if after-init-time
      (sml/set-face-color)
    (add-hook 'after-init-hook 'sml/set-face-color))
  
  ;;;; And this is where the magic happens.
  ;; Remove elements we implement separately, and improve the ones not removed.
  (sml/filter-mode-line-list 'mode-line-mule-info)
  (setq-default mode-line-client sml/mode-line-client)
  (sml/filter-mode-line-list 'mode-line-modified)
  (sml/filter-mode-line-list 'mode-line-remote)
  (setq-default mode-line-frame-identification
                '(sml/show-frame-identification "%F"))

  ;; (setq-default mode-line-buffer-identification '("%b"))
  
  (setq-default mode-line-buffer-identification
                '(sml/buffer-identification
                  sml/buffer-identification
                  (:eval (sml/generate-buffer-identification))))
  (sml/filter-mode-line-list 'mode-line-position)
  (sml/filter-mode-line-list 'mode-line-modes)
  (setq-default mode-line-end-spaces nil)

  ;; Add position descriptions on the left (they were already removed from the middle)
  
  (setq-default mode-line-front-space '((sml/position-help-text
                                         nil
                                         (:eval (sml/generate-position-help)))
                                        (sml/position-construct
                                         sml/position-construct
                                         (:eval (sml/compile-position-construct)))))

  (add-hook 'after-save-hook 'sml/generate-buffer-identification)
  (ad-activate 'rename-buffer)
  (ad-activate 'set-visited-file-name)
  (ad-activate 'set-buffer-modified-p)
  (add-hook 'after-change-functions 'sml/generate-position-help)

  ;; This is to ensure fixed name width. The reason we do this manually
  ;; is that some major-modes change `mode-line-buffer-identification'
  ;; (so we can't fill inside the variable), and we want this
  ;; symbol to be an element in `mode-line-format' for compatibility
  ;; with other packages which hack into the mode-line.
  
  (add-to-list 'mode-line-position
               '(sml/buffer-identification-filling
                 sml/buffer-identification-filling
                 (:eval (sml/generate-buffer-identification))))

  ;; Remove some annoying big spaces
  
  (setq-default mode-line-format
                (mapcar
                 (lambda (x) (if (and (stringp x) (string-match "\\` +\\'" x))
                                 " " x))
                 mode-line-format))

      ;;;; And here comes support for a bunch of extra stuff. Some of
      ;;;; these are just needed for coloring.

  ;; Display time
  
  (add-hook 'display-time-hook 'sml/propertize-time-string)

  ;; ;; Small thing to help powerline support
  ;; (when (fboundp 'powerline-default-theme)
  ;;   (when (eq sml/mode-width 'full) (setq sml/mode-width 0))
  ;;   (when (= sml/name-width 44)
  ;;     (setq sml/mode-width 0)
  ;;     (setq sml/shorten-directory nil)))
  
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
                                         ((string-match "^ [!\\?]" noback) 'sml/modified))))))))

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

  (unless (and (boundp 'erc-track-position-in-mode-line)
               (null erc-track-position-in-mode-line))
    (setq erc-track-position-in-mode-line t)))

(defun sml/generate-position-help (&rest ignored)
  "Set the string describing various buffer content information."
  (when (get-buffer-window (current-buffer))
    (setq sml/position-help-text
          (format-mode-line
           (concat "Buffer size:\n\t%IB\n"
                   "Number of Lines:\n\t"
                   (int-to-string (line-number-at-pos (point-max)))
                   "\nmouse-1: Display Line and Column Mode Menu")))
    nil))

(defun sml/compile-position-construct (&optional symbol value)
  "Recompile the `sml/position-construct' after one of the formats was edited."
  (when (and symbol value) (set symbol value))
  (sml/generate-position-help)
  (setq sml/position-construct
        `((size-indication-mode
           ,(propertize sml/size-indication-format
                        'face 'sml/col-number
                        'help-echo 'sml/position-help-text
                        'mouse-face 'mode-line-highlight
                        'local-map mode-line-column-line-number-mode-map))
          (column-number-mode
           ,(propertize sml/col-number-format
                        'face 'sml/col-number
                        'help-echo 'sml/position-help-text
                        'mouse-face 'mode-line-highlight
                        'local-map mode-line-column-line-number-mode-map))
          (column-number-mode
           (line-number-mode
            ,(propertize sml/numbers-separator
                         'face 'sml/numbers-separator
                         'help-echo 'sml/position-help-text
                         'mouse-face 'mode-line-highlight
                         'local-map mode-line-column-line-number-mode-map)))
          (line-number-mode
           ,(propertize sml/line-number-format
                        'face 'sml/line-number
                        'help-echo 'sml/position-help-text
                        'mouse-face 'mode-line-highlight
                        'local-map mode-line-column-line-number-mode-map)))))

(defun sml/generate-modified-status ()
  "Return a string describing the modified status of the buffer."
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

(defmacro sml/propertize-position (s face help)
  "Propertize string S as a line/column number, using FACE and help-echo HELP."
  `(propertize ,s
               'face ,face
               'help-echo ,help
               'mouse-face 'mode-line-highlight
               'local-map mode-line-column-line-number-mode-map))

(defun sml/propertize-time-string ()
  "Function to be added to `display-time-hook' to propertize the string."
  (when (and (boundp 'display-time-string) (stringp display-time-string))
    (setq display-time-string
          (propertize display-time-string
                      'face 'sml/time))))

(defun sml/filter-mode-line-list (l)
  "Filter some elements of L and propertize the ones not filtered.

L must be a symbol! We asign right back to it"
  (if (and (symbolp l) (listp (eval l)))
      (set-default l
       (remove nil (mapcar 'sml/parse-mode-line-elements (eval l))))
    (error "l must be a symbol to a list!")))

(defun sml/fill-for-buffer-identification ()
  "Returns a string of spaces so that `mode-line-buffer-identification' is fixed-width."
  (make-string (max (- sml/name-width (length (format-mode-line mode-line-buffer-identification)))
                    0) sml/fill-char))

(defun sml/generate-buffer-identification ()
  "Return fully propertized prefix+path+buffername."
  (if (equal 'sml/buffer-identification
             (car-safe mode-line-buffer-identification))
      (setq sml/buffer-identification
            (let* ((prefix (sml/get-prefix (sml/replacer (sml/get-directory))))
                   (bufname (sml/buffer-name))
                   (dirsize (max 0 (- (abs sml/name-width) (length prefix) (length bufname))))
                   (dirstring (funcall sml/shortener-func (sml/get-directory) dirsize)))

              (propertize (concat (sml/propertize-prefix (replace-regexp-in-string "%" "%%" prefix))
                                  (propertize (replace-regexp-in-string "%" "%%" dirstring) 'face 'sml/folder)
                                  (propertize (replace-regexp-in-string "%" "%%" bufname) 'face 'sml/filename)
                                  (make-string (max 0 (- dirsize (length dirstring))) ?\ ))
                          'help-echo (format "%s\n\nmouse-1: Previous buffer\nmouse-3: Next buffer"
                                             (or (buffer-file-name) (buffer-name)))
                          'mouse-face 'mode-line-highlight
                          'local-map   mode-line-buffer-identification-keymap))
            sml/buffer-identification-filling "")
    (setq sml/buffer-identification-filling (sml/fill-for-buffer-identification)))
  nil)

(defun sml/parse-mode-line-elements (el)
  "Propertize or delete EL.

To be used in mapcar and accumulate results."
  (cond
   ;; These are implemented separately
   ((member el '("%1+" "(" ")" "%1@" (t erc-modified-channels-object)
                 (:eval (if (display-graphic-p) " " "-"))
                 (:eval (unless (display-graphic-p) "-%-"))
                 (:eval (mode-line-frame-control))))
    nil)
   ((member (car-safe el) '(line-number-mode column-number-mode size-indication-mode current-input-method)) nil)

   ;; mode-line-client
   ((equal el '("" (:propertize ("" (:eval (if (frame-parameter nil 'client) "@" "")))
                                help-echo "emacsclient frame")))
    `(sml/show-client
      (:eval (if (frame-parameter nil 'client)
                 ,(propertize "@" 'face 'sml/client 'help-echo (purecopy "emacsclient frame"))
               " "))))

   ;; mode-line-modified
   ((and (stringp el) (string-match "%[0-9-]*\\*" el))
    '(:eval (sml/generate-modified-status)))

   ;;;; mode-line-position
   ;; Color the position percentage
   ((sml/is-%p-p el)
    `(sml/position-percentage-format
      (-3 (:propertize (:eval sml/position-percentage-format)
                       local-map ,mode-line-column-line-number-mode-map
                       mouse-face mode-line-highlight
                       face sml/position-percentage
                       help-echo "Buffer Relative Position\n\
mouse-1: Display Line and Column Mode Menu"))))

   ;;;; mode-line-mule-info
   ;; Partially hide some MULE info
   ((and (stringp el) (string-match "\\s-*%[-0-9]*z" el))
    `(sml/mule-info ((1 (current-input-method
                         (:propertize ("" current-input-method-title)
                                      help-echo (concat
                                                 ,(purecopy "Current input method: ")
                                                 current-input-method
                                                 ,(purecopy "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method"))
                                      local-map ,mode-line-input-method-map
                                      mouse-face mode-line-highlight)))
                     (:propertize (:eval sml/mule-info)
                                  help-echo mode-line-mule-info-help-echo
                                  mouse-face mode-line-highlight
                                  local-map ,mode-line-coding-system-map))))
   ;; Make EOL optional
   ((equal el '(:eval (mode-line-eol-desc)))
    '(sml/show-eol (:eval (mode-line-eol-desc))))

   ;;;; mode-line-mods
   ;; Color the mode line process
   ((or (equal el '("" mode-line-process))
        (equal (car (cdr-safe el)) '("" mode-line-process)))
    `(:propertize ("" mode-line-process) face sml/process))
   ;; Color the mode name, without changing other properties
   ((and (listp el)
         (equal (car el) :propertize)
         (equal (cadr el) '("" mode-name)))
    (append el '(face sml/modes)))
   ;; Completely replace the minor modes (so we can truncate)
   ((and (listp el)
         (equal (car el) :propertize)
         (equal (cadr el) '("" minor-mode-alist)))
    '(:eval (sml/generate-minor-modes)))
   ;; If it's something we don't recognize, just leave it as-is.
   (t el)))

(defun sml/is-%p-p (x)
  "Non-nil if X matches \"%p\" in a very subjective sense."
  (or (and (listp x)
           (-first (lambda (y) (string-match ".*%p.*" y))
                   (-filter 'stringp x)))
      (and (stringp x)
           (string-match ".*%p.*" x))))

(defun sml/buffer-name ()
  "Uses `sml/show-file-name' to decide between buffer name or file name to show on the mode-line.

Unless `sml/strip-N' is nil, prevents the \"<N>\" (used in
duplicated buffer names) from being displayed."
  (if (and sml/show-file-name (buffer-file-name))
      (file-name-nondirectory (buffer-file-name))
    (if sml/show-trailing-N
        (buffer-name)
      (replace-regexp-in-string "<[0-9]+>$" "" (buffer-name)))))

(defun sml/fill-width-available ()
  "Return the size available for filling."
  (max 0
       (+ sml/extra-filler
          (- (window-width)
             (let ((sml/simplified t))
               (length (format-mode-line mode-line-format)))))))

(defun sml/mode-list-to-string-list (ml) ;;Credits to Constantin
  "Try to read the mode-list (which contains almost anything) and return a sensible list of strings."
  (case (type-of ml)
    ('string (list ml))
    ('symbol
     (if ml
         (sml/mode-list-to-string-list (symbol-value ml))
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
              (:eval (sml/mode-list-to-string-list (eval kadr)))
              ;; properties now handled properly
              (:propertize `((:propertize ,@(sml/mode-list-to-string-list kadr) ,@(cdr-safe kdr))))
              (t
               (if val
                   (sml/mode-list-to-string-list kadr)
                 (sml/mode-list-to-string-list (cdr kdr)))))))
         ('integer
          ;; heh, now do nothing, must reduce max width if < 0 or do padding if > 0
          (sml/mode-list-to-string-list kdr))
         (t (append (sml/mode-list-to-string-list kar) (sml/mode-list-to-string-list kdr))))))
    ;; unknown
    (t ;;(message "mode-list-to-string-error Unknown: type: %s;\nval: %s" ml (type-of ml))
     (list (format "%s" ml)))))

(defconst sml/propertized-shorten-mode-string
  '(:propertize sml/shorten-mode-string
              help-echo "mouse-1: Shorten minor modes"
              local-map (keymap (mode-line keymap (mouse-1 . sml/toggle-shorten-modes)))
              mouse-face mode-line-highlight))
(defconst sml/propertized-full-mode-string
  '(:propertize sml/full-mode-string
              help-echo "mouse-1: Show all modes"
              local-map (keymap (mode-line keymap (mouse-1 . sml/toggle-shorten-modes)))
              mouse-face mode-line-highlight))

(defun sml/count-occurrences-starting-at (regex string start)
  "Count occurrences of REGEX in STRING starting at index START."
  (if (string-match regex string start)
      (+ 1 (sml/count-occurrences-starting-at regex string (match-end 0)))
    0))

(defun sml/generate-minor-modes ()
  "Extracts all rich strings necessary for the minor mode list."
  (if sml/simplified
      ""
    (let* ((nameList (sml/mode-list-to-string-list minor-mode-alist))
           (finalNameList (mapconcat 'format-mode-line  nameList ""))
           (size (if (equal sml/mode-width 'full) (sml/fill-width-available) sml/mode-width))
           (helpString (concat "Full list:" (replace-regexp-in-string " " "\n    " finalNameList)
                               "\n\n" sml/major-help-echo))
           needs-removing)
      ;; Remove hidden-modes
      (setq nameList
            (remove
             nil
             (mapcar (lambda (x) (unless (and (stringp x) (member x sml/hidden-modes)) x))
                     nameList)))
      ;; Truncate
      (setq finalNameList (mapconcat 'format-mode-line  nameList ""))
      (when (and sml/shorten-modes (> (length finalNameList) size))
        ;; We need to remove 1+"the number of spaces found". We use
        ;; 2+ because the car of the list element returned by `last'
        ;; (below) won't actually be removed.
        (setq needs-removing
              (+ 2 (sml/count-occurrences-starting-at
                    " " finalNameList (- size (length sml/full-mode-string))))))
      ;; Add truncation string if necessary
      (when needs-removing
        (setcdr (last nameList needs-removing)
                (list t sml/propertized-full-mode-string)))
      ;; If we're not shortenning, add " -" at the end.
      (unless sml/shorten-modes
        (add-to-list 'nameList sml/propertized-shorten-mode-string t))

      (list size ;; Padding
            (list ':propertize nameList
                  'help-echo helpString
                  'mouse-face 'mode-line-highlight
                  'face 'sml/folder
                  'local-map mode-line-minor-mode-keymap)))))

(defun sml/propertize-prefix (prefix)
  "Set the color of the prefix according to its contents."
  (let ((out prefix))
    (dolist (pair sml/prefix-face-list)
      (if (string-match (car pair) prefix)
          (return (propertize prefix 'face (car (cdr pair))))))))

(defun sml/get-directory ()
  "Decide if we want directory shown. If so, return it."
  (abbreviate-file-name
   (cond
    ;; In email attachments, buffer-file-name is non-nil, but
    ;; file-name-directory returns nil
    ((buffer-file-name) (or (file-name-directory (buffer-file-name)) ""))
    ((and (listp mode-name) (stringp (car mode-name))
          (string-match "Dired" (car mode-name)))
     (replace-regexp-in-string "/[^/]*/$" "/" default-directory))
    (t ""))))

(defun sml/set-battery-font ()
  "Set `sml/battery' face depending on battery state."
  (let ((data (and (boundp 'battery-status-function)
                   battery-status-function
                   (funcall battery-status-function))))
    (if  (string-equal "AC" (cdr (assoc 76 data)))
        (copy-face 'sml/charging 'sml/battery)
      (copy-face 'sml/discharging 'sml/battery))))

(defadvice battery-update (before sml/set-battery-font activate)
  (sml/set-battery-font))

(defun sml/replacer (in)
  "Runs the replacements specified in `sml/replacer-regexp-list'.

Used by `sml/strip-prefix' and `sml/get-prefix'."
  (let ((out in))
    (dolist (cur sml/replacer-regexp-list)
      (setq out (replace-regexp-in-string
                 (car cur) (car (cdr cur)) out)))
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

(defun sml/set-face-color (&optional sym val)
  (if sym (set-default sym val))
  (set-face-attribute 'mode-line nil
                      :inverse-video nil
                      :foreground sml/active-foreground-color
                      :background sml/active-background-color)
  (set-face-attribute 'mode-line-inactive nil
                      :inverse-video nil
                      :background sml/inactive-background-color
                      :foreground sml/inactive-foreground-color)
  (sml/set-mode-line-buffer-id-face))

;; Backup the original configs, just in case.
(defconst sml/format-backup mode-line-format
  "Backs up the `mode-line-format' before SML was required.")
(defconst sml/battery-format-backup (if (boundp 'battery-mode-line-format) battery-mode-line-format "")
  "Backs up the `battery-mode-line-format' before SML was required.")
(copy-face 'mode-line 'sml/active-backup)
(copy-face 'mode-line-inactive 'sml/inactive-backup)

(provide 'smart-mode-line)
;;; smart-mode-line.el ends here
