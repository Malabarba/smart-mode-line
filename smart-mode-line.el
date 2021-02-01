;;; smart-mode-line.el --- A color coded smart mode-line.

;; Copyright (C) 2012 Artur Malabarba <emacs@endlessparentheses.com>

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; URL: http://github.com/Malabarba/smart-mode-line
;; Version: 2.13
;; Package-Requires: ((emacs "24.3") (rich-minority "0.1.1"))
;; Keywords: mode-line faces themes
;; Prefix: sml
;; Separator: /

;;; Commentary:
;;
;; Smart Mode Line is a sexy mode-line for Emacs. It aims to be easy to
;; read from small to large monitors by using *colors*, a *prefix feature*, and
;; *smart truncation*.
;;
;; New in v2.5
;; ===========
;; - Emacs 24.4 compatible.
;; - Integration with [Projectile](https://github.com/bbatsov/projectile)!
;; - Display `current-directory' in Shell and eshell.
;; - New value for `sml/theme': `automatic' (highly recommended).
;; - `sml/apply-theme' is interactive and has completion.
;; - Smart-mode-line themes are now regular themes.
;;
;; Installation
;; ===
;; **smart-mode-line** is available on Melpa, and that's the recommended
;; way of installing it. If you do that, you can simply activate it with:
;;
;;     (sml/setup)
;;
;; To set the color theme, do one of the following BEFORE `sml/setup`:
;;
;;     (setq sml/theme 'dark)
;;     (setq sml/theme 'light)
;;     (setq sml/theme 'respectful)
;;
;; Features
;; ===
;; Its main features include:
;;
;;  1. **Color coded**:
;;     Highlights the most important information for you
;;     (buffer name, modified state, line number). Don't
;;     like the colors? See item *5.*!
;;
;;  2. **Fixed width** (if you want):
;;     Lets you set a maxium width for the path name and mode names, and
;;     truncates them intelligently (truncates the directory, not the
;;     buffer name). Also let's you **right indent** strings in the
;;     mode-line (see `sml/mode-width').
;;
;;  3. **Directory as Prefixes**:
;;     Prefix feature saves a LOT of space. e.g. *"~/.emacs.d/"*
;;     is translated to *":ED:"* in the path (open a file inside
;;     this folder to see it in action). Long path names you
;;     are commonly working on are displayed as short
;;     abbreviations. Set your own prefixes to make best use
;;     of it (by configuring `sml/replacer-regexp-list'). Mousing
;;     over the abbreviated path will show you the full
;;     path. See below for examples.
;;
;;  4. **Hide or Highlight minor-modes**:
;;     The [rich-minority](https://github.com/Malabarba/rich-minority)
;;     package saves even more space. Select which minor modes you don't
;;     want to see listed by adding them to the variable
;;     `rm-excluded-modes', or even highlight the modes that are more
;;     important with the variable `rm-text-properties'. This will filter
;;     out the modes you don't care about and unclutter the modes list
;;     (mousing over the modes list still shows the full list).
;;
;;  4. **Hide minor-modes**:
;;     Hidden-modes feature saves even more space. Select
;;     which minor modes you don't want to see listed by
;;     customizing the `rm-blacklist' variable. This will
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
;;  1. `sml/theme'
;;   Choose what theme you want to use for the mode-line colors. For now
;;   there are 3 different themes: `dark', `light', and `respectful'.
;;
;;  1. `sml/shorten-directory' and `sml/shorten-modes'
;;   Setting both of these to `t' guarantees a fixed width mode-line
;;   (directory name and minor-modes list will be truncated to fit). To
;;   actually define the width, see below.
;;
;;  2. `sml/name-width' and `sml/mode-width'
;;   Customize these according to the width of your Emacs frame. I set
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
;;   the form *":SOMETHING:"*, it is considered a prefix and get's
;;   a different color (you can change what's considered a prefix
;;   by customizing `sml/prefix-regexp').
;;   For example, if you do a lot of work on a folder called
;;   *"~/Dropbox/Projects/In-Development/"* almost half the
;;   mode-line would be occupied just by the folder name, which
;;   is much less important than the buffer name. But, you can't
;;   just hide the folder name, since editing a file in
;;   *"~/Dropbox/Projects/In-Development/Source"* is VERY different
;;   from editting a file in *"~/Dropbox/Projects/Source"*. By
;;   setting up a prefix for your commonly used folders, you get
;;   all that information without wasting all that space. In this
;;   example you could set the replacement to *":ProjDev:"* or just
;;   *":InDev:"*, so the path shown in the mode-line will be
;;   *":ProjDev:Source/"* (saves a lot of space without hiding
;;   information).
;;
;; Here go some more useful examples:
;;
;;     (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/Projects/In-Development/" ":ProjDev:") t)
;;     (add-to-list 'sml/replacer-regexp-list '("^~/Documents/Work/" ":Work:") t)
;;
;;     ;; Added in the right order, they even work sequentially:
;;     (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:") t)
;;     (add-to-list 'sml/replacer-regexp-list '("^:DB:Documents" ":DDocs:") t)
;;     (add-to-list 'sml/replacer-regexp-list '("^~/Git-Projects/" ":Git:") t)
;;     (add-to-list 'sml/replacer-regexp-list '("^:Git:\\(.*\\)/src/main/java/" ":G/\\1/SMJ:") t)

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
;; 2.6     - 2014/08/15 - Allow for sml/name-width to have different mininum and maximum values.
;; 2.6     - 2014/08/15 - Delegated minor-mode filtering to rich-minority package.
;; 2.5.3   - 2014/06/18 - Fix custom-theme-load-path for manual installations.
;; 2.5.2   - 2014/06/16 - sml/no-confirm-load-theme variable to skip theme confirmation.
;; 2.5.1   - 2014/06/16 - sml/apply-theme no-confirm in daemon mode.
;; 2.5     - 2014/05/15 - sml/theme: New possible values: 'automatic (highly recommended) or nil.
;; 2.5     - 2014/05/14 - sml/mode-width: New possible value: 'right.
;; 2.5     - 2014/05/14 - Themes engine completely redone.
;; 2.5     - 2014/05/14 - sml/apply-theme is interactive.
;; 2.4.5   - 2014/04/24 - Changed default value of sml/mode-width back to 'full.
;; 2.4.3   - 2014/03/25 - sml/mode-line-buffer-identification fix for ggtags.
;; 2.4.2   - 2014/03/13 - Perspective support simplified to sml/apply-theme.
;; 2.4.2   - 2014/03/13 - Projectile integration only applies after the user replacements (to change, see sml/use-projectile-p).
;; 2.4.1   - 2014/03/11 - Small fix to dired-mode with uniquify.
;; 2.4     - 2014/03/10 - Projectile integration! To disable it, set sml/use-projectile-p.
;; 2.4     - 2014/03/10 - Change the order of line/column numbers with sml/order-of-line-and-column.
;; 2.4     - 2014/03/10 - Take over dired's buffer-identification. We will undo this if dired ever does anything special with this variable.
;; 2.4     - 2014/03/10 - Show current-directory in Shell and eshell.
;; 2.4     - 2014/03/10 - Tested against 24.4.
;; 2.4     - 2014/03/10 - Ditch total number of lines count.
;; 2.3.13  - 2014/03/05 - sml/apply-theme forces our foreground/background colors.
;; 2.3.12  - 2014/03/05 - Use sml/show-remote to hide/show the "@" symbol. .
;; 2.3.12  - 2014/03/05 - Support showing tramp state (remote buffer).
;; 2.3.12  - 2014/02/27 - sml/apply-theme avoids nesting.
;; 2.3.11  - 2014/02/15 - Silent sml/apply-theme.
;; 2.3.10  - 2014/02/15 - Fix sml/setup ignoring sml/theme.
;; 2.3.9   - 2014/02/10 - sml/hidden-modes allows regexps.
;; 2.3.8   - 2014/02/07 - Buffer identification width auto-updates when sml/name-width changes.
;; 2.3.8   - 2014/02/07 - sml/apply-theme customizes helm-candidate-number.
;; 2.3.7   - 2014/01/21 - Adapt sml/generate-buffer-identification.
;; 2.3.6   - 2013/12/16 - sml/replacer follows symlinks.
;; 2.3.6   - 2013/12/16 - Fix filling and name on the very first update of non-file buffers.
;; 2.3.5   - 2013/12/14 - sml/generate-position-help runs less often now.
;; 2.3.4   - 2013/12/14 - Remove lag-inducing advice.
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

(require 'cl-lib)
(require 'custom)
(require 'cus-face)
(require 'rich-minority)

(defconst sml/version "2.13" "Version of the smart-mode-line.el package.")
(defun sml/bug-report ()
  "Opens github issues page in a web browser. Please send me any bugs you find, and please include your Emacs and sml versions."
  (interactive)
  (browse-url "https://github.com/Malabarba/smart-mode-line/issues/new")
  (message "Your sml/version is: %s, and your emacs version is: %s.\nPlease include this in your report!" sml/version emacs-version))
(defun sml/customize ()
  "Open the customization group for the `smart-mode-line' package."
  (interactive)
  (customize-group 'smart-mode-line t))
(defun sml/customize-faces ()
  "Open the customization group for faces used by the `smart-mode-line' package."
  (interactive)
  (customize-group 'smart-mode-line-faces t))

(defgroup smart-mode-line '()
  "Customization group for the `smart-mode-line' package."
  :group 'convenience
  :prefix 'sml)
(defgroup smart-mode-line-position '()
  "Showing the point position in the smart mode line."
  :group 'smart-mode-line
  :prefix 'sml)
(defgroup smart-mode-line-path-and-prefix '()
  "Showing the path, buffer-name, and prefix in the smart mode line."
  :group 'smart-mode-line
  :prefix 'sml)
(defgroup smart-mode-line-mode-list '()
  "Showing major/minor modes in the smart mode line."
  :group 'smart-mode-line
  :prefix 'sml)
(defgroup smart-mode-line-others '()
  "Showing other data in the smart mode line."
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


;;; Actual Code
(defvar erc-track-position-in-mode-line)
(defvar sml/simplified nil
  "Temporary dynamic variable. Used for filling.")
(defvar sml/active-background-color)

(defvar sml/-debug nil
  "Whether debugging information should be printed.")

(defmacro sml/-debug (fmt &rest r)
  "If variable `sml/-debug' is non-nil, describe FMT.
If FMT is a string, this is essentially the same as `message'.
If FMT is anything else, this is essentially:
    (message \"%s is: %s\" 'FMT FMT)"
  (when (and (boundp 'sml/-debug) sml/-debug)
    (if (stringp fmt)
        `(apply #'message (concat "[sml/debug] " ,fmt) ,r)
      `(message "[sml/debug] %s is: %s" ',fmt ,fmt))))

(defvar sml/shortener-func 'sml/do-shorten-directory
  "Function used to shorten the directory name.

Value is a funcallable symbol that takes two arguments: the
string to be shortened and the maximum size. This is set
automatically when `sml/shorten-directory' is changed via the
customization menu or via the `sml/toggle-shorten-directory'
function (which are the only ways you should change it).")

(defun sml/set-shortener-func (sym val)
  "Configure `sml/shortener-func' according to `sml/shorten-directory'.
Set SYM to VAL."
  (set-default sym val)
  (if val (setq sml/shortener-func 'sml/do-shorten-directory)
    (setq sml/shortener-func 'sml/not-shorten-directory)))

(define-obsolete-variable-alias 'sml/time-format 'display-time-format "2.0.3")
(define-obsolete-variable-alias 'sml/show-time 'display-time-mode "2.0.3")
(define-obsolete-variable-alias 'sml/override-theme 'sml/theme "2.0.3")

(defcustom sml/theme 'automatic
  "Defines which theme `smart-mode-line' should use.

This is usually one of the symbols:
'automatic, 'respectful, 'dark, 'light or nil;
but it can be something else if there are other smart-mode-line
themes defined.

Setting this to 'light and 'dark will apply some predefined
colors to the mode-line, which are designed to be easy to read.

Setting this to nil will apply almost no colors. Use this if your
global color theme already customizes sml faces (flatui-theme is
an example).

Setting this to 'automatic will let sml decide between 'light or
'dark or nil, to best match the global theme that is active when
`sml/setup' is called.

Setting it to 'respectful will try to use the colors defined by
your current Emacs theme (emphasis on the \"try\"). Use this if
you color theme does NOT customize sml faces, AND if you're not
happy with 'light or 'dark.
This option will make the mode-line colors more consistent with
buffer colors (when compared to 'light or 'dark, which have fixed
colors) , but it's a bit of a shot in the dark. The result will
vary for each color theme, and you may get colors that don't read
well.

But don't forget, ALL COLORS ARE CUSTOMIZABLE!
`sml/customize-faces'
Any color you change manually won't get affected by this
variable.

Setting this variable via `setq' only has effect BEFORE calling
`sml/setup'. If smart-mode-line is already loaded, use
 `sml/apply-theme' instead (or the customization interface)."
  :type '(choice (const :tag "Automatically choose between 'light, 'dark, or nil during setup. (Default and Recommended)" automatic)
                 (const :tag "Don't use a theme." nil)
                 (const :tag "Use a dark color-theme." dark)
                 (const :tag "Use a light color-theme." light)
                 (const :tag "Respect the color-theme's colors." respectful)
                 (symbol :tag "Other smart-mode-line theme you installed."))
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

(defcustom sml/show-remote t
  "Whether to display an \"@\" for remote buffers.
If the buffer is local, an \"-\" is displayed instead.
If this variable is nil, nothing is displayed."
  :type 'boolean
  :group 'smart-mode-line-others)
(put 'sml/show-remote 'safe-local-variable 'booleanp)

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

(defcustom sml/not-modified-char " "
  "String that indicates if buffer is un-modified. Should be one SINGLE char."
  :type 'string
  :group 'smart-mode-line-others
  :package-version '(smart-mode-line . "1.16"))

(defcustom sml/show-trailing-N t
  "Whether the \"<N>\" suffix in buffer names should be displayed in the mode-line."
  :type 'boolean
  :group 'smart-mode-line-path-and-prefix)
(put 'sml/show-trailing-N 'safe-local-variable 'booleanp)

(defcustom sml/show-file-name t
  "Unless nil: show file name instead of buffer name on the mode-line."
  :type 'boolean
  :group 'smart-mode-line-path-and-prefix)
(put 'sml/show-file-name 'safe-local-variable 'booleanp)

(defcustom sml/fill-char ?\s
  "The char to be used for filling."
  :type 'char
  :group 'smart-mode-line-path-and-prefix)

(defcustom sml/replacer-regexp-list
  `((,(concat "^" (if (boundp 'org-directory) (regexp-quote org-directory) "~/org/")) ":Org:")
    ("^~/\\.emacs\\.d/elpa/" ":ELPA:")
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
usually want to add them to the back (thus the t at the end).

You can also set custom colors (faces) for these prefixes, just
set `sml/prefix-face-list' accordingly."
  :type '(repeat (list regexp string))
  :group 'smart-mode-line-path-and-prefix
  :package-version '(smart-mode-line . "1.22"))

(defcustom sml/prefix-regexp '(":\\(.*:\\)" "~/")
  "List of Regexps used to identify prefixes.

A prefix is anything at the beginning of a line that matches any
of these regexps. Don't start these regexps with \"^\", the
parser applies that for you."
  :type '(repeat regexp)
  :group 'smart-mode-line-path-and-prefix)

(defcustom sml/prefix-face-list '((":SU:" sml/sudo)
                                  (":G" sml/git)
                                  (sml/projectile-replacement-format sml/projectile)
                                  ("" sml/prefix))
  "List of (STRING FACE) pairs used by `sml/propertize-prefix'.

After the file path is constructed, the prefix contained in it is
colored according to this list. The elements are checked one by
one and, if the prefix contains the STRING part of the pair, then
FACE is applied to it (and checking stops there)."
  :type '(repeat (list string face))
  :group 'smart-mode-line-path-and-prefix)

(defcustom sml/name-width 44
  "Minimum and maximum size of the file name in the mode-line.

If `sml/shorten-directory' is nil, this is the minimum width.
Otherwise, this is both the minimum and maximum width.

Alternatively, you can set the minimum and maximum widths
separately, by setting this variable to a cons cell of integers:
    (MIN-WIDTH . MAX-WIDTH)
"
  :type '(choice integer (cons (integer :tag "Minimum width")
                               (integer :tag "Maximum width")))
  :group 'smart-mode-line-path-and-prefix)

(defvaralias 'sml/path-width 'sml/name-width)

(defcustom sml/shorten-directory t
  "Should directory name be shortened to fit width?

When the buffer+directory name is longer than
`sml/name-width':
    if nil the rest of the mode-line is pushed right;
    otherwise the directory name is shortened to fit."
  :type 'boolean
  :group 'smart-mode-line-path-and-prefix
  :set 'sml/set-shortener-func)
(put 'sml/shorten-directory 'safe-local-variable 'booleanp)

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

(defun sml/toggle-shorten-directory (&rest val)
  "Toggle the variable `sml/shorten-directory'.

If given an argument VAL, the variable is set to the argument,
otherwise it is toggled. This can be used as an alternative to
customizing the variable with `customize-group'. Setting the
variable with `setq' will NOT work and should be avoided."
  (interactive)
  (sml/set-shortener-func 'sml/shorten-directory
                          (if val (car-safe val)
                            (not sml/shorten-directory))))

(defun sml/toggle-shorten-modes (&rest val)
  "Toggle the variable `sml/shorten-modes'.

If given an argument VAL, the variable is set to the argument,
otherwise it is toggled. This can be used as an alternative to
customizing the variable with `customize-group'. Equivalent to
setting the variable with `setq'."
  (interactive)
  (setq sml/shorten-modes (if val (car val)
                            (not sml/shorten-modes)))
  (force-mode-line-update))

(defcustom sml/mode-width 'full
  "Maximum and/or minimum size of the modes list in the mode-line.

If it is an integer, then the modes list width is that many
characters.

If it is the symbol `full', then the mode-list fills all the
empty space is available in the mode-line (this has the effect of
indenting right anything after the mode-list).

If it is the symbol `right', then it behaves like `full', but the
minor-modes list is moved all the way to the right.

If `sml/shorten-modes' is nil, this is the minimum width.
Otherwise, this is both the minimum and maximum width."
  :type '(choice integer symbol)
  :group 'smart-mode-line-mode-list
  :package-version '(smart-mode-line . "2.4.5"))

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

(defcustom sml/extra-filler 0
  "The number of extra filling chars to use.
It comes into play when `sml/mode-width' is set to 'full.

This is necessary because the mode-line width (which we need but
don't have access to) is larger than `window-total-width' (which
we have access to).

Decrease this if right indentation seems to be going too far (or
if you just want to fine-tune it)."
  :type 'integer
  :group 'smart-mode-line-mode-list)

;; Face definitions
(defface sml/global           '((t :inverse-video nil)) "" :group 'smart-mode-line-faces)
(defface sml/modes            '((t :inherit sml/global)) "" :group 'smart-mode-line-faces)
(defface sml/minor-modes      '((t :inherit sml/global)) "" :group 'smart-mode-line-faces)
(defface sml/filename         '((t :inherit sml/global :weight bold)) "" :group 'smart-mode-line-faces)
(defface sml/prefix           '((t :inherit sml/global)) "" :group 'smart-mode-line-faces)
(defface sml/read-only        '((t :inherit sml/not-modified)) "" :group 'smart-mode-line-faces)
(defface sml/modified         '((t :inherit sml/not-modified :foreground "Red" :weight bold))
  "" :group 'smart-mode-line-faces)
(defface sml/outside-modified '((t :inherit sml/not-modified :foreground "#ffffff" :background "#c82829"))
  "" :group 'smart-mode-line-faces)

(defface sml/line-number         '((t :inherit sml/modes :weight bold))               "" :group 'smart-mode-line-faces)
(defface sml/remote              '((t :inherit sml/global))                           "" :group 'smart-mode-line-faces)
(defface sml/name-filling        '((t :inherit sml/position-percentage))              "" :group 'smart-mode-line-faces)
(defface sml/position-percentage '((t :inherit sml/prefix :weight normal))            "" :group 'smart-mode-line-faces)
(defface sml/col-number          '((t :inherit sml/global))                           "" :group 'smart-mode-line-faces)
(defface sml/numbers-separator   '((t :inherit sml/col-number))                       "" :group 'smart-mode-line-faces)
(defface sml/client              '((t :inherit sml/prefix))                           "" :group 'smart-mode-line-faces)
(defface sml/not-modified        '((t :inherit sml/global))                           "" :group 'smart-mode-line-faces)
(defface sml/mule-info           '((t :inherit sml/global))                           "" :group 'smart-mode-line-faces)
(defface sml/sudo                '((t :inherit sml/outside-modified))                 "" :group 'smart-mode-line-faces)
(defface sml/git                 '((t :inherit (sml/read-only sml/prefix)))           "" :group 'smart-mode-line-faces)
(defface sml/folder              '((t :inherit sml/global :weight normal))            "" :group 'smart-mode-line-faces)
(defface sml/process             '((t :inherit sml/prefix))                           "" :group 'smart-mode-line-faces)
(defface sml/vc                  '((t :inherit sml/git))                              "" :group 'smart-mode-line-faces)
(defface sml/vc-edited           '((t :inherit sml/prefix))                           "" :group 'smart-mode-line-faces)
(defface sml/charging            '((t :inherit sml/global :foreground "ForestGreen")) "" :group 'smart-mode-line-faces)
(defface sml/discharging         '((t :inherit sml/global :foreground "Red"))         "" :group 'smart-mode-line-faces)
(defface sml/time                '((t :inherit sml/modes))                            "" :group 'smart-mode-line-faces)

(defvar sml/-apply-theme-is-running nil "Avoid nesting in `sml/apply-theme'.")

(defcustom sml/no-confirm-load-theme nil
  "If non-nil, `sml/apply-theme' will pass the NO-CONFIRM flag to `load-theme'.
If you're having problems with Emacs always asking for permission
to load a theme (and not remembering your choice), you can set
this to t to workaround the problem. But it's recommended that
you try the problem instead."
  :type 'boolean
  :group 'smart-mode-line-faces
  :package-version '(smart-mode-line . "2.5.2"))

;;;###autoload
(when load-file-name
  (let ((dir (file-name-as-directory (file-name-directory load-file-name))))
    (add-to-list 'custom-theme-load-path dir)
    (when (file-directory-p (file-name-as-directory (concat dir "themes")))
      (add-to-list 'custom-theme-load-path
                   (file-name-as-directory (concat dir "themes"))))))

(defun sml/apply-theme (theme &optional value silent)
  "Apply the theme called smart-mode-line-THEME.

THEME is usually one of the symbols: respectful, dark, or light;
but it can be something else if there are other smart-mode-line
themes defined.
See the `sml/theme' variable for the meaning of each symbol.

This function will call `disable-theme' on any enabled themes
whose name starts with \"smart-mode-line-\", then it will call
`load-theme' on the theme called \"smart-mode-line-THEME\".

This also sets the `sml/theme' variable, see its documentation
for more information on each value.

The second argument (VALUE) is for internal use only, DON'T USE IT.

Third argument SILENT prevents messages."
  (interactive
   (list
    (intern
     (completing-read
      "Load smart-mode-line theme: "
      (cons
       'automatic
       (mapcar
        (lambda (x) (replace-regexp-in-string "\\`smart-mode-line-" "" (symbol-name x)))
        (cl-remove-if-not #'sml/theme-p (custom-available-themes))))))
    nil nil))
  (sml/-debug "Entering apply-theme")
  (when (eq theme (intern "")) (setq theme nil))
  (sml/-debug theme)
  (sml/-debug sml/theme)
  (unless silent (message "[sml] %s set to %s" 'sml/theme (or value theme)))
  (sml/-debug sml/-apply-theme-is-running)
  (unless sml/-apply-theme-is-running
    (let ((sml/-apply-theme-is-running t)) ;Avoid nesting.
      ;; Set the variable
      (setq-default sml/theme (or value theme))
      (sml/-debug sml/theme)

      ;; Disable any previous smart-mode-line themes.
      (sml/-debug custom-enabled-themes)
      (mapc (lambda (x) (when (sml/theme-p x) (disable-theme x)))
            custom-enabled-themes)
      (sml/-debug custom-enabled-themes)

      ;; Load the theme requested.
      (sml/-debug sml/theme)
      (when (eq sml/theme 'automatic)
        (setq sml/theme (sml/-automatically-decide-theme)))
      (sml/-debug sml/theme)
      (when sml/theme
        (let ((theme-name
               (if (sml/theme-p sml/theme) sml/theme
                 (intern (format "smart-mode-line-%s" sml/theme)))))
          (sml/-debug theme-name)
          (load-theme theme-name sml/no-confirm-load-theme))))))

(defadvice enable-theme (after sml/after-enable-theme-advice (theme) activate)
  "Make sure smart-mode-line themes take priority over global themes that don't customize sml faces."
  (unless (or (eq theme 'user) (sml/faces-from-theme theme))
    (mapc #'enable-theme
          (reverse (cl-remove-if-not #'sml/theme-p custom-enabled-themes)))))

(defun sml/theme-p (theme)
  "Return non-nil if theme named THEME is a smart-mode-line theme.
Takes symbols and strings."
  (string-match "\\`smart-mode-line-" (if (symbolp theme) (symbol-name theme) theme)))

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

;;; Buffer Identification
(defvar sml/buffer-identification-filling nil
  "Filling generated by `sml/fill-for-buffer-identification'.")
(make-variable-buffer-local 'sml/buffer-identification-filling)
(put 'sml/buffer-identification-filling 'risky-local-variable t)

(defvar sml/buffer-identification nil
  "Used for recycling buffer identification without having to recompute it.")
(make-variable-buffer-local 'sml/buffer-identification)
(put 'sml/buffer-identification 'risky-local-variable t)

(defadvice rename-buffer (after sml/after-rename-buffer-advice ())
  "Regenerate buffer-identification after `rename-buffer'."
  (sml/generate-buffer-identification))
(defadvice set-visited-file-name (after sml/after-set-visited-file-name-advice ())
  "Regenerate buffer-identification after `set-visited-file-name'."
  (sml/generate-buffer-identification))

(defvar sml/name-width-old nil "Used for recalculating buffer identification filling only when necessary.")
(make-variable-buffer-local 'sml/name-width-old)
(defvar sml/shorten-directory-old nil "Used for recalculating buffer identification filling only when necessary.")
(make-variable-buffer-local 'sml/shorten-directory-old)
(defun sml/generate-buffer-identification-if-necessary ()
  "Call `sml/generate-buffer-identification' only if `sml/name-width' has changed."
  (unless (and (equal sml/name-width-old sml/name-width)
               (equal sml/shorten-directory-old sml/shorten-directory))
    (setq sml/name-width-old sml/name-width)
    (setq sml/shorten-directory-old sml/shorten-directory)
    (sml/generate-buffer-identification))
  nil)

(defvar sml/mode-line-client
  `(sml/show-client
    (:eval (if (frame-parameter nil 'client)
               ,(propertize "@" 'face 'sml/client 'help-echo (purecopy "emacsclient frame"))
             " ")))
  "Construct that replaces `mode-line-client'.")

(defvar sml/mode-line-buffer-identification
  '("" (sml/buffer-identification
        sml/buffer-identification
        (:eval (sml/generate-buffer-identification))))
  "Replace the default `mode-line-buffer-identification' with our own.")

(defvar sml/projectile-replacement-format)
(defvar sml/use-projectile-p)
(defvar sml/projectile-loaded-p nil "Non-nil if projectile has been loaded.")

(defcustom sml/pos-id-separator " "
  "Miscellaneous mode-line construct."
  :type 'string)
(put 'sml/pos-id-separator 'risky-local-variable t)
(defcustom sml/pre-modes-separator " "
  "Miscellaneous mode-line construct."
  :type 'string)
(put 'sml/pre-modes-separator 'risky-local-variable t)
(defcustom sml/pre-id-separator ""
  "Miscellaneous mode-line construct."
  :type 'string)
(put 'sml/pre-id-separator 'risky-local-variable t)
(defcustom sml/pre-minor-modes-separator ""
  "Miscellaneous mode-line construct."
  :type 'string)
(put 'sml/pre-minor-modes-separator 'risky-local-variable t)
(defcustom sml/pos-minor-modes-separator ""
  "Miscellaneous mode-line construct."
  :type 'string)
(put 'sml/pos-minor-modes-separator 'risky-local-variable t)

(defun sml/-automatically-decide-theme ()
  "Return the most appropriate sml theme, based on global theme."
  (sml/-debug "Entering -automatically-decide-theme")
  (sml/-debug (sml/global-theme-support-sml-p))
  (unless (sml/global-theme-support-sml-p)
    (sml/-debug (face-background 'mode-line nil t))
    (sml/-debug (face-background 'default nil t))
    (let ((bg (ignore-errors
                (or (face-background 'mode-line nil t)
                    (face-background 'default nil t)))))
      (if (ignore-errors
            (and (stringp bg)
                 (> (color-distance "white" bg)
                    (color-distance "black" bg))))
          'dark 'light))))

(defun sml/-setup-theme ()
  "Decide what theme to use and apply it.
Used during initialization."
  (sml/-debug "Entering -setup-theme")
  (sml/-debug sml/theme)
  (when sml/theme
    (when (eq sml/theme 'automatic)
      (setq sml/theme (sml/-automatically-decide-theme)))
    (sml/-debug "chosen theme:")
    (sml/-debug sml/theme)
    (sml/apply-theme sml/theme nil :silent)))

(defvar battery-mode-line-format)


;;;###autoload
(defun sml/setup (&optional arg)
  "Setup the mode-line to be smart and sexy.

ARG is ignored. Just call this function in your init file, and
the mode-line will be setup."
  (interactive)
  (sml/-debug "Entering setup")
  (sml/-debug custom-enabled-themes)

  ;; Just a couple of useful variables
  (setq sml/simplified nil)
  (setq battery-mode-line-format sml/battery-format)

  ;; Activate rich-minority, and configure it for us.
  (setq rm-base-text-properties
        (append rm-base-text-properties '('face 'sml/minor-modes)))

  ;; Set the theme the user requested.
  (sml/-setup-theme)

  ;;;; And this is where the magic happens.
  ;; Remove elements we implement separately, and improve the ones not removed.
  (sml/filter-mode-line-list 'mode-line-mule-info)
  (setq-default mode-line-client sml/mode-line-client)
  (sml/filter-mode-line-list 'mode-line-modified)
  (sml/filter-mode-line-list 'mode-line-remote)
  (setq-default mode-line-frame-identification
                '("" (sml/show-frame-identification "%F")
                  sml/pre-id-separator))

  ;; (setq-default mode-line-buffer-identification '("%b"))

  (setq-default mode-line-buffer-identification
                sml/mode-line-buffer-identification)
  (sml/filter-mode-line-list 'mode-line-position)
  (sml/filter-mode-line-list 'mode-line-modes)
  (setq-default mode-line-end-spaces nil)

  ;; Add position descriptions on the left (they were already removed
  ;; from the middle). Since this is the very first symbol to be
  ;; evaluated, we also use it for calculating variables that need to
  ;; be updated
  (setq-default mode-line-front-space '((:eval (sml/generate-buffer-identification-if-necessary))
                                        (sml/position-help-text
                                         nil
                                         (:eval (let ((sml/-this-buffer-changed-p t))
                                                  (sml/generate-position-help))))
                                        (sml/position-construct
                                         sml/position-construct
                                         (:eval (sml/compile-position-construct)))))

  (add-hook 'after-save-hook 'sml/generate-buffer-identification)
  (ad-activate 'rename-buffer)
  (ad-activate 'set-visited-file-name)
  (add-hook 'clone-indirect-buffer-hook 'sml/generate-buffer-identification)
  ;; (ad-activate 'set-buffer-modified-p)
  (add-hook 'after-change-functions 'sml/-this-buffer-changed)
  (add-hook 'post-command-hook 'sml/generate-position-help)

  ;; This is to ensure fixed name width. The reason we do this manually
  ;; is that some major-modes change `mode-line-buffer-identification'
  ;; (so we can't fill inside the variable), and we want this
  ;; symbol to be an element in `mode-line-format' for compatibility
  ;; with other packages which hack into the mode-line.

  (add-to-list 'mode-line-position
               '(sml/buffer-identification-filling
                 sml/buffer-identification-filling
                 (:eval (setq sml/buffer-identification-filling
                              (sml/fill-for-buffer-identification)))))

  ;; Remove some annoying big spaces
  (setq-default mode-line-format
                (mapcar
                 (lambda (x) (cond
                         ;; ((eq x 'mode-line-buffer-identification)
                         ;;  '(:propertize mode-line-buffer-identification face sml/id))
                         ((and (stringp x) (string= x "   "))
                          'sml/pos-id-separator)
                         ((and (stringp x) (string= x "  "))
                          'sml/pre-modes-separator)
                         (t x)))
                 mode-line-format))

      ;;;; And here comes support for a bunch of extra stuff. Some of
      ;;;; these are just needed for coloring.

  ;; Shell and eshell support
  (add-hook 'comint-output-filter-functions 'sml/generate-buffer-identification)
  (add-hook 'eshell-directory-change-hook 'sml/generate-buffer-identification)

  ;; ;; Term support - Disabled for now because of Issue#198
  ;; (defadvice term-command-hook (after sml/term-advice-1 activate)
  ;;   (sml/generate-buffer-identification))

  ;; (defadvice term-handle-ansi-terminal-messages (after sml/term-advice-2 activate)
  ;;   (sml/generate-buffer-identification))

  ;; Dired overrides the buffer-identification (which we would
  ;; normally respect) but doesn't actually do anything useful with
  ;; it, so we overoverride back.
  (add-hook 'dired-mode-hook 'sml/set-buffer-identification)

  ;; Display time
  (add-hook 'display-time-hook 'sml/propertize-time-string)

  ;; Battery support
  (eval-after-load 'battery
    '(defadvice battery-update (after sml/after-battery-update-advice () activate)
       "Change battery color."
       (when battery-mode-line-string
         (setq battery-mode-line-string
               (propertize battery-mode-line-string
                           'face 'sml/battery)))))

  ;; Projectile support
  (eval-after-load "projectile"
    '(progn
       (setq sml/projectile-loaded-p t)
       (defcustom sml/projectile-replacement-format "[%s]"
         "Format used for replacements derived from projectile."
         :type 'string
         :group 'smart-mode-line-others
         :package-version '(smart-mode-line . "2.4"))
       (defcustom sml/use-projectile-p 'after-prefixes
         "Whether we should use projectile to guess path prefixes.

If this is non-nil, and if current buffer is inside a project (as
defined by projectile), we use the project's name as a
prefix (with the `sml/projectile-replacement-format' variable).

If this is 'after-prefix, then this replacement will only be used
if no other prefixes (defined in `sml/replacer-regexp-list') were
found to match the current file path."
         :type '(choice (const :tag "Use projectile only if current path doesn't match any prefixes." after-prefixes)
                        (const :tag "Use projectile before checking prefixes." before-prefixes)
                        (const :tag "Don't use projectile." nil))
         :group 'smart-mode-line-others
         :package-version '(smart-mode-line . "2.4.1"))
       (defface sml/projectile '((t :inherit sml/git)) "" :group 'smart-mode-line-faces)
       (add-to-list 'sml/prefix-regexp (format (regexp-quote sml/projectile-replacement-format) ".*"))))

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
    (setq erc-track-position-in-mode-line t))

  (run-hooks 'sml/after-setup-hook))

;;;###autoload
(defalias 'smart-mode-line-enable #'sml/setup)

(defun sml/global-theme-support-sml-p ()
  "Non-nil if any of the enabled themes supports smart-mode-line."
  (cl-remove-if
   #'sml/theme-p
   (cl-remove-if-not #'sml/faces-from-theme custom-enabled-themes)))

(defun sml/faces-from-theme (theme)
  "Return the sml faces that THEME customizes."
  (cl-remove-if-not
   (lambda (it) (string-match "\\`sml/" (symbol-name it)))
   (mapcar #'cadr (get theme 'theme-settings))))

(defun sml/set-buffer-identification (&rest ignored)
  "Setq the buffer-identification of this buffer back to ours.

Currently, we only this for dired. For other modes (like info) we
respect their changes.

Argument IGNORED is obsolete."
  (setq mode-line-buffer-identification sml/mode-line-buffer-identification))

(defvar sml/-this-buffer-changed-p nil
  "t if buffer was changed since last help-text update.")
(make-variable-buffer-local 'sml/-this-buffer-changed-p)

(defun sml/-this-buffer-changed (&rest ignored)
  "Set variable `sml/-this-buffer-changed-p' to t.
Argument IGNORED is ignored."
  (setq sml/-this-buffer-changed-p t) nil)

(defun sml/generate-position-help (&rest ignored)
  "Set the string describing various buffer content information.
Argument IGNORED is ignored."
  (when (and sml/-this-buffer-changed-p
             (get-buffer-window (current-buffer)))
    (setq sml/-this-buffer-changed-p nil)
    (setq sml/position-help-text
          (format-mode-line
           (concat "Buffer size:\n\t%IB\n"
                   ;; ;; This is way too slow, unfortunately.
                   ;; "Number of Lines:\n\t"
                   ;; (int-to-string (line-number-at-pos (point-max)))
                   "\nmouse-1: Display Line and Column Mode Menu")))
    nil))

(defcustom sml/order-of-line-and-column nil
  "Decide the order of line-number and column-number display.

When both `line-number-mode' and `column-number-mode' are
enabled, this variable decides which gets displayed on the left,
and which gets displayed on the right. If either one of the modes
is not enabled, this variable has no effect (obviously).

It can only be t or nil.
    t means column-number:line-number
    nil means line-number:column-number"
  :type '(choice (const :tag "column-number:line-number" t)
                 (const :tag "line-number:column-number" nil))
  :group 'smart-mode-line-position
  :package-version '(smart-mode-line . "2.4"))

(defun sml/compile-position-construct (&optional symbol value)
  "Recompile the `sml/position-construct' after one of the formats was edited.
Also sets SYMBOL to VALUE."
  (when (and symbol value) (set symbol value))
  (sml/generate-position-help)
  (setq sml/position-construct
        `((size-indication-mode
           ,(propertize sml/size-indication-format
                        'face 'sml/col-number
                        'help-echo 'sml/position-help-text
                        'mouse-face 'mode-line-highlight
                        'local-map mode-line-column-line-number-mode-map))
          (sml/order-of-line-and-column
           (column-number-mode
            ,(propertize sml/col-number-format
                         'face 'sml/col-number
                         'help-echo 'sml/position-help-text
                         'mouse-face 'mode-line-highlight
                         'local-map mode-line-column-line-number-mode-map))
           (line-number-mode
            ,(propertize sml/line-number-format
                         'face 'sml/line-number
                         'help-echo 'sml/position-help-text
                         'mouse-face 'mode-line-highlight
                         'local-map mode-line-column-line-number-mode-map)))
          (column-number-mode
           (line-number-mode
            ,(propertize sml/numbers-separator
                         'face 'sml/numbers-separator
                         'help-echo 'sml/position-help-text
                         'mouse-face 'mode-line-highlight
                         'local-map mode-line-column-line-number-mode-map)))
          (sml/order-of-line-and-column
           (line-number-mode
            ,(propertize sml/line-number-format
                         'face 'sml/line-number
                         'help-echo 'sml/position-help-text
                         'mouse-face 'mode-line-highlight
                         'local-map mode-line-column-line-number-mode-map))
           (column-number-mode
            ,(propertize sml/col-number-format
                         'face 'sml/col-number
                         'help-echo 'sml/position-help-text
                         'mouse-face 'mode-line-highlight
                         'local-map mode-line-column-line-number-mode-map))))))

(defun sml/generate-modified-status ()
  "Return a string describing the modified status of the buffer."
  (cond
   ((not (or (and (buffer-file-name) (file-remote-p buffer-file-name))
             (verify-visited-file-modtime (current-buffer))))
    (propertize sml/outside-modified-char 'face 'sml/outside-modified
                'help-echo "Modified outside Emacs!\nRevert first!"))
   ((buffer-modified-p)
    (propertize (if buffer-read-only
                    sml/read-only-char
                  sml/modified-char)
                'face 'sml/modified
                'help-echo (if (and (buffer-file-name) (not (file-remote-p buffer-file-name)))
                               (format-time-string
                                sml/modified-time-string
                                (nth 5 (file-attributes (buffer-file-name))))
                             "Buffer Modified")
                'local-map '(keymap (mode-line keymap (mouse-1 . save-buffer)))))
   (buffer-read-only (propertize sml/read-only-char
                                 'face 'sml/read-only
                                 'help-echo "Read-Only Buffer"))
   (t (propertize sml/not-modified-char 'face 'sml/not-modified))))

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

L must be a symbol! We assign right back to it"
  (if (and (symbolp l) (listp (eval l)))
      (set-default l
                   (remove nil (mapcar 'sml/parse-mode-line-elements (eval l))))
    (error "l must be a symbol to a list!")))

(defun sml/fill-for-buffer-identification ()
  "Return a string such that `mode-line-buffer-identification' is fixed-width.
In buffers where `mode-line-buffer-identification' is nil, we
don't do any filling. That's because the given mode probably
doesn't want any buffer-id."
  (if mode-line-buffer-identification
      (propertize
       (make-string (max (- (or (car-safe sml/name-width) sml/name-width)
                            (string-width (format-mode-line mode-line-buffer-identification)))
                         0)
                    sml/fill-char)
       'face 'sml/name-filling)
    ""))

(defun sml/generate-buffer-identification (&rest ignored)
  "Return fully propertized prefix+path+buffername.
Argument IGNORED is ignored."
  (setq sml/name-width-old sml/name-width)
  (setq sml/buffer-identification-filling nil)
  (when (and (listp mode-line-buffer-identification)
             (or ;; Only calculate all this if it will actually be used
              (equal sml/mode-line-buffer-identification mode-line-buffer-identification)
              (member (cadr sml/mode-line-buffer-identification) mode-line-buffer-identification)
              (member sml/mode-line-buffer-identification mode-line-buffer-identification)))
    (setq sml/buffer-identification
          (let* ((dir (sml/replacer (abbreviate-file-name (sml/get-directory))))
                 (sml/use-projectile-p (unless (or (not sml/projectile-loaded-p)
                                                   (and (buffer-file-name)
                                                        (file-remote-p (buffer-file-name))))
                                         sml/use-projectile-p))
                 (prefix (sml/get-prefix dir))
                 (bufname (sml/buffer-name))
                 (dirsize (max 0 (- (abs (or (cdr-safe sml/name-width) sml/name-width))
                                    (string-width prefix) (string-width bufname))))
                 (dirstring (funcall sml/shortener-func dir dirsize)))

            (propertize (concat (sml/propertize-prefix (replace-regexp-in-string "%" "%%" prefix))
                                (propertize (replace-regexp-in-string "%" "%%" dirstring) 'face 'sml/folder)
                                (propertize (replace-regexp-in-string "%" "%%" bufname) 'face 'sml/filename))
                        'help-echo (format "%s\n\nmouse-1: Previous buffer\nmouse-3: Next buffer"
                                     (or (buffer-file-name) (buffer-name)))
                        'mouse-face 'mode-line-highlight
                        'local-map   mode-line-buffer-identification-keymap)))))

(defun sml/parse-mode-line-elements (el)
  "Propertize or delete EL.

To be used in mapcar and accumulate results."
  (cond
   ;; These are implemented separately
   ((member el '("%[" "%]" "%1+" "(" ")" (t erc-modified-channels-object)
                 (:eval (if (display-graphic-p) " " "-"))
                 (:eval (unless (display-graphic-p) "-%-"))
                 (:eval (mode-line-frame-control))))
    nil)
   ((member (car-safe el) '(line-number-mode column-number-mode size-indication-mode current-input-method)) nil)
   ;; mode-line-remote
   ((and (stringp el) (string= el "%1@"))
    `(sml/show-remote
      (:propertize ,el face sml/remote)))
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
   ((or (sml/is-%p-p el)
        (and (listp el) (memq 'mode-line-percent-position el)))
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
    `(sml/mule-info ((1 (:propertize
                         (current-input-method
                          ("" current-input-method-title)
                          " ")
                         face sml/mule-info
                         help-echo (concat
                                    ,(purecopy "Current input method: ")
                                    current-input-method
                                    ,(purecopy "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method"))
                         local-map ,mode-line-input-method-map
                         mouse-face mode-line-highlight))
                     (:propertize (:eval sml/mule-info)
                                  face sml/mule-info
                                  help-echo mode-line-mule-info-help-echo
                                  mouse-face mode-line-highlight
                                  local-map ,mode-line-coding-system-map))))
   ;; Make EOL optional
   ((equal el '(:eval (mode-line-eol-desc)))
    '(sml/show-eol (:eval (mode-line-eol-desc))))

   ;;;; mode-line-modes
   ;; Color the mode line process
   ((or (equal el '("" mode-line-process))
        (equal (car (cdr-safe el)) '("" mode-line-process)))
    '(mode-line-process
      (:eval (let ((text (format-mode-line mode-line-process)))
               (add-face-text-property 0 (length text) 'sml/process t text)
               text))))
   ;; Color the mode name, without changing other properties
   ((and (listp el)
         (equal (car el) :propertize)
         (equal (cadr el) '("" mode-name)))
    (setf (cadr el) '("" "%[" mode-name "%]"))
    (append el '(face sml/modes)))
   ;; Completely replace the minor modes (so we can truncate)
   ((and (listp el)
         (equal (car el) :propertize)
         (equal (cadr el) '("" minor-mode-alist)))
    '(:eval (sml/generate-minor-modes)))

   ;; ;;; Propertize misc-info
   ;; ((memq (car-safe el) '(which-func-mode global-mode-string))
   ;;  `(:eval (add-text-properties (format-mode-line ',el))))

   ;; If it's something we don't recognize, just leave it as-is.
   (t el)))

(defun sml/is-%p-p (x)
  "Non-nil if X matches \"%p\" in a very subjective sense."
  (or (and (listp x)
           (cl-remove-if-not
            (lambda (y) (string-match ".*%p.*" y))
            (cl-remove-if-not #'stringp x)))
      (and (stringp x)
           (string-match ".*%p.*" x))))

(defun sml/buffer-name ()
  "Return either buffer name or file name to be shown on the mode-line.
Uses `sml/show-file-name' to decide between the two.

Unless `sml/show-trailing-N' is nil, prevents the \"<N>\" (used in
duplicated buffer names) from being displayed."
  (cond ((buffer-base-buffer)
         (buffer-name))
        ((and sml/show-file-name (buffer-file-name))
         (file-name-nondirectory (buffer-file-name)))
        ((derived-mode-p 'dired-mode)
         (file-name-nondirectory (directory-file-name default-directory)))
        (sml/show-trailing-N
         (buffer-name))
        (t (replace-regexp-in-string "<[0-9]+>$" "" (buffer-name)))))

(defun sml/fill-width-available ()
  "Return the size available for filling."
  (max 0
       (+ sml/extra-filler
          (- (window-total-width)
             (let ((sml/simplified t))
               (string-width (format-mode-line mode-line-format)))))))

(defconst sml/propertized-shorten-mode-string
  '(:propertize sml/shorten-mode-string
                face sml/minor-modes
                help-echo "mouse-1: Shorten minor modes"
                local-map (keymap (mode-line keymap (mouse-1 . sml/toggle-shorten-modes)))
                mouse-face mode-line-highlight))
(defconst sml/propertized-full-mode-string
  '(:propertize sml/full-mode-string
                face sml/minor-modes
                help-echo "mouse-1: Show all modes"
                local-map (keymap (mode-line keymap (mouse-1 . sml/toggle-shorten-modes)))
                mouse-face mode-line-highlight))

(defun sml/count-occurrences-starting-at (regex string start)
  "Count occurrences of REGEX in STRING starting at index START."
  (if (string-match regex string start)
      (+ 1 (sml/count-occurrences-starting-at regex string (match-end 0)))
    0))

;;; Patch, in case the user is using the wrong variable.
(defvar sml/-hidden-modes-bound-by-user
  (bound-and-true-p sml/hidden-modes))
(when sml/-hidden-modes-bound-by-user
  (setq sml/-hidden-modes-bound-by-user nil)
  (setq rm-blacklist (bound-and-true-p sml/hidden-modes)))
(define-obsolete-variable-alias 'sml/hidden-modes 'rm-blacklist "2.9")

(defun sml/generate-minor-modes ()
  "Extracts all rich strings necessary for the minor mode list."
  (if sml/simplified
      ""
    (let* (;; The minor-mode-alist
           (nameList (rm--mode-list-as-string-list))
           ;; The size available
           (size (max 0
                      (- (if (member sml/mode-width '(full right))
                             ;; Calculate how much width is available
                             (sml/fill-width-available)
                           ;; or use what the user requested.
                           sml/mode-width)
                         (string-width (format-mode-line
                                        'sml/pre-minor-modes-separator))
                         (string-width (format-mode-line
                                        'sml/pos-minor-modes-separator)))))
           ;; Used for counting size.
           (finalNameList (mapconcat 'identity  nameList ""))
           needs-removing filling)

      ;; Calculate whether truncation is necessary.
      (when (and sml/shorten-modes (> (string-width finalNameList) size))
        ;; We need to remove 1+ "the number of spaces found".
        (setq needs-removing
              (1+
               (sml/count-occurrences-starting-at
                " " finalNameList
                (- size (string-width sml/full-mode-string))))))
      ;; Add truncation string if necessary
      (when needs-removing
        (setcdr (last nameList (1+ needs-removing))
                (list t sml/propertized-full-mode-string)))
      ;; If we're not shortenning, add " -" at the end.
      (unless sml/shorten-modes
        (add-to-list 'nameList sml/propertized-shorten-mode-string t))

      ;; Padding
      (setq filling (- size (string-width (format-mode-line nameList))))
      (setq filling (make-string (max 0 filling) sml/fill-char))

      (if (eq sml/mode-width 'right)
          (list (propertize filling 'face 'sml/modes)
                'sml/pre-minor-modes-separator nameList
                'sml/pos-minor-modes-separator)
        (list "" 'sml/pre-minor-modes-separator nameList
              'sml/pos-minor-modes-separator filling)))))

(defun sml/propertize-prefix (prefix)
  "Set the color of PREFIX according to its contents."
  (cl-loop for pair in sml/prefix-face-list
           if (let* ((c (car pair))
                     (s (if (symbolp c)
                            (when (boundp c) (symbol-value c))
                          c)))
                (when s
                  (string-match (format (regexp-quote s) ".*") prefix)))
           return (propertize prefix 'face (car (cdr pair)))))

(defun sml/get-directory ()
  "Decide if we want directory shown. If so, return it."
  (abbreviate-file-name
   (cond
    ;; In email attachments, buffer-file-name is non-nil, but
    ;; file-name-directory returns nil
    ((buffer-file-name) (or (file-name-directory (buffer-file-name)) ""))
    ((eq major-mode 'dired-mode)
     (replace-regexp-in-string "/[^/]*/$" "/" default-directory))
    ((and (symbolp major-mode)
          (member major-mode '(shell-mode eshell-mode term-mode)))
     default-directory)
    ;; In indirect buffers, buffer-file-name is nil. The correct value is
    ;; retrieved from the base buffer.
    ((buffer-base-buffer)
     (with-current-buffer (buffer-base-buffer) (sml/get-directory)))
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
  "Fontify the battery display."
  (sml/set-battery-font))

(defun sml/replacer (in)
  "Run on string IN the replacements from `sml/replacer-regexp-list'.
Runs first on the given path, and if that doesn't have any affect,
runs them again on a version of the given path with all symlinks
expanded via `file-truename'.  If neither run succeeds in making
any replacements, returns the path originally given.

Used by `sml/strip-prefix' and `sml/get-prefix'."
  ;; First try replacing on the original path
  (if (string= in "")
      in
    (sml/replacer-raw in)))

(defcustom sml/fallback-on-buffer-identification nil
  "Whether to fallback on regular buffer-identification.
Defines the what should be displayed in the buffer identification
if it is unchanged by the entries in `sml/replacer-regexp-list'.
If the value is nil, use the sml behaviour (full file name).
Otherwise, use the default Emacs behaviour (usually just `buffer-name')."
  :type 'boolean)

(defun sml/replacer-raw (in)
  "Run on the string IN the replacements from `sml/replacer-regexp-list'.

If projectile is loaded, also performs replacements specified by
project name first."
  (let ((out in)
        proj)
    ;; Maybe try projectile
    (when (and sml/projectile-loaded-p
               (eq sml/use-projectile-p 'before-prefixes))
      (setq out (sml/perform-projectile-replacement out)))
    ;; Try regular replacements
    (when (string= out in)
      (dolist (cur sml/replacer-regexp-list)
        (setq out (replace-regexp-in-string (car cur) (car (cdr cur)) out))))
    (when (and sml/fallback-on-buffer-identification
               (string= out in))
      (setq out (format-mode-line (propertized-buffer-identification "%12b"))))
    ;; Try truename replacements
    (when (string= out in)
      (let* ((true-in (abbreviate-file-name (if (file-remote-p in)
                                                in
                                              (file-truename in))))
             (true-out true-in))
        (dolist (cur sml/replacer-regexp-list)
          (setq true-out (replace-regexp-in-string
                          (car cur) (car (cdr cur)) true-out)))
        (unless (string= true-in true-out)
          (setq out true-out))))
    ;; Maybe try projectile later
    (when (and sml/projectile-loaded-p
               (eq sml/use-projectile-p 'after-prefixes)
               (string= out in))
      (setq out (sml/perform-projectile-replacement out)))
    out))

(declare-function projectile-project-p "projectile")
(declare-function projectile-project-name "projectile")
(defun sml/perform-projectile-replacement (in)
  "If path IN is inside a project, use its name as a prefix."
  (let ((proj (projectile-project-p)))
    (if (stringp proj)
        (let* ((replacement
                (format sml/projectile-replacement-format
                  (projectile-project-name)))
               (short (replace-regexp-in-string
                       (concat "^" (regexp-quote (abbreviate-file-name proj)))
                       replacement
                       in)))
          (if (string= short in)
              (let* ((true-in (abbreviate-file-name (file-truename in)))
                     (true-short
                      (replace-regexp-in-string
                       (concat "^" (regexp-quote (abbreviate-file-name (file-truename proj))))
                       replacement true-in)))
                (if (string= true-in true-short) in true-short))
            short))
      in)))

(defun sml/regexp-composer (getter)
  "Prepare the actual regexp using `sml/prefix-regexp'.
If GETTER is non-nil, result regexp also accepts empty match."
  (let ((left "^\\(")
        (right (if getter "\\|\\).*" "\\)")))
    (if (stringp sml/prefix-regexp)
        (if (string-match "\\(" sml/prefix-regexp)
            sml/prefix-regexp
          (concat left sml/prefix-regexp right))
      (concat left (mapconcat 'identity sml/prefix-regexp "\\|") right))))

(defun sml/strip-prefix (path)
  "Remove prefix from string PATH.

A prefix is anything at the beginning of the line that matches a
regexp in `sml/prefix-regexp'."
  (replace-regexp-in-string (sml/regexp-composer nil) "" path))

(defun sml/get-prefix (path)
  "Get prefix from string PATH.

A prefix is anything at the beginning of the line that matches a
regexp in `sml/prefix-regexp'."
  (replace-regexp-in-string (sml/regexp-composer t) "\\1" path))

(defun sml/not-shorten-directory (dir ml)
  "Return DIR, abbreviated and prefixed.
ML isn't used."
  (sml/strip-prefix dir))

(defcustom sml/directory-truncation-string (if (char-displayable-p ?…) "…/" ".../")
  "String used when truncating part of the file path.
Set this to nil or an empty string if you don't want any
indication of a truncated path."
  :type 'string
  :group 'smart-mode-line
  :package-version '(smart-mode-line . "2.10"))

(defun sml/do-shorten-directory (dir max-length)
  "Show up to MAX-LENGTH characters of a directory name DIR."
  (let ((longname (sml/strip-prefix dir)))
    ;; If it fits, return the string.
    (if (<= (string-width longname) max-length) longname
      ;; If it doesn't, shorten it
      (let ((path (reverse (split-string longname "/")))
            (output ""))
        (when (and path (equal "" (car path)))
          (setq path (cdr path)))
        (let ((max (- max-length (string-width sml/directory-truncation-string))))
          ;; Concat as many levels as possible, leaving 4 chars for safety.
          (while (and path (<= (string-width (concat (car path) "/" output))
                               max))
            (setq output (concat (car path) "/" output))
            (setq path (cdr path))))
        ;; If we had to shorten, prepend .../
        (when path
          (setq output (concat sml/directory-truncation-string output)))
        output))))

(provide 'smart-mode-line)
;;; smart-mode-line.el ends here
