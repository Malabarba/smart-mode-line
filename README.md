smart-mode-line
===============

A fixed width smart mode line for Emacs.

Usage:
===
Make sure **"smart-mode-line.el"** is in your load path, then place
this code in your .emacs file:

    (require 'smart-mode-line)
    (sml/setup)

Description
===
Smart Mode Line is a mode-line format that aims to be easy to
read from small to large monitors by using a prefix feature and
smart truncation. Its main features are:

 1. **Color coded**:  
    Highlights the most important information for you
    (buffer name, modified state, line number). Don't
    like the colors? See item 4)!

 2. **Fixed width** (if you want):  
    Lets you set a maxium width for the path name and mode
    names, and truncated intelligently (truncates the
    directory, not the buffer name).

 3. **Fancy features**:  
    Prefix feature saves a LOT of space. e.g. **"~/.emacs.d/"**
    is translated to **":ED:"** in the path (open a file inside
    this folder to see it in action). Long path names you
    are commmonly working on are displayed as short
    abbreviations. Set your own prefixes to make best use
    of it (by configuring `sml/replacer-regexp-list`). Mousing
    over the abbreviated path will show you the full
    path. See below for examples.  	
    Hidden-modes feature saves even more space. Select
    which minor modes you don't want to see listed by
    customizing the `sml/hidden-modes` variable. This will
    filter out the modes you don't care about and unclutter
    the modes list (mousing over the modes list still shows
    the full list).

 4. **Very easy to configure**:  
    All fonts are in the `smart-mode-line-faces`
    customization group, and all other options are in
    `smart-mode-line`. Just run `sml/customize` and see
    what's in there. If you feel anything is missing send me
    an e-mail.
	
 5. **Compatible with `battery-display-mode` **:
	Just turn the mode on to have the battery level
	displayed. sml uses a very short syntax for the
	battery. Only the battery level is displayed (with no %
	symbol), and green/red font means charging/discharging
	respectively. `sml/battery-format` controls this format.

Important Variables:
===
All variables can be edited by running `sml/customize`, and the
documentations are mostly self explanatory, I list here only the
most important ones.

 1. `sml/shorten-directory` and `sml/shorten-modes`  
  Setting both of these to t garantees a fixed width mode-line
  (directory name and modes list will be truncated to fit). To
  actually define the width, see below.

 2. `sml/name-width` and `sml/mode-width`  
  Customize these according to the width of your emacs
  frame. I set them to 40 and 30 respectively, and the
  mode-line fits perfectly when the frame is split in two even
  on my laptop's small 17" monitor.

 3. `sml/replacer-regexp-list`  
  This variable is a list of (REGEXP REPLACEMENT) that is used
  to parse the path. The replacements are applied
  sequentially. This allows you to greatly abbreviate the path
  that's shown in the mode-line. If this abbreviation is of
  the form **":SOMETHING:"**, it is considered a prefix and get's
  a different color (you can change what's considered a prefix
  by customizing `sml/prefix-regexp`).  
  For example, if you do a lot of work on a folder called
  **"~/Dropbox/Projects/In-Development/"** almost half the
  mode-line would be occupied just by the folder name, which
  is much less important than the buffer name. But, you can't
  just hide the folder name, since editting a file in
  **"~/Dropbox/Projects/In-Development/Source"** is VERY different
  from editting a file in **"~/Dropbox/Projects/Source"**. By
  setting up a prefix for your commonly used folders, you get
  all that information without wasting all that space. In this
  example you could set the replacement to **":ProjDev:"** or just
  **":InDev:"**, so the path shown in the mode-line will be
  **":ProjDev:Source/"** (saves a lot of space without hiding
  information).  

Here go some more useful examples:

    (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/Projects/In-Development/" ":ProjDev:"))
    (add-to-list 'sml/replacer-regexp-list '("^~/Documents/Work/" ":Work:))
    
    ;; Added in the right order, they even work sequentially:
    (add-to-list 'sml/replacer-regexp-list '("^:DB:Documents" ":DDocs:"))
    (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:"))
    (add-to-list 'sml/replacer-regexp-list '("^~/Git-Projects/" ":Git:"))
    (add-to-list 'sml/replacer-regexp-list '("^:Git:\\(.*\\)/src/main/java/" ":G/\\1/SMJ:"))
