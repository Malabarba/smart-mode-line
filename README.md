smart-mode-line [![Donate](https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=GLZLRLW72Q8G2)
---------------

Smart Mode Line is a sexy mode-line for Emacs, that aims to be easy to
read from small to large monitors by using a *prefix feature* and
*smart truncation*. 

**Dark Theme**  
![Dark Theme Screenshot](https://raw.github.com/Bruce-Connor/smart-mode-line/master/screenshot-2013-11-11-dark.png)

**Light Theme**  
![Light Theme Screenshot](https://raw.github.com/Bruce-Connor/smart-mode-line/master/screenshot-2013-11-11-light.png)

Installation
===
**smart-mode-line** is available on Melpa, and that's the recommended
way of installing it. If you do that, you can simply activate it with:

    (setq sml/theme 'dark)
    (sml/setup)

To install it manually, you need **emacs-version >= 24.3.**. First
make sure you install [dash.el](https://github.com/magnars/dash.el)
(which is a dependency), then make sure *"smart-mode-line.el"* is in
your load path, and finally place this code in your `.emacs` file:

    (setq sml/theme 'dark)
    (require 'smart-mode-line)
    (sml/setup)
    
Features
===
Its main features include:

 1. **Color coded**:  
    Highlights the most important information for you
    (buffer name, modified state, line number). Don't
    like the colors? See item 5)!

 2. **Fixed width** (if you want):  
    Lets you set a maxium width for the path name and mode names, and
    truncates them intelligently (truncates the directory, not the
    buffer name). Also let's you **right indent** strings in the
    mode-line (see `sml/mode-width`).

 3. **Directory as Prefixes**:  
    Prefix feature saves a LOT of space. e.g. **"~/.emacs.d/"**
    is translated to **":ED:"** in the path (open a file inside
    this folder to see it in action). Long path names you
    are commmonly working on are displayed as short
    abbreviations. Set your own prefixes to make best use
    of it (by configuring `sml/replacer-regexp-list`). Mousing
    over the abbreviated path will show you the full
    path. See below for examples.  	

 4. **Hide minor-modes**:__
    Hidden-modes feature saves even more space. Select
    which minor modes you don't want to see listed by
    customizing the `sml/hidden-modes` variable. This will
    filter out the modes you don't care about and unclutter
    the modes list (mousing over the modes list still shows
    the full list).

 5. **Very easy to configure**:  
    All colors and variables are customizable. You can change the
    whole theme with `sml/apply-theme`, or just customize anything
    manually with `sml/customize` and `sml/customize-faces`. There are
    *DOZENS* of variables to customize your mode-line, just pop over
    there and have a look!
	
 6. **Compatible with absolutely anything**:  
    I'm serious. Versions 2.0 and above should be compatible with
    **any** other packages that display information in the mode-line
    (evil, nyan-mode, elscreen, display-battery-mode, etc). If you
    find *ANYTHING* that does not appear as it should, file a bug report
    and I'll get to it.
    
Important Variables:
===
All variables can be edited by running `sml/customize`, and the
documentations are mostly self explanatory, I list here only the
most important ones.

 1. `sml/shorten-directory` and `sml/shorten-modes`  
  Choose what theme ou want to use for the mode-line colors. For now
  there are 3 different themes: `dark`, `light`, and `respectful`.
 
 1. `sml/shorten-directory` and `sml/shorten-modes`  
  Setting both of these to `t` garantees a fixed width mode-line
  (directory name and minor-modes list will be truncated to fit). To
  actually define the width, see below.
  
 2. `sml/name-width` and `sml/mode-width`  
  Customize these according to the width of your emacs frame. I set
  them to `40` and `full` respectively, and the mode-line fits
  perfectly when the frame is split in two even on my laptop's small
  17" monitor. `full` means everything after the minor-modes will be
  right-indented.
  
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
    (add-to-list 'sml/replacer-regexp-list '("^~/Documents/Work/" ":Work:"))
    
    ;; Added in the right order, they even work sequentially:
    (add-to-list 'sml/replacer-regexp-list '("^:DB:Documents" ":DDocs:"))
    (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:"))
    (add-to-list 'sml/replacer-regexp-list '("^:Git:\\(.*\\)/src/main/java/" ":G/\\1/SMJ:"))
    (add-to-list 'sml/replacer-regexp-list '("^~/Git-Projects/" ":Git:"))
