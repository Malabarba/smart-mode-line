New in v2.6
===========
- Filtering of minor modes is now delegated to the
  [rich-minority](https://github.com/Bruce-Connor/rich-minority)
  package. This has 3 effects:
  - The original (black list) method of filtering is given by the
    variable `rm-blacklist`. `sml/hidden-modes` still works, but
    is obsolete.
  - Alternatively, modes can be filtered by a whitelist, using `rm-whitelist`.
  - Modes can also be propertized, using `rm-text-properties`.

- The file-path display can have different minimum and maximum widths.
  Just set `sml/name-width` to a cons cell `(MIN . MAX)`.

New in v2.5
===========
- Emacs 24.4 compatible.
- Integration with [Projectile](https://github.com/bbatsov/projectile)!
- Display `current-directory` in Shell and eshell.
- New value for `sml/theme`: `automatic` (highly recommended).
- `sml/apply-theme` is interactive and has completion.
- Smart-mode-line themes are now regular themes, which makes it easier to define new ones. You can activate them with `(load-theme 'smart-mode-line-light)`, though `(sml/apply-theme 'light)` still works and has the advantages of disabling the previous theme.

New in v2.1
===========
- **Themes!** (yes, finally). Besides the usual dark theme, we now have
light and a respectful theme. See `sml/theme`.
- **Zero Compromise.** This means you don't have to
give anything up in order to use this package. Every single bit of
information which is displayed in the standard mode-line is present in
`sml` (though some might need to be turned on). This includes 100%
compatibility with other external packages which display information
in the mode-line.
- Performance improvements.

