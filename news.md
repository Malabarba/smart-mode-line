New in v2.5
===========
- Emacs 24.4 compatible.
- Integration with [Projectile](https://github.com/bbatsov/projectile)!
- Display `current-directory` in Shell and eshell.
- `sml/apply-theme` is interactive and has completion.
- Smart-mode-line themes are now regular themes, which makes it easier to define new ones. You can activate them with `(load-theme 'smart-mode-line-light)`, though `(sml/apply-theme 'light)` still works.

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

