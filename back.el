(defun sml/replacer (in)
  "Runs the replacements specified in `sml/replacer-regexp-list'.

Used by `sml/strip-prefix' and `sml/get-prefix'."
  (let ((out in))
    (dolist (cur sml/replacer-regexp-list)
      (setq out (replace-regexp-in-string (car cur)
                                          (car (cdr cur))
                                          out)))
    out))
