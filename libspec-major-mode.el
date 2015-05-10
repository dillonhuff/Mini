(setq mylsl-keywords '("operation" "gen" "r" "rw" "output" "matrix"))
(setq mylsl-types '("float" "double" "list" "rotation" "string" "vector"))

(setq mylsl-keywords-regexp (regexp-opt mylsl-keywords 'words))
(setq mylsl-type-regexp (regexp-opt mylsl-types 'words))

(setq mylsl-keywords nil)
(setq mylsl-types nil)
(setq mylsl-constants nil)
(setq mylsl-events nil)
(setq mylsl-functions nil)

(setq mylsl-font-lock-keywords
  `(
    (,mylsl-type-regexp . font-lock-type-face)
    (,mylsl-keywords-regexp . font-lock-keyword-face)
))

(define-derived-mode mylsl-mode fundamental-mode
  "lsl mode"
  "Major mode for editing Mini library specifications"
  (setq font-lock-defaults '((mylsl-font-lock-keywords)))

  (setq mylsl-keywords-regexp nil)
  (setq mylsl-types-regexp nil)
  (setq mylsl-constants-regexp nil)
  (setq mylsl-events-regexp nil)
  (setq mylsl-functions-regexp nil)
)

(provide 'mylsl-mode)
