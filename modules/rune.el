
(defvar rune--keywords
  '("pub" "await" "break" "continue" "do" "else" "for" "if" "loop" "match" "return" "try" "while" "yield" "select" "extern" "let" "macro" "mod" "const" "fn" "async" "impl" "trait" "struct" "in" "as"))

(defvar rune--keywords-regexp
  (regexp-opt rune--keywords 'symbols))

(defvar rune--types
  '("bool" "u8" "u16" "u32" "u64" "u128" "i8" "i16" "i32" "i64" "i128" "f32" "f64" "char" "str" "None" "Some" "Ok" "Err")
  )


(defvar rune--types-regexp
  (regexp-opt rune--types 'symbols))

;; detecting types like FooBar::qux, highlight FooBar
(defvar rune--dynamic-types-regexp
  "\\([A-Z][a-zA-Z0-9_]*\\)::")

(defvar rune--variables
  '("self" "super" "Self" "crate"))

(defvar rune--variables-regexp
  (regexp-opt rune--variables 'symbols))

(defvar rune--strings-regexp
  "\"[^\"]*\"")

;; like foobar::baz, highlight foobar, but not baz, also not parts of wrods
;; also not FooBar::baz
(defvar rune--path-matcher
  "\\([a-z_][a-z0-9_]*\\)::")

(setq rune-highlights
	  `((,rune--keywords-regexp . font-lock-keyword-face)
		(,rune--types-regexp . font-lock-type-face)
		(,rune--variables-regexp . font-lock-variable-name-face)
		(,rune--strings-regexp . font-lock-string-face)
		(,rune--path-matcher 1 font-lock-constant-face)
		(,rune--dynamic-types-regexp 1 font-lock-type-face)
		)
	  )

(defvar rune-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?% ?& ?| ?^ ?! ?< ?> ?~ ?@))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Angle brackets.  We suppress this with syntactic propertization
    ;; when needed
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23n"  table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)

    table)
  "Syntax definitions and helpers.")

(define-derived-mode rune-mode prog-mode "rune"
  "Major mode for rune files."
  :syntax-table rune-mode-syntax-table
  (setq font-lock-defaults '(rune-highlights))


  ;; Misc
  (setq-local comment-start "// ")
  (setq-local comment-end   "")


  (setq-local comment-start-skip "\\(?://[/!]*\\|/\\*[*!]?\\)[[:space:]]*")
  (setq-local paragraph-start
              (concat "[[:space:]]*\\(?:"
                      comment-start-skip
                      "\\|\\*/?[[:space:]]*\\|\\)$"))
  (setq-local indent-tabs-mode nil))

(add-to-list 'auto-mode-alist '("\\.rn" . rune-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(rune-mode . ("cargo" "run" "--" "lsp"))))

;; (use-package flymake-diagnostic-at-point)


(add-to-list
 'rune-mode-hook (lambda () (eglot-ensure)
				   (flymake-diagnostic-at-point-mode 1)
				   (corfu-mode 1)))
