(use-package cc-mode
  :defer t
  :mode (("\\.h\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.cxx\\'" . c++-mode)
         ("\\.c\\'" . c-mode))
  :bind (:map c-mode-base-map ("RET" . newline-and-indent))
  :init
  (setq-default c-default-style "linux"
                c-basic-offset 4)
  :config
  (c-set-offset 'inline-open 0)
  (c-set-offset 'innamespace 0)
  (c-set-offset 'topmost-intro 0)
  (c-set-offset 'comment-intro 0)
  (c-set-offset 'member-init-intro 0)

  (electric-pair-mode 1)
  (add-hook 'before-save-hook 'clang-format-buffer t t))

;;
;; irony-mode
;;
(use-package irony
  :commands irony-mode
  :defer t
  :config
  (irony-cdb-autosetup-compile-options))

;; Redefinition of function from gud.el
(defun gud-find-expr (&rest args)
  (let ((expr (if (and transient-mark-mode mark-active)
                  (buffer-substring (region-beginning) (region-end))
                (apply gud-find-expr-function args))))
    (save-match-data
      (if (string-match "\n" expr)
          (error "Expression must not include a newline"))
      (with-current-buffer gud-comint-buffer
        (save-excursion
          (goto-char (process-mark (get-buffer-process gud-comint-buffer)))
          (forward-line 0)
          (when (looking-at comint-prompt-regexp)
            (set-marker gud-delete-prompt-marker (point))
            (set-marker-insertion-type gud-delete-prompt-marker t))
          (unless (eq (buffer-local-value 'gud-minor-mode gud-comint-buffer)
                      'jdb)
            (message (concat expr " = "))))))
    expr))

(use-package asm-mode :mode (("\\.tmpli\\'" . asm-mode)))
