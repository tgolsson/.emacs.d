(use-package lisp-mode
  :straight (:type built-in)
  :mode (("\\.el\\'" . emacs-lisp-mode)
		 ("\\.lisp\\'" . lisp-mode))
  :bind (:map emacs-lisp-mode-map ("\r"
                                   . reindent-then-newline-and-indent))
  :config (add-hook 'emacs-lisp-mode-hook
                    (lambda ()
                       (make-local-variable 'completion-at-point-functions)
                       (setq completion-at-point-functions '(elisp-completion-at-point comint--complete-file-name-data)
                             comint-completion-addsuffix nil)

                       (eldoc-mode 1))))

(use-package sly
  :custom
  (inferior-lisp-program "sbcl")
  :config (add-hook 'sly-mode-hook
                    (lambda ()
                       (make-local-variable 'completion-at-point-functions))))
