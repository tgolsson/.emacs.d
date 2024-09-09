(use-package corfu
  :straight (:files ("*" (:exclude ".git") "extensions/corfu-popupinfo.el"))
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.3)         ;; After 0.0 seconds
  (corfu-auto-prefix 3)          ;; And a single character
  (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary nil)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-echo-documentation t)   ;; Show documentation in the echo area
  (corfu-popupinfo-delay 1)
  :hook ((emacs-lisp-mode . corfu-mode)
		 (sly-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)
		 (python-mode . corfu-mode)
		 (corfu-mode . corfu-popupinfo-mode))
  :init
  (require 'corfu-popupinfo)
  (use-package kind-icon
	:after corfu
	:custom
	(kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly

	:config
	(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  :bind (:map corfu-map ("M-d" . corfu-popupinfo--toggle)))

(use-package corfu-terminal
  :init
  (unless (display-graphic-p)
	(corfu-terminal-mode +1)))
