
(use-package go-mode
  :custom
  (eglot-go-directory-filters ["-bazel-src" "-bazel-bin" "-bazel-out" "-bazel-testlogs"])
  :mode "\\.go\\'"
  :init
  (use-package flycheck-golangci-lint :commands flycheck-golangci-lint-setup)
  ;; (use-package go-projectile :commands go-projectile-tools-add-path :hook (go-mode . go-projectile-set-gopath))
  :config
  (defun to/my-go-mode ()
    ;; (define-key go-mode-map (kbd "C-c C-e") #'go-gopath-set-gopath)
    ;; (add-hook 'before-save-hook 'gofmt-before-save)
    ;; (go-eldoc-setup)
    (go-projectile-tools-add-path)
    (flycheck-golangci-lint-setup)
    (flycheck-pos-tip-mode 1)
    (add-hook 'before-save-hook 'gofmt-before-save nil t))
  (add-hook 'go-mode-hook 'to/my-go-mode))

(defun buf-fmt-buffer ()
  "use shell command to format buffer or region via buf"
  (interactive)
  (let ((command (concat "buf format " (buffer-file-name) " -w")))
	(message command)
	(shell-command command))
  (revert-buffer-quick nil))

(use-package protobuf-mode
  :mode "\\.proto\\'"
  :hook (protobuf-mode . (lambda ()
                           (c-add-style "my-style" my-protobuf-style t)
						   (add-hook 'after-save-hook 'buf-fmt-buffer nil t)))
  :init
  (defconst my-protobuf-style
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil))))
