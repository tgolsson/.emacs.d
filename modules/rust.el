(use-package rust-mode
  :mode "\\.rs\\'"
  :hook ((rust-mode . hs-minor-mode)
		 (rust-mode . rust-enable-format-on-save)
         (rust-mode . (lambda ()
						(eglot-ensure)
						(corfu-mode 1)
                        (flycheck-pos-tip-mode 0)
                        ;; (flycheck-inline-mode 0)
                        (set (make-local-variable 'compile-command) "cargo run")
                        (add-hook 'before-save-hook #'eglot-format-buffer t t))))
  :init
  (use-package cargo :hook (rust-mode . cargo-minor-mode)))
