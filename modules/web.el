(defvar auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions, see `auto-mode-alist'
All elements of this alist are checked, meaning you can enable multiple minor modes for the same regexp.")

(defun enable-minor-mode-based-on-extension ()
  "Check file name against `auto-minor-mode-alist' to enable minor modes
the checking happens for all pairs in auto-minor-mode-alist"
  (when buffer-file-name
    (let ((name (file-name-sans-versions buffer-file-name))
          (remote-id (file-remote-p buffer-file-name))
          (case-fold-search auto-mode-case-fold)
          (alist auto-minor-mode-alist))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match-p (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))

(add-hook 'find-file-hook #'enable-minor-mode-based-on-extension)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :custom (lsp-clients-typescript-prefer-use-project-ts-server t)
  :hook ((typescript-mode . hs-minor-mode)
		 (typescript-mode . (lambda ()
							  (prettier-js-mode 1)))))

(use-package web-mode
  :mode ("\\.phtml\\'" "\\.html\\'")
  :config
  :init
  (use-package prettier-js
    :init
    (setq prettier-js-args '("--tab-width" "4" "--use-tabs" "true")))

  (use-package add-node-modules-path
    :hook web-mode)

  (setq indent-tabs-mode t
		web-mode-code-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-markup-indent-offset 4
        web-mode-script-padding 4))

(use-package svelte-mode
  :custom
  (svelte-basic-offset 4))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
			   '(svelte-mode . ("svelteserver" "--stdio"))))
