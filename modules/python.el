(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . eglot-ensure)
  :custom
  (dap-python-debugger 'debugpy)
  :config
  (setq python-indent-offset 4
        python-environment-virtualenv '("virtualenv" "-p" "python3" "--system-site-packages" "--quiet")
        flycheck-python-flake8-executable "/home/tgolsson/anaconda3/envs/py38/bin/python3.8")

  :init
  (use-package pyvenv
    :after python-mode
    :init
    (setenv "WORKON_HOME" (expand-file-name "~/anaconda3/envs" ))
    :config
    (pyvenv-mode 1))

  (use-package python-black
    :demand t
    :after python
    :hook (python-mode . python-black-on-save-mode))

  (use-package python-isort
    :demand t
    :after python
    :hook (python-mode . python-isort-on-save-mode))

  :config
  (require 'dap-python)
  (modify-syntax-entry ?_  "_")
  (local-set-key (kbd "M-.") 'jedi:goto-definition)
  (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker))

(defun @-default-directory-project-root (orig-fun &rest args)
  "Overrides the default-directory with the project root if possible."
  (let ((default-directory (or (and (project-current) (project-root (project-current))) default-directory)))
	(apply orig-fun args)))

(advice-add 'run-python :around #'@-default-directory-project-root)
