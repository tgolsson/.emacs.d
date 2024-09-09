
(use-package projectile
  :diminish t
  :config (projectile-mode)
  :defer 10
  :custom ((projectile-completion-system 'default))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (use-package ibuffer-projectile
    :hook (ibuffer . (lambda ()
                       (ibuffer-projectile-set-filter-groups)
                       (unless (eq ibuffer-sorting-mode 'alphabetic)
                         (ibuffer-do-sort-by-alphabetic)))))

  ;; (use-package counsel-projectile
  ;;   :after projectile
  ;;   :config (counsel-projectile-mode))

  :config
  (setq projectile-switch-project-action 'magit-status
        projectile-globally-ignored-directories (append '("*__pycache__*" "*.egg-info") projectile-globally-ignored-directories)
        projectile-globally-ignored-file-suffixes (append '(".pyc") projectile-globally-ignored-file-suffixes)
        projectile-indexing-method 'alien
        projectile-enable-caching 't
        projectile-git-command "fd . -0"
        projectile-git-submodule-command "git submodule --quiet foreach 'echo $path'")
  (projectile-mode +1)
  (projectile-global-mode 1))
