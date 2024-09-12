
;; (use-package projectile
;;   :diminish t
;;   :config (projectile-mode)
;;   :defer 10
;;   :custom ((projectile-completion-system 'default))
;;   :bind-keymap ("C-c p" . projectile-command-map)
;;   :init
;;   (use-package ibuffer-projectile
;;     :hook (ibuffer . (lambda ()
;;                        (ibuffer-projectile-set-filter-groups)
;;                        (unless (eq ibuffer-sorting-mode 'alphabetic)
;;                          (ibuffer-do-sort-by-alphabetic)))))

;;   ;; (use-package counsel-projectile
;;   ;;   :after projectile
;;   ;;   :config (counsel-projectile-mode))

;;   :config
;;   (setq projectile-switch-project-action 'magit-status
;;         projectile-globally-ignored-directories (append '("*__pycache__*" "*.egg-info") projectile-globally-ignored-directories)
;;         projectile-globally-ignored-file-suffixes (append '(".pyc") projectile-globally-ignored-file-suffixes)
;;         projectile-indexing-method 'alien
;;         projectile-enable-caching 't
;;         projectile-git-command "fd . -0"
;;         projectile-git-submodule-command "git submodule --quiet foreach 'echo $path'")
;;   (projectile-mode +1)
;;   (projectile-global-mode 1))

(use-package projection
  :hook (after-init . global-projection-hook-mode)
  :straight (:build (:not native-compile))
  :config
  (with-eval-after-load 'project
	(require 'projection))
  :config
  :bind-keymap ("C-x P" . projection-map))


(use-package compile-multi
  :commands compile-multi
  :straight (:build (:not native-compile))
  :bind
  (("<f9>" .   'mxl/compile-multi-recompile)
   ("<f10>" .   'ts/compile-multi-current-dir)
   ("C-<f9>" . 'compile-multi))
  :init
  (setq compile-multi-config nil)
  (setq compile-multi-default-directory (lambda () (project-root (project-current))))

  (defun mxl/compile-multi-recompile ()
    (interactive)
    (if (and compile-command
             (equal compilation-directory
                    (funcall compile-multi-default-directory)))
        (recompile)
      (compile-multi)))

  (defun ts/compile-multi-current-dir()
    (interactive)
	(let ((compile-multi-default-directory (lambda () default-directory)))
	  (compile-multi))))


(use-package projection-multi
  :straight (:build (:not native-compile))
  :bind ( :map project-prefix-map
          ("RET" . projection-multi-compile)))

(use-package projection-multi-embark
  :after embark
  :after projection-multi
  :config (projection-multi-embark-setup-command-map))

(use-package projection-dape)
