;;
;; magit
;;
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status))
  :bind (:map magit-status-mode-map ( "q" . magit-quit-session))
  :init
  ;; (use-package magit-filenotify
  ;;   :if (not (memq window-system '(w32)))
  ;;   :hook (magit-status-mode . magit-filenotify-mode))
  ;; (use-package magit-todos
  ;;   :hook (magit-status-mode . magit-todos-mode))
  :config
  (setq magit-todos-nice nil)
  (magit-auto-revert-mode -1)
  (custom-set-faces '(magit-diff-added ((t (:background "black" :foreground "green3"))))
                    '(magit-diff-removed ((t (:background "black" :foreground "red3"))))))

(use-package git-link)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(add-hook 'git-commit-mode-hook (lambda () (interactive "") (set-fill-column 72)))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))
