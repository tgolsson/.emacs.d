
(defun to/adjust-flycheck-automatic-syntax-eagerness ()
  "Adjust how often we check for errors based on if there are any.
This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
  (setq flycheck-idle-change-delay (if flycheck-current-errors 0.5 30.0)))

(defun flycheck-handle-idle-change ()
  "Handle an expired idle time since the last change.
This is an overwritten version of the original
flycheck-handle-idle-change, which removes the forced deferred.
Timers should only trigger inbetween commands in a single
threaded system and the forced deferred makes errors never show
up before you execute another command."
  (flycheck-clear-idle-change-timer)
  (flycheck-buffer-automatically 'idle-change))

(use-package flycheck
  :hook ((cc-mode rust-mode go-mode protobuf-mode) . flycheck-mode)
  :diminish t
  :commands flycheck-mode
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-disabled-checkers '(python-pylint))
  :init
  (defvar-local flycheck-local-checkers nil)
  (defun +flycheck-checker-get(fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)
  (use-package flycheck-clang-tidy
    :hook (cc-mode . flycheck-clang-tidy-setup)
    :after flycheck)
  :config
  (add-hook 'flycheck-after-syntax-check-hook 'to/adjust-flycheck-automatic-syntax-eagerness))


(flycheck-def-config-file-var typos-typos-toml typos
                              '("typos.toml"))

(flycheck-define-checker typos
  "A typo checker using typos-cli."
  :command ("typos" "--format" "brief" source
            (config-file "--config" typos-typos-toml))
  :error-patterns
    ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
  :modes (text-mode prog-mode markdown-mode rust-mode toml-mode python-mode))

(add-to-list 'flycheck-checkers 'typos t)
(flycheck-add-next-checker 'python-ruff 'typos)
(flycheck-add-next-checker 'python-ruff 'python-pyright)

(flycheck-def-config-file-var buf-buf-yaml buf '("buf.yaml"))

(flycheck-define-checker buf
  "A protobuf checker using buf."
  :command ("buf" "lint" (config-file "--config" buf-buf-yaml) "--error-format" "text" source-original)
  :error-patterns ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
  :modes (protobuf-mode)
  :working-directory (lambda (checker) (locate-dominating-file buffer-file-name "buf.yaml")))

(add-to-list 'flycheck-checkers 'buf t)

(defun @-flycheck-python-find-project-root (_checker)
  (let ((start (if buffer-file-name
                   (file-name-directory buffer-file-name)
                 default-directory)))

    (or (locate-dominating-file start "pants.toml")
		(flycheck-python-find-project-root _checker))))

(setf (flycheck-checker-get 'python-pyright 'working-directory) #'@-flycheck-python-find-project-root)

(use-package flymake-biome
  :straight (:host github :repo "erickgnavar/flymake-biome")
  :hook ((web-mode . flymake-biome-load)))
