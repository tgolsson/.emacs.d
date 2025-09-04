
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


(use-package compile-multi-all-the-icons
  :ensure t
  :after all-the-icons-completion
  :after compile-multi
  :demand t)

(defun @-pants-peek (filename-or-dir)
  ;; get list of all targets in dir
  (json-parse-string (shell-command-to-string (format "pants peek %s" filename-or-dir))
					 :array-type 'list))

(defun @-compile-multi-python-test-file (output address goals)
  "If the current file is a Python test file, adds a command to run the test-at-point."
  (if (and (buffer-file-name)
		   (string= (file-name-extension (buffer-file-name))  "py")
		   (cl-find "test" goals :test #'string=))
	  (let ((defun-name (save-excursion (beginning-of-defun)
										(python-info-current-defun))))

		(when (and (not (null defun-name)) (string-prefix-p "test_" defun-name))
		  (push `(,(format "test : current test (%s)"  defun-name) "pants" "--no-dynamic-ui" "test" ,address "--" "-k" ,defun-name) output)))
	output))

(defun @-compile-multi-pants-targets+ ()
  (let ((pants-output (@-pants-peek (or (buffer-file-name) dired-directory default-directory)))
		output)
	;; for stability reasons we sort the output by address. Not sure
	;; if multi-compile actually *does* care but it seems to be more
	;; stable?
	(cl-sort pants-output (lambda (a b)
							(string-lessp (gethash "address" a)
										  (gethash "address" b))))

	(dolist (target pants-output output)
	  (let ((address (gethash "address" target))
			(goals (gethash "goals" target)))

		(setf output (@-compile-multi-python-test-file output address goals))

		;; all known goals
		(dolist (goal goals)
		  (push `(,(format "%s : %s" goal address) "pants" "--no-dynamic-ui" ,goal ,address) output))

		;; pants doesn't report lint, fmt
		(dolist (goal (list "lint" "fmt" "fix"))
		  (cond
		   ((buffer-file-name) (let ((source (string-join (gethash "sources" target) " ")))
								 (push `(,(format "%s : %s" goal source) "pants" "--no-dynamic-ui" ,goal ,source) output)
								 ))
		   (dired-directory (push `(,(format "%s : here" goal) "pants" "--no-dynamic-ui" ,goal ,(format "%s:" dired-directory)) output)
							(push `(,(format "%s : recursive" goal) "pants" "--no-dynamic-ui" ,goal ,(format "%s::" dired-directory)) output))
		   (default-directory (push `(,(format "%s : here" goal) "pants" "--no-dynamic-ui" ,goal ,(format "%s:" default-directory)) output)
							 (push `(,(format "%s : recursive" goal) "pants" "--no-dynamic-ui" ,goal ,(format "%s::" default-directory)) output))))
		(when (buffer-file-name)
		  (let ((source (string-join (gethash "sources" target) " ")))
								 (push `(,(format "repl : %s" address) "pants" "--no-dynamic-ui repl" ,address) output)
								 ))
		(when (not (buffer-file-name))
		  (when-let ((root-dir (or dired-directory default-directory)))
			(dolist (goal (list "tailor" "update-build-files" "test"))
			  (push `(,(format "%s : here" goal) "pants" "--no-dynamic-ui" ,goal ,(format "%s:" root-dir)) output)
			  (push `(,(format "%s : recursive" goal) "pants" "--no-dynamic-ui" ,goal ,(format "%s::" root-dir)) output))))))))

(push `((file-exists-p "pants.toml") ,#'@-compile-multi-pants-targets+) compile-multi-config)

(use-package projection-multi
  :straight (:build (:not native-compile))
  :bind ( :map project-prefix-map
          ("RET" . projection-multi-compile)))

(use-package projection-multi-embark
  :after embark
  :after projection-multi
  :config (projection-multi-embark-setup-command-map))

(use-package projection-dape)
(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter))
