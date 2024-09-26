(defvar @-dotenv-file-name ".env" "The name of the .env file.")
(defun @-find-env-file ()
  "Find the closest .env file in the directory hierarchy."
  (let* ((env-file-directory (locate-dominating-file "." @-dotenv-file-name))
         (file-name (concat env-file-directory @-dotenv-file-name)))
    (when (file-exists-p file-name)
      file-name)))

(defun @-set-project-env ()
  "Export all environment variables in the closest .env file."
  (make-local-variable 'process-environment)
  (let ((env-file (@-find-env-file)))

    (when env-file
      (load-env-vars env-file))))

(use-package load-env-vars)
(add-hook 'find-file-hook #'@-set-project-env)
(add-hook 'comint-exec-hook #'@-set-project-env)
(add-hook 'vterm-mode-hook #'@-set-project-env)
