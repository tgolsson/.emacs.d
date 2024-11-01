(defvar @-dotenv-file-name ".env" "The name of the .env file.")
(defun @-find-env-file ()
  "Find the closest .env file in the directory hierarchy."
  (let* ((env-file-directory (locate-dominating-file "." @-dotenv-file-name))
         (file-name (concat env-file-directory @-dotenv-file-name)))
    (when (file-exists-p file-name)
      file-name)))

(defvar @-relative-path-env-vars '("WORKON_HOME" "PYTHONPATH"))

(defun @-replace-dot-prefix (s parent)
  "Replaces ./ in given s with `parent`"
  (or (and (s-prefix? "./" s)
		   (string-join (list parent (string-remove-prefix "." s))))
	  s))

(defun @-process-var (value root-path)
  (when value
	(let ((list (s-split ":" value)) out)
	  (dolist (s list out)
		(setf out (append out (list (@-replace-dot-prefix s root-path)))))
	  (s-join ":" out))))

(defun @-fixup-relative-paths (root-path)
  (dolist (var @-relative-path-env-vars)
	(setenv var (@-process-var (getenv var) root-path))))

(defun @-set-project-env ()
  "Export all environment variables in the closest .env file."
  (make-local-variable 'process-environment)
  (let ((env-file (@-find-env-file)))
    (when env-file
      (load-env-vars env-file)
	  (@-fixup-relative-paths (f-parent env-file)))))

(use-package load-env-vars)
(add-hook 'find-file-hook #'@-set-project-env)
(add-hook 'comint-exec-hook #'@-set-project-env)
(add-hook 'vterm-mode-hook #'@-set-project-env)
(add-hook 'vterm-mode-hook #'@-set-project-env)
