(setq gc-cons-threshold 500000000
      load-prefer-newer t
      package-enable-at-startup nil)

;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)


;;; Init bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(when (memq window-system '(w32))
  (setq native-comp-speed -1
		no-native-compile t))

;;; In order to still use `package-list-packages' to find new toys.
(require 'use-package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;;; Use utf-8 everywhere it makes sense.
(define-coding-system-alias 'cp65001 'utf-8)
(prefer-coding-system 'utf-8) ;; fixes some packages containing non-iso-latin characets
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(use-package benchmark-init
  :demand t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(setq doom-gc-cons-threshold (* 16 1024 1024)) ;; 16 MB
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold doom-gc-cons-threshold
                  gc-cons-percentage 0.1)))

(defun doom-defer-garbage-collection-h ()
  "Don't do garbage collection during startup."
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  "Defer it so that commands launched immediately after will enjoy the benefits."
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold doom-gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

(defmacro to/disable (name) `(when (fboundp ',name) (,name -1)))
(defun risky-local-variable-p (sym &optional _ignored) "Noop" nil)

(use-package emacs
  :demand t
  :config
  (when (not (string-equal (window-system) "w32"))
    (use-package server
	  :demand t
	  :config
	  (defun server-ensure-safe-dir (dir) "Noop" t)
	  (unless (file-exists-p server-socket-dir)
	    (make-directory server-socket-dir))
	  (unless (server-running-p)
        (server-start))))

  :init
  (global-so-long-mode 1)
  (push '(fullscreen . maximized) default-frame-alist)
  (when window-system (set-frame-font (cond
                                       ((string-equal (window-system) "w32") "Iosevka Term Medium 13")
                                       (t "Iosevka Term Medium Extended 18"))))
  (set-fringe-mode 20)
  (setq-default custom-safe-themes t
                transient-mark-mode t
				tab-width 4)
  (setq backup-by-copying t
		completion-cycle-threshold 3
		completions-detailed t
		read-extended-command-predicate #'command-completion-default-include-p
		tab-always-indent 'complete
		read-process-output-max (* 1024 1024)
		bidi-paragraph-direction 'left-to-right
		bidi-inhibit-bpa t
		apropos-do-all t
		auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
		backup-directory-alist `((".*" . ,temporary-file-directory))
		bookmark-default-file  (concat user-emacs-directory "bookmarks.em")
		bookmark-save-flag 1
		compilation-scroll-output 'first-error
		confirm-kill-emacs 'y-or-n-p
		cursor-type t
		delete-selection-mode t
		echo-keystrokes 0.1
		ediff-diff-options "-w"
		ediff-split-window-function 'split-window-horizontally
		ediff-window-setup-function 'ediff-setup-windows-plain
		fill-column 80
		frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))
		history-length 1000
		indent-tabs-mode nil
		inhibit-splash-screen t
		inhibit-startup-message t
		large-file-warning-threshold 100000000
		locale-coding-system 'utf-8
		mouse-yank-at-point t
		require-final-newline t
		ring-bell-function 'ignore
		save-interprogram-paste-before-kill t
		scroll-conservatively 100000
		scroll-margin 0
		scroll-preserve-screen-position 0
		user-full-name "Tom Solberg"
		user-mail-address "me@sbg.dev"
		vc-make-backup-files t
		x-select-enable-clipboard t
		x-select-enable-primary t)



  (fset 'yes-or-no-p 'y-or-n-p)
  (mapc 'frame-set-background-mode (frame-list))

  (transient-mark-mode t)
  (make-variable-buffer-local 'transient-mark-mode)
  (put 'transient-mark-mode 'permanent-local t)
  (electric-indent-mode nil)

  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package no-littering)

(dolist (name (list "env" "cc" "consult" "corfu" "flycheck"
			   "go" "interactive" "lisp" "magit" "odin" "projectile"
			   "python" "rune" "rust" "treemacs" "visual" "web"))
  (load (expand-file-name (format "modules/%s.el" name) user-emacs-directory) nil 'nomessage))

(when (memq window-system '(w32))
  (load (expand-file-name "modules/windows.el" user-emacs-directory)  nil 'nomessage))

(use-package exec-path-from-shell
  :when (memq window-system '(mac ns x))
  :init (setq exec-path-from-shell-arguments nil)
  :config (exec-path-from-shell-initialize))

(use-package savehist
  :init (savehist-mode +1)
  :custom
  (savehist-additional-variables '(search-ring regexp-search-ring))
  (savehist-autosave-interval 60))

(use-package recentf
  :custom
  (recentf-max-saved-items 2000)
  :init
  (recentf-mode +1)
  :config
  (setq recentf-exclude (append recentf-exclude '("/elpa/"
                                                  "company-statistics-cache.el"
                                                  "bookmarks.em" ".mc-lists.el"
                                                  "custom.el"
                                                  no-littering-etc-directory
                                                  no-littering-var-directory))))


(use-package saveplace
  :init (setq save-place-file (concat user-emacs-directory "places"))
  :config (save-place-mode))

(use-package uniquify
  :straight (:type built-in)
  :config (setq uniquify-buffer-name-style 'forward))


(defun ts/eglot-mode-setup-completion ()
  (setf (alist-get 'styles (alist-get 'eglot-capf completion-category-defaults))
        '(orderless))) ;; Configure orderless


(use-package eglot
  :ensure t
  :hook (((rust-mode go-mode python-mode web-mode c-mode web-mode) . eglot-ensure))
  :bind-keymap ("C-c l" . eglot-mode-map)
  :bind (:map eglot-mode-map
			  ("C-c l a" . eglot-code-actions)
			  ("C-c l r" . eglot-rename)
			  ("C-c l h" . eldoc)
			  ("C-c l e" . eglot-code-action-extract)
			  ("C-c l w" . eglot-code-action-rewrite)
			  ("C-c l i" . eglot-code-action-inline)
			  ("C-c l f t" . eglot-find-typeDefinition)
			  ("C-c l f d" . eglot-find-declaration)
			  ("C-c l f i" . eglot-find-implementation)
			  ;; sometimes ionide acts up
			  ("C-c l R" . eglot-reconnect))
  :custom
  (eglot-events-buffer-config '(:size 0))
  (eglot-report-progress t)
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref t)
  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((c-mode c++-mode)
                   . ("clangd"
                      "-j=4"
                      "--log=error"
                      "--malloc-trim"
                      "--background-index"
                      "--clang-tidy"
                      "--cross-file-rename"
                      "--completion-style=detailed"
                      "--pch-storage=memory"
                      "--header-insertion=never"
                      "--header-insertion-decorators=0"))))
  (setq-default eglot-workspace-configuration
				'(:pylsp (
						  :plugins (
									:mccabe (:enabled nil)
									:pycodestyle (:enabled nil)
									:floe (:enabled t)))
						 :gopls (
								 :build.directoryFilters ["-bazel-bin" "-bazel-out" "-bazel-testlogs" "-bazel-mercari-feature-flags"]
								 :formatting.gofumpt t
								 :completion.usePlaceholders t
								 :diagnostic.vulncheck "Imports"
								 :diagnostic.staticcheck t))))

(use-package wgrep :demand t)
(use-package editorconfig)
(use-package copy-as-format)
(use-package gif-screencast)
(use-package dockerfile-mode :mode ("Dockerfile"))
(use-package bazel)
(use-package glsl-mode :mode ("\\.glsl\\'"))
(use-package toml-mode)
(use-package graphviz-dot-mode)
(use-package yaml-mode :mode ("\\.yaml$" "\\.yml$") :custom (yaml-indent-offset 2))
(use-package dumb-jump)

(use-package jsonnet-mode
  :config
  :hook (jsonnet-mode . (lambda ()
						  (progn
							(indent-tabs-mode -1)
							(setq-local indent-tabs-mode nil
										tab-width 2)))))


(use-package comint
  :straight (:type built-in)
  :init
  (defun to/preoutput-turn-buffer-read-only (text)
    (propertize text 'read-only t))
  :custom
  (comint-prompt-readonly t)
  (comint-buffer-maximum-size 20000 "Increase comint buffer size.")
  (comint-prompt-read-only t "Make the prompt read only.")
  :config
  (add-hook 'comint-preoutput-filter-functions
            'to/preoutput-turn-buffer-read-only))

(global-subword-mode +1)
(use-package elec-pair
  :commands elec-pair-mode
  :hook (prog-mode . electric-pair-mode)
  :demand t
  :config (electric-pair-mode +1))

(use-package multiple-cursors
  :bind (("M-ö" .    mc/edit-lines)
         ("C-M-ö" .  mc/edit-ends-of-lines)
         ("M-Ö" .    mc/edit-beginnings-of-lines)
         ("M-ä" .    mc/mark-all-dwim)
         ("C-<" .    mc/mark-previous-like-this)
         ("C->" .    mc/mark-next-like-this)
         ("C-S-ä" .  mc/mark-more-like-this-extended)
         ("M-å" .    mc/mark-all-in-region))
  :config
  (unsupported-cmd isearch-forward-use-region ".")
  (unsupported-cmd isearch-backward-use-region "."))

(use-package which-key
  :init (which-key-mode 1)
  :config (setq which-key-idle-delay 0.3))


(use-package vterm :defer t)
(use-package vterm-toggle
  :defer t
  :bind
  ("C-'" . vterm-toggle)
  :custom
  (vterm-toggle-scope 'project)
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-cd-auto-create-buffer nil)
  :config
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(use-package dired+)

  ;; :config
  ;; (push `(t
  ;;         ("Fallback:Edit command" . ,#'(lambda ()
  ;;                                         (call-interactively 'compile))))
  ;;       compile-multi-config)
  ;; (push '((file-exists-p "Makefile")
  ;;       ("make:build" . "make build")
  ;;       ("make:test" . "make test")
  ;;       ("make:all" . "make all"))
  ;;     compile-multi-config)


(use-package compile-multi-all-the-icons
  :ensure t
  :after all-the-icons-completion
  :after compile-multi
  :demand t)

(bind-key "M-Q" 'delete-trailing-whitespace)
(global-set-key (kbd "C-x C-b")              'ibuffer)
(global-set-key (kbd "M-z")                  'zap-up-to-char)

(global-set-key (kbd "C-s")                  'isearch-forward-regexp)
(global-set-key (kbd "C-r")                  'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")                'isearch-forward)
(global-set-key (kbd "C-M-r")                'isearch-backward)
(global-set-key (kbd "M-j")  (lambda () (interactive) (join-line -1)))

(global-set-key (kbd "C-x C-r")              'rename-current-buffer-file)

;; Transpose
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l")                'transpose-lines)
(global-set-key (kbd "M-t w")                'transpose-words)
(global-set-key (kbd "M-t s")                'transpose-sexps)
(global-set-key (kbd "M-t p")                'transpose-params)

(global-set-key (kbd "C-c u")                'uncomment-region)
(global-set-key (kbd "C-c c")                'comment-or-uncomment-region)

(global-set-key (kbd "M-o")                'copy-selected-text)

(global-set-key (kbd "M-p")                  'backward-paragraph)
(global-set-key (kbd "M-n")                  'forward-paragraph)

(global-set-key (kbd "C-x C-=") 'acg/zoom-frame)
(global-set-key (kbd "C-x C--") 'acg/zoom-frame-out)
(global-set-key (kbd "<C-wheel-up>") 'acg/zoom-frame)
(global-set-key (kbd "<C-wheel-down>") 'acg/zoom-frame-out)
(define-key global-map (kbd "M-o") 'wsl-copy-region-to-clipboard)
