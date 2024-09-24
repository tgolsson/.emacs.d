(use-package delight)

(use-package all-the-icons)
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
		doom-themes-padded-modeline t)
  (load-theme 'doom-one-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config))

(set-face-attribute 'vertico-current nil :background "#dfdfdf")

(use-package doom-modeline
  :custom
  (doom-modeline-lsp t)
  (doom-modeline-lsp-icon t)
  (doom-modeline-github t)
  :init
  (doom-modeline-mode 1)
  (set-face-attribute 'mode-line nil :underline nil)
  :config
  (set-face-attribute 'mode-line nil :underline nil)
										;   (when (not (memq window-system '(w32))) (nerd-icons-install-fonts t))
  )

(use-package rainbow-mode :hook prog-mode)
(use-package smooth-scrolling)

(use-package hl-line
  :demand t
  :init (global-hl-line-mode +1))

(use-package paren
  :commands show-paren-mode
  :init (show-paren-mode +1))

(use-package global-display-line-numbers-mode
  :straight (:type built-in)
  :commands global-display-line-numbers-mode
  :custom
  (display-line-numbers-major-tick 50)
  (display-line-numbers-minor-tick 0)
  :init
  (global-display-line-numbers-mode 1)
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  treemacs-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el" :files ("*.el"))
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  (global-ligature-mode t))

(use-package solaire-mode  :init (solaire-global-mode +1)
  (setq solaire-mode-auto-swap-bg t))

(use-package hl-todo
  :bind (("C-c n" . #'hl-todo-next)
		 ("C-c o" . #'hl-todo-rgrep)
		 ("C-c i"  . #'hl-todo-insert))
  :init
  (setq hl-todo-keyword-faces
      '(("TODO"   . "green")
        ("FIXME"  . "turquoise")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF")
		("BUG"   . "red")))
  (global-hl-todo-mode))

;FIXME
