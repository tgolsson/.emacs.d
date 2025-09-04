
(use-package copilot
  ;; :hook ((prog-mode . copilot-mode))
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :bind (:map copilot-mode-map (("<backtab>" . copilot-accept-completion))))
