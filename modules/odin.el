(use-package odin-mode
  :straight (:host github :repo "mattt-b/odin-mode")
  :mode ("\\.odin\\'" . odin-mode)
  :hook (odin-mode . eglot-ensure))


(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(odin-mode . ("ols" "--stdio"))))
