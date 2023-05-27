(use-package cider :ensure t)
(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'flycheck-mode))
