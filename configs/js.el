(use-package tide
  :ensure t
  :hook ((javascript-mode . tide-setup)
         (javascript-mode . tide-hl-identifier-mode)))

(use-package slim-mode
  :ensure t
  :mode ("\\.emblem\\'" . slim-mode))
