(use-package tide
  :ensure t
  :after (company)
  :mode ("\\.jsx\\'" . js-jsx-mode)
  :config
  (setq typescript-ts-mode-indent-offset 2
        js-indent-level 2
        js-jsx-indent-level 2)
  :hook ((typescript-ts-mode . tide-setup)
         (js-mode . tide-setup)
         (js-jsx-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode)))
         ;;(before-save . tide-format-before-save))

(use-package slim-mode
  :ensure t
  :mode ("\\.emblem\\'" . slim-mode))
