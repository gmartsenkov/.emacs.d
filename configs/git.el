(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

; (use-package forge :ensure t :after magit)
(use-package git-link :ensure t)
