(use-package projectile
  :ensure t
  :init
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)
	      ("C-f" . vertico-exit)
	      :map minibuffer-local-map
	      ("M-h" . backward-kill-word)))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))
