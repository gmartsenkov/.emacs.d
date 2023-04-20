(use-package projectile
  :ensure t
  :init
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package consult :ensure t)
(use-package consult-lsp :ensure t)
(use-package anzu :ensure t :config (global-anzu-mode +1))
(use-package orderless :ensure t)
(use-package vertico
  :ensure t
  :init
  (setq completion-styles '(basic partial-completion orderless))
  (setq vertico-count 15)
  (vertico-mode)
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)
	      ("C-f" . vertico-exit)
	      :map minibuffer-local-map
	      ("M-h" . backward-kill-word)))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

(use-package rg :ensure t)
