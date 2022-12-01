(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-clients-elixir-server-executable '("~/elixir-ls/release/language_server.sh"))
  (setq lsp-enable-file-watchers nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-lens-enable nil)
  :init
  (add-hook 'elixir-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'rust-mode-hook #'lsp-deferred)
  (add-hook 'ruby-mode-hook #'lsp-deferred))

(use-package company
  :ensure t
  :config
  (setq company-tooltip-limit 10)
  (setq company-idle-delay .1)
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (setq lsp-completion-provider :none)
  (setq company-backends '((:separate company-yasnippet company-tide company-capf company-files)))
  :init
  (add-hook 'ruby-mode-hook 'company-mode)
  (add-hook 'go-mode-hook 'company-mode)
  (add-hook 'rust-mode-hook 'company-mode)
  (add-hook 'clojure-mode-hook 'company-mode)
  (add-hook 'elixir-mode-hook 'company-mode)
  (add-hook 'inf-ruby-mode 'company-mode)
  (add-hook 'rspec-compilation-mode 'company-mode)
  (add-hook 'javascript-mode 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode))
