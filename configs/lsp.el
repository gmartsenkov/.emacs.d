(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-clients-elixir-server-executable '("~/elixir-ls/release/language_server.sh"))
  (setq lsp-enable-file-watchers nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-lens-enable nil)
  :hook
  (rust-ts-mode . lsp-deferred)
  (ruby-ts-mode . lsp-deferred))
;; (use-package consult-lsp :ensure t)

;; (use-package eglot
;;   :ensure t
;;   :init
;;   (setq eglot-connect-timeout 60)
;;   :config
;;   (add-hook 'ruby-ts-mode-hook 'eglot-ensure)
;;   (add-hook 'rust-ts-mode-hook 'eglot-ensure))

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
  (add-hook 'ruby-ts-mode-hook 'company-mode)
  (add-hook 'go-mode-hook 'company-mode)
  (add-hook 'rust-ts-mode-hook 'company-mode)
  (add-hook 'clojure-mode-hook 'company-mode)
  (add-hook 'elixir-mode-hook 'company-mode)
  (add-hook 'inf-ruby-mode 'company-mode)
  (add-hook 'rspec-compilation-mode 'company-mode)
  (add-hook 'javascript-mode 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode))
