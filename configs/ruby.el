(use-package bundler :ensure t)

(use-package rspec-mode
  :ensure t
  :config
  (setq rspec-primary-source-dirs '("app" "apps" "lib"))
  ;;(setq rspec-primary-source-dirs '("apps"))
  ) ;; When you've hit the breakpoint, hit C-x C-q to enable inf-ruby.

(use-package ruby-end :ensure t)
(use-package inf-ruby :ensure t
  :config
  (inf-ruby-enable-auto-breakpoint))
(use-package rubocop :ensure t)

(setq ruby-deep-indent-paren nil)
(setq ruby-method-call-indent nil)
(setq ruby-after-operator-indent nil)
(setq ruby-parenless-call-arguments-indent nil)
(setq ruby-method-params-indent 0)
(setq ruby-block-indent nil)
