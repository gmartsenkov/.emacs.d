(use-package bundler :ensure t)
(use-package rspec-mode
  :ensure t
  :config
  (setq rspec-primary-source-dirs '("app" "apps" "lib"))
  ;;(setq rspec-primary-source-dirs '("apps"))
  ) ;; When you've hit the breakpoint, hit C-x C-q to enable inf-ruby.
(use-package ruby-end :ensure t)
(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'rspec-mode-hook 'inf-ruby-switch-setup))
(use-package rubocop :ensure t)
