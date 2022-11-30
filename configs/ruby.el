(use-package bundler
  :mode ("\\.rb\\'" . ruby-mode))

(use-package rspec-mode
  :mode ("\\.rb\\'" . ruby-mode)
  :config
  (setq rspec-primary-source-dirs '("app" "apps" "lib"))
  ;;(setq rspec-primary-source-dirs '("apps"))
  ) ;; When you've hit the breakpoint, hit C-x C-q to enable inf-ruby.

(use-package ruby-end
  :mode ("\\.rb\\'" . ruby-mode))

(use-package inf-ruby
  :mode ("\\.rb\\'" . ruby-mode)
  :init
  (add-hook 'rspec-mode-hook 'inf-ruby-switch-setup))

(use-package rubocop
  :mode ("\\.rb\\'" . ruby-mode))
