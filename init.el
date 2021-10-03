(load-file "~/.emacs.d/defaults.el")

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t
  :init
  (diminish 'evil-collection-unimpaired-mode)
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode))

(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))
(use-package evil
  :after (projectile rspec-mode bundler)
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>bb") 'ivy-switch-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-this-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>p") 'projectile-command-map)
  (evil-define-key 'normal 'global (kbd "<leader>gg") 'magit)
  (evil-define-key 'normal 'global (kbd "<leader>SPC") 'projectile-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>mtt") 'projectile-toggle-between-implementation-and-test)
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>mtv") 'rspec-verify)
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>mtc") 'rspec-verify-single)
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>mta") 'rspec-verify-all)
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>mp") 'rubocop-check-project)
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>mbi") 'bundle-install))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (ivy-mode))

(use-package lsp-mode
  :ensure t
  :diminish lsp-mode
  :config
  (setq lsp-clients-elixir-server-executable '("~/elixir-ls/release/language_server.sh"))
  (setq lsp-enable-file-watchers nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  :init
  (add-hook 'elixir-mode-hook #'lsp)
  (add-hook 'ruby-mode-hook #'lsp))

(use-package evil-collection
  :after (evil ivy)
  :ensure t
  :custom (evil-collection-setup-minibuffer t)
  :init
  (evil-collection-init)
  (evil-collection-define-key 'insert 'ivy-minibuffer-map
    [backspace] 'ivy-backward-delete-char
    (kbd "C-j") 'ivy-next-line
    (kbd "C-k") 'ivy-previous-line))

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'company-mode))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(use-package which-key
  :diminish which-key-mode
  :ensure t
  :init
  (which-key-mode))

(use-package cider :ensure t)
(use-package bundler :ensure t)
(use-package rspec-mode
  :diminish rspec-mode
  :ensure t) ;; When you've hit the breakpoint, hit C-x C-q to enable inf-ruby.
(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'rspec-mode-hook 'inf-ruby-switch-setup))
(use-package rubocop :ensure t)
(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark t))

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-buffer-encoding nil)
  :init
  (doom-modeline-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(package-selected-packages
   '(doom-modeline diminish simple-modeline spacemacs-theme rubocop rspec-mode bundler parseedn which-key cider ivy evil-collection evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
