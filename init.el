(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(load-file "~/.emacs.d/common.el")
(load-theme 'doom-gruvbox t)

;; MacOS Path Fix
(use-package exec-path-from-shell :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; UI
(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))

;; DASHBOARD
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq dashboard-center-content t)
(setq dashboard-startup-banner '2)
(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (bookmarks . 5)
                        (agenda . 5)))
                        
;; PROJECTILE
(use-package projectile :ensure t)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'ivy)

;; Company
(use-package company :ensure t)
(setq company-tooltip-limit 10) 
(setq company-idle-delay .1)
(setq company-tooltip-align-annotations t)
(add-hook 'after-init-hook 'global-company-mode)

;; Flycheck
(use-package flycheck :ensure t)
(global-flycheck-mode t)

;; IVY
(use-package ivy :ensure t)
(ivy-mode 1)

;; RUBY
(use-package ruby-end :ensure t)
(use-package rspec-mode :ensure t)
(setq ruby-indent-level 2)
(setq rspec-primary-source-dirs '("app"))

;; WHICH-KEY
(use-package which-key :ensure t)
(which-key-mode)

;; ACE WINDOWS
(use-package ace-window :ensure t)
(global-set-key (kbd "C-z") 'ace-window)

;; Magit AND OTHER GIT STUFF
(use-package magit :ensure t)
(use-package forge :ensure t)
(setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
(use-package diff-hl :ensure t)
(global-diff-hl-mode)
(define-key global-map (kbd "C-x g") 'magit)

;; Elixir
(use-package elixir-mode :ensure t)
(use-package alchemist :ensure t)

;; Yasnippets
(use-package yasnippet :ensure t)
(use-package yasnippet-snippets :ensure t)
(yas-global-mode 1)

;; Take care of trailing whitespaces
(use-package ws-butler :ensure t)
(add-hook 'prog-mode-hook #'ws-butler-mode)

;; expand-region
(use-package expand-region :ensure t)
(global-set-key (kbd "C-=") 'er/expand-region)

;; ace-jump-mode
(use-package ace-jump-mode :ensure t)
(define-key global-map (kbd "C-c ,") 'ace-jump-mode)

;; JS
(use-package js2-mode :ensure t)
(setq-default js2-basic-offset 2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))

;;ag.el
(use-package ag :ensure t)

;;anzu.el
(use-package anzu :ensure t)
(global-anzu-mode +1)

;; crystal
(use-package crystal-mode :ensure t)
(autoload 'crystal-mode "crystal-mode" "Major mode for crystal files" t)
(add-to-list 'auto-mode-alist '("\\.cr$" . crystal-mode))
(add-to-list 'interpreter-mode-alist '("crystal" . crystal-mode))

;; MARKDOWN
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
