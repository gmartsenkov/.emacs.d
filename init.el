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

(use-package flymake
  :ensure t
  :config
  (setq flymake-fringe-indicator-position nil))

(use-package ag :ensure t)
(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))
(use-package evil
  :after (projectile rspec-mode bundler perspective)
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>sr") 'anzu-query-replace-regexp)
  (evil-define-key 'normal 'global (kbd "<leader>bb") 'ivy-switch-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-this-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>p") 'projectile-command-map)
  (evil-define-key 'normal 'global (kbd "<leader>gg") 'magit)
  (evil-define-key 'normal 'global (kbd "<leader>gl") 'git-link)
  (evil-define-key 'normal 'global (kbd "<leader>SPC") 'projectile-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>c f") 'flymake-show-diagnostics-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>c d") 'lsp-find-definition)
  (evil-define-key 'normal 'global (kbd "<leader>c r") 'lsp-find-references)
  (evil-define-key 'normal 'global (kbd "<leader>c s") 'lsp-ivy-workspace-symbol)
  (evil-define-key 'normal 'global (kbd "<leader>TAB TAB") 'persp-switch)
  (evil-define-key 'normal 'global (kbd "<leader>TAB k") 'persp-kill)
  (evil-define-key 'normal 'global (kbd "<leader>TAB 1") (lambda () (interactive) (persp-switch-by-number 1)))
  (evil-define-key 'normal 'global (kbd "<leader>TAB 2") (lambda () (interactive) (persp-switch-by-number 2)))
  (evil-define-key 'normal 'global (kbd "<leader>TAB 3") (lambda () (interactive) (persp-switch-by-number 3)))
  (evil-define-key 'normal 'global (kbd "<leader>TAB 4") (lambda () (interactive) (persp-switch-by-number 4)))
  (evil-define-key 'normal 'global (kbd "<leader>TAB 5") (lambda () (interactive) (persp-switch-by-number 5)))
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>tt") 'rspec-toggle-spec-and-target)
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>tv") 'rspec-verify)
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>tc") 'rspec-verify-single)
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>ta") 'rspec-verify-all)
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>mp") 'rubocop-check-project)
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>mbi") 'bundle-install)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>mc") 'cider)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>tt") 'projectile-toggle-between-implementation-and-test)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>ta") 'cider-test-run-project-tests)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>tv") 'cider-test-run-test)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>eb") 'cider-eval-buffer)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>ee") 'cider-eval-last-sexp)
  (evil-define-key 'normal emacs-lisp-mode-map (kbd "<leader>eb") 'eval-buffer)
  (evil-define-key 'normal emacs-lisp-mode-map (kbd "<leader>ee") 'eval-last-sexp))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (ivy-mode))

(use-package clojure-mode :ensure t)
(use-package lsp-mode
  :ensure t
  :diminish lsp-mode
  :config
  (setq lsp-clients-elixir-server-executable '("~/elixir-ls/release/language_server.sh"))
  (setq lsp-enable-file-watchers nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  :init
  (add-hook 'elixir-mode-hook #'lsp)
  ;(add-hook 'clojure-mode-hook #'lsp)
  (add-hook 'ruby-mode-hook #'lsp))
(use-package lsp-ivy :ensure t)
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
  :config
  (setq company-tooltip-limit 10) 
  (setq company-idle-delay .1)
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  :init
  (add-hook 'ruby-mode-hook 'company-mode)
  (add-hook 'clojure-mode-hook 'company-mode)
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
;; (use-package spacemacs-theme
;;   :defer t
;;   :init
;;   (load-theme 'spacemacs-dark t))
(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-one t))

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-buffer-encoding nil)
  :init
  (doom-modeline-mode))

(use-package perspective :ensure t)
(use-package persp-projectile
  :after (perspective projectile)
  :ensure t
  :init
  (persp-mode))

(use-package git-link :ensure t)
(use-package git-gutter :ensure t)
(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :after git-gutter
  :demand fringe-helper
  :ensure t
  :init
  (add-hook 'text-mode-hook #'git-gutter-mode)
  (add-hook 'prog-mode-hook #'git-gutter-mode)
  (add-hook 'conf-mode-hook #'git-gutter-mode)
  :config
  ;; subtle diff indicators in the fringe
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center))

(use-package yasnippet
  :ensure t
  :init
  (add-hook 'ruby-mode-hook #'yas-minor-mode)
  (add-hook 'clojure-mode-hook #'yas-minor-mode)
  (add-hook 'elixir-mode-hook #'yas-minor-mode))
(use-package yasnippet-snippets :ensure t)

(use-package anzu
  :ensure t
  :init
  (add-hook 'text-mode-hook #'anzu-mode)
  (add-hook 'prog-mode-hook #'anzu-mode)
  (add-hook 'conf-mode-hook #'anzu-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(package-selected-packages
   '(ag anzu yasnippet-snippets yasnippet doom-themes cider :clojure-mode lsp-ivy git-gutter-fringe git-link perspective doom-modeline diminish simple-modeline spacemacs-theme rubocop rspec-mode bundler parseedn which-key ivy evil-collection evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
