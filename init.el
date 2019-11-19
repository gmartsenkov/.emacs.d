(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package doom-themes :ensure t)

(load-theme 'doom-dracula t)
(load-file "~/.emacs.d/common.el")

;; MacOS Path Fix
(use-package exec-path-from-shell :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; UI
(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))
(setq doom-modeline-buffer-encoding nil)

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
(setq doom-modeline-buffer-encoding nil)

;; Flycheck
(use-package flycheck :ensure t)
(global-flycheck-mode t)

;; IVY
(use-package ivy :ensure t)
(use-package swiper :ensure t)
(global-set-key (kbd "C-s") 'swiper-isearch)
(ivy-mode 1)

(use-package all-the-icons :ensure t)
(use-package ivy-rich
  :ensure t
  :after ivy)

(defun ivy-rich-switch-buffer-icon (candidate)
  (with-current-buffer
      (get-buffer candidate)
    (let ((icon (all-the-icons-icon-for-mode major-mode)))
      (if (symbolp icon)
	  (all-the-icons-icon-for-mode 'fundamental-mode)
	icon))))

(setq ivy-rich-display-transformers-list
      '(ivy-switch-buffer
        (:columns
         ((ivy-rich-switch-buffer-icon :width 2)
          (ivy-rich-candidate (:width 20))
          (ivy-rich-switch-buffer-size (:width 0))
          (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
          (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
          (ivy-rich-switch-buffer-project (:width 15 :face success))
          (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
         :predicate
         (lambda (cand) (get-buffer cand)))))
(setq ivy-rich-path-style 'abbrev)
(ivy-rich-mode 1)

;; RUBY
;;(use-package ruby-end :ensure t)
(use-package rspec-mode :ensure t)
(use-package rubocop :ensure t)
(use-package inf-ruby :ensure t)
(add-hook 'ruby-mode-hook #'rubocop-mode)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(setq ruby-indent-level 2)
(setq rspec-primary-source-dirs '("app"))
(setq ruby-insert-encoding-magic-comment nil)

;; WHICH-KEY
;;(use-package which-key :ensure t)
;;(which-key-mode)

;; ACE WINDOWS
(use-package ace-window :ensure t)
(global-set-key (kbd "C-z") 'ace-window)

;; Magit AND OTHER GIT STUFF
(use-package magit :ensure t)
;;(use-package forge :ensure t)
(setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
(define-key global-map (kbd "C-x g") 'magit)

;;(use-package diff-hl :ensure t)
;;(global-diff-hl-mode)

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
;;(use-package js2-mode :ensure t)
;;(setq-default js2-basic-offset 2)
;;(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))

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

(use-package cider :ensure t)
(cider-auto-test-mode 1)

(use-package smartparens :ensure t)

(use-package smartparens-config
  :ensure smartparens
  :config (progn (show-smartparens-global-mode t)))

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-auto-test-mode t)
 '(custom-safe-themes
   (quote
    ("bd6ced8badda12f95e16e641d76d861de096c691720ede6388a226914e97cf23" "2d392972cbe692ee4ac61dc79907af65051450caf690a8c4d36eb40c1857ba7d" "7d56fb712ad356e2dacb43af7ec255c761a590e1182fe0537e1ec824b7897357" "c8f959fb1ea32ddfc0f50db85fea2e7d86b72bb4d106803018be1c3566fd6c72" "1728dfd9560bff76a7dc6c3f61e9f4d3e6ef9d017a83a841c117bd9bebe18613" "728eda145ad16686d4bbb8e50d540563573592013b10c3e2defc493f390f7d83" default)))
 '(package-selected-packages
   (quote
    (inf-ruby rubocop magit yasnippet-snippets ws-butler which-key use-package tide swiper smartparens shell-pop ruby-end rspec-mode robe projectile ivy-rich indium forge flycheck-pos-tip flycheck-clojure expand-region exec-path-from-shell elixir-yasnippets doom-themes doom-modeline diff-hl dashboard crystal-mode anzu ample-theme alchemist ag ace-window ace-jump-mode ac-js2))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
