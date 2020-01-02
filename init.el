(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package doom-themes :ensure t)

(load-theme 'doom-gruvbox t)
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
(global-set-key (kbd "C-c r p") 'rubocop-check-project)
(use-package inf-ruby :ensure t)
(use-package robe :ensure t)
(add-hook 'ruby-mode-hook #'rubocop-mode)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(setq ruby-indent-level 2)
(setq rspec-primary-source-dirs '("app"))
(setq ruby-insert-encoding-magic-comment nil)
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))
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


;; (use-package lsp-mode
;;   :ensure t
;;   :commands (lsp lsp-deferred)
;;   :hook (js-mode . lsp-deferred)
;;   :config
;;   (add-hook 'js-mode-hook (lambda () (setq js-indent-level 4)))
;;   (add-hook 'js-mode-hook (lambda () (setq tab-width 4))))

;; (use-package company-lsp :ensure t)
;; (push 'company-lsp company-backends)

(use-package tide
  :ensure t
  :after (js company flycheck)
  :hook ((js-mode . tide-setup)
         (js-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(setq js-indent-level 2)
(add-hook 'js-mode-hook (lambda () (setq tab-width 2)))

(setq tide-format-options '(:indentSize 2 :tabSize 2))
(use-package web-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "js" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
;;(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

(use-package add-node-modules-path :ensure t)
(add-hook 'js-mode-hook 'add-node-modules-path)

;; Duplicate thing
(use-package duplicate-thing :ensure t)
(global-set-key (kbd "M-c") 'duplicate-thing)

(use-package json-mode :ensure t)
(use-package csv-mode :ensure t)

(projectile-register-project-type 'npm '("package.json")
                                  :compile "npm install"
                                  :test "npm test"
                                  :run "npm run start"
                                  :test-suffix ".spec")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-auto-test-mode t)
 '(custom-safe-themes
   '("a4b9eeeabde73db909e6b080baf29d629507b44276e17c0c411ed5431faf87dd" "e7666261f46e2f4f42fd1f9aa1875bdb81d17cc7a121533cad3e0d724f12faf2" "fe76f3d5094967034192f6a505085db8db6deb0e135749d9a54dc488d6d3ee2f" "bc99493670a29023f99e88054c9b8676332dda83a37adb583d6f1e4c13be62b8" "a02836a5807a687c982d47728e54ff42a91bc9e6621f7fe7205b0225db677f07" "e47c0abe03e0484ddadf2ae57d32b0f29f0b2ddfe7ec810bd6d558765d9a6a6c" "1ca1f43ca32d30b05980e01fa60c107b02240226ac486f41f9b790899f6f6e67" "32fd809c28baa5813b6ca639e736946579159098d7768af6c68d78ffa32063f4" "1897b97f63e91a792e8540c06402f29d5edcbfb0aafd64b1b14270663d6868ee" "15ba8081651869ec689c9004288bed79003de5b4ee9c51a9d4a208d9e3439706" "4b0b568d63b1c6f6dddb080b476cfba43a8bbc34187c3583165e8fb5bbfde3dc" "c6b93ff250f8546c7ad0838534d46e616a374d5cb86663a9ad0807fd0aeb1d16" "0fe9f7a04e7a00ad99ecacc875c8ccb4153204e29d3e57e9669691e6ed8340ce" "bd6ced8badda12f95e16e641d76d861de096c691720ede6388a226914e97cf23" "2d392972cbe692ee4ac61dc79907af65051450caf690a8c4d36eb40c1857ba7d" "7d56fb712ad356e2dacb43af7ec255c761a590e1182fe0537e1ec824b7897357" "c8f959fb1ea32ddfc0f50db85fea2e7d86b72bb4d106803018be1c3566fd6c72" "1728dfd9560bff76a7dc6c3f61e9f4d3e6ef9d017a83a841c117bd9bebe18613" "728eda145ad16686d4bbb8e50d540563573592013b10c3e2defc493f390f7d83" default))
 '(package-selected-packages
   '(csv-mode web-mode duplicate-thing js2-mode smex inf-ruby rubocop magit yasnippet-snippets ws-butler which-key use-package tide swiper smartparens shell-pop ruby-end rspec-mode robe projectile ivy-rich indium forge flycheck-pos-tip flycheck-clojure expand-region exec-path-from-shell elixir-yasnippets doom-themes doom-modeline diff-hl dashboard crystal-mode anzu ample-theme alchemist ag ace-window ace-jump-mode ac-js2)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
