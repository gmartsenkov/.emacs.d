(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#282a36" "#ff5555" "#50fa7b" "#f1fa8c" "#61bfff" "#ff79c6" "#8be9fd" "#f8f8f2"])
 '(custom-enabled-themes (quote (doom-spacegrey)))
 '(custom-safe-themes
   (quote
    ("ef4edbfc3ec509612f3cf82476beddd2aeb3da7bdc3a35726337a0cc838a4ef4" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "8c847a5675ece40017de93045a28ebd9ede7b843469c5dec78988717f943952a" "06e4b3fdcbadc29ff95a7146dee846cd027cfefca871b2e9142b54ad5de4832f" "4ea0aa360264ff861fb0212abe4161b83ad1d8c8b74d8a04bcd1baf0ebdceeae" "cdb3e7a8864cede434b168c9a060bf853eeb5b3f9f758310d2a2e23be41a24ae" "2f0cbe053485bccbbbb582acdba7c7c9585ad808ee8ab32f0d727c3d39b42275" "8047ac280914cbe8dcdc489703c398f0941339cfca77dfc09f3641f1f040267c" "e3c87e869f94af65d358aa279945a3daf46f8185f1a5756ca1c90759024593dd" "4e132458143b6bab453e812f03208075189deca7ad5954a4abb27d5afce10a9a" "f5568ed375abea716d1bdfae0316d1d179f69972eaccd1f331b3e9863d7e174a" "e95ad48fd7cb77322e89fa7df2e66282ade015866b0c675b1d5b9e6ed88649b4" "e838d6375a73fda607820c65eb3ea1f9336be7bd9a5528c9161e10c4aa663b5b" "b0fd04a1b4b614840073a82a53e88fe2abc3d731462d6fde4e541807825af342" "9c27124b3a653d43b3ffa088cd092c34f3f82296cf0d5d4f719c0c0817e1afa6" "868abc288f3afe212a70d24de2e156180e97c67ca2e86ba0f2bf9a18c9672f07" "155a5de9192c2f6d53efcc9c554892a0d87d87f99ad8cc14b330f4f4be204445" "cb477d192ee6456dc2eb5ca5a0b7bd16bdb26514be8f8512b937291317c7b166" "614e5089876ea69b515c50b6d7fa0a37eb7ed50fda224623ec49e1c91a0af6a1" "8e04ea7bf8a736b0bfacd363f4810ffce774ff9ba24f356172ae2b83307aebb2" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "fd944f09d4d0c4d4a3c82bd7b3360f17e3ada8adf29f28199d09308ba01cc092" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "9954ed41d89d2dcf601c8e7499b6bb2778180bfcaeb7cdfc648078b8e05348c6" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "a8c210aa94c4eae642a34aaf1c5c0552855dfca2153fa6dd23f3031ce19453d4" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" default)))
 '(doom-modeline-mode t)
 '(fci-rule-color "#6272a4")
 '(global-flycheck-mode t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1E2029" "#bd93f9"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1E2029" "#50fa7b"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1E2029" "#565761"))
 '(objed-cursor-color "#ff5555")
 '(package-selected-packages
   (quote
    (shell-pop goto-chg anzu robe ag ac-js2 js2-mode ace-jump-mode expand-region ws-butler forge yasnippet-snippets elixir-yasnippets yasnippets exec-path-from-shell alchemist company elixir-mode magit ace-window dashboard doom-themes doom-modeline which-key ivy rspec-mode ruby-end projectile use-package)))
 '(shell-pop-universal-key "C-t")
 '(vc-annotate-background "#282a36")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50fa7b")
    (cons 40 "#85fa80")
    (cons 60 "#bbf986")
    (cons 80 "#f1fa8c")
    (cons 100 "#f5e381")
    (cons 120 "#face76")
    (cons 140 "#ffb86c")
    (cons 160 "#ffa38a")
    (cons 180 "#ff8ea8")
    (cons 200 "#ff79c6")
    (cons 220 "#ff6da0")
    (cons 240 "#ff617a")
    (cons 260 "#ff5555")
    (cons 280 "#d45558")
    (cons 300 "#aa565a")
    (cons 320 "#80565d")
    (cons 340 "#6272a4")
    (cons 360 "#6272a4")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-file "~/.emacs.d/common.el")
(load-theme 'doom-spacegrey t)

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
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

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

;;shell-pop
(use-package shell-pop :ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(shell-pop-default-directory "/Users/martsenkovg")
 '(shell-pop-shell-type (quote ("terminal" "*terminal*" (lambda nil (term shell-pop-term-shell)))))
 '(shell-pop-term-shell "/bin/zsh")
 '(shell-pop-universal-key "C-t")
 '(shell-pop-window-size 40)
 '(shell-pop-full-span t)
 '(shell-pop-window-position "right"))

(define-key term-raw-map (kbd "C-t") 'shell-pop)
