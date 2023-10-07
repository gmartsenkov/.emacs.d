(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 140 :weight 'medium)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(winner-mode t)
(electric-pair-mode t)
(setq max-lisp-eval-depth 10000)
(setq ns-use-thin-smoothing t)
(setq delete-old-versions -1 )
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) )
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) )
(setq ring-bell-function 'ignore )
(setq coding-system-for-read 'utf-8 )
(setq coding-system-for-write 'utf-8 )
(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq-default indent-tabs-mode nil)

(setf epa-pinentry-mode 'loopback)
(setq auth-sources '("~/.authinfo.gpg"))
(setq use-dialog-box nil)
(setq linum-format "%d ")
(setq sentence-end-double-space nil)
(setq default-fill-column 80)
(setq compilation-scroll-output t)
(setq ruby-indent-level 2)
(setq ruby-insert-encoding-magic-comment nil)
(setq js-indent-level 2)
(setq visual-line-mode nil)
(setq frame-title-format "\n")
(setq ns-use-proxy-icon nil)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)


;; ELPACA
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(elpaca-wait)
;; ELPACA END

;; PACKAGES
;; (use-package ef-themes
;;   :ensure t
;;   :config
;;   (load-theme 'ef-trio-dark))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t ; if nil, italics is universally disabled
        doom-gruvbox-dark-variant "soft")
  (load-theme 'doom-gruvbox t)
  (doom-themes-org-config))

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Async Shell Command\\*"
          "*rspec-compilation*"
          "*mix test*"
          "*RuboCop"
          "*Help*"
          "*compilation*"
          "\\*Bundler\\*"))
  (setq popper-window-height 25)
  (setq popper-display-function 'popper-display-popup-at-bottom)
  (popper-mode +1)
  (popper-echo-mode +1))

(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  (evil-set-leader 'normal (kbd "SPC"))

  (global-set-key (kbd "C-h") 'evil-window-left)
  (global-set-key (kbd "C-l") 'evil-window-right)
  (global-set-key (kbd "C-j") 'evil-window-down)
  (global-set-key (kbd "C-k") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "]d") 'flycheck-next-error)
  (evil-define-key 'normal 'global (kbd "[d") 'flycheck-previous-error)
  (evil-define-key 'insert 'global (kbd "C-e") 'end-of-line)
  (evil-define-key 'insert 'global (kbd "C-a") 'beginning-of-line)
  (evil-define-key 'normal 'global (kbd "<escape>") (lambda ()
                                                      (interactive)
                                                      (popper--bury-all)))
  (evil-define-key 'normal 'global (kbd "gt") 'evil-avy-goto-char-2)
  (evil-define-key 'normal 'global (kbd "<leader>nt") 'org-roam-dailies-goto-today)
  (evil-define-key 'normal 'global (kbd "<leader>nc") 'org-roam-dailies-capture-today)
  (evil-define-key 'insert 'global (kbd "C-v") 'yank)
  (evil-define-key 'normal 'global (kbd "<leader>T") (lambda
						       ()
						       (interactive)
						       (split-window-sensibly)
						       (other-window 1)
						       (term "/bin/zsh")))
  ;; (evil-define-key 'normal 'global (kbd "<leader><tab><tab>") (lambda
  ;;                                                        ()
  ;;                                                        (interactive)
  ;;                                                        (->
  ;;                                                         (let ((x 0)) (mapcar
  ;;                                                                       (lambda (a) (string-join (list (concat "[" (number-to-string (cl-incf x)) "]") a) ""))
  ;;                                                                       (persp-all-names)))
  ;;                                                         (string-join " | ")
  ;;                                                         (message))))
  ;; (evil-define-key 'normal 'global (kbd "<leader><tab>1") (lambda () (interactive) (persp-switch-by-number 1)))
  ;; (evil-define-key 'normal 'global (kbd "<leader><tab>2") (lambda () (interactive) (persp-switch-by-number 2)))
  ;; (evil-define-key 'normal 'global (kbd "<leader><tab>3") (lambda () (interactive) (persp-switch-by-number 3)))
  ;; (evil-define-key 'normal 'global (kbd "<leader><tab>4") (lambda () (interactive) (persp-switch-by-number 4)))
  (evil-define-key 'normal 'global (kbd "<leader>/") 'projectile-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader>hv") 'describe-variable)
  (evil-define-key 'normal 'global (kbd "<leader>hf") 'describe-function)
  (evil-define-key 'normal 'global (kbd "<leader>hk") 'describe-key)
  (evil-define-key 'normal 'global (kbd "<leader>bd") 'kill-this-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>wu") 'winner-undo)
  (evil-define-key 'normal 'global (kbd "<leader>wv") 'split-window-right)
  (evil-define-key 'normal 'global (kbd "<leader>wh") 'split-window-below)
  (evil-define-key 'normal 'global (kbd "<leader>wd") 'delete-window)
  (evil-define-key 'normal 'global (kbd "<leader>wq") 'delete-window)
  (evil-define-key 'normal 'global (kbd "<leader>wr") 'winner-redo)
  (evil-define-key 'normal 'global (kbd "<leader>sr") 'anzu-query-replace-regexp)
  (evil-define-key 'normal 'global (kbd "<leader>bb") 'projectile-switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-this-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bd") 'kill-this-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>pp") 'projectile-switch-project)
  (evil-define-key 'normal 'global (kbd "<leader>pf") 'projectile-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>SPC") 'projectile-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>fd") 'projectile-find-dir)
  (evil-define-key 'normal 'global (kbd "<leader>ca") 'lsp-execute-code-action)
  (evil-define-key 'normal 'global (kbd "<leader>cf") 'lsp-format-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>cc") 'lsp-workspace-restart)
  (evil-define-key 'normal 'global (kbd "<leader>cd") 'lsp-find-definition)
  (evil-define-key 'normal 'global (kbd "<leader>cD") 'xref-find-definitions-other-window)
  (evil-define-key 'normal 'global (kbd "<leader>cr") 'lsp-find-references)
  (evil-define-key 'normal 'global (kbd "<leader>gg") 'magit)
  (evil-define-key 'normal 'global (kbd "<leader>gc") 'magit-branch-or-checkout)
  (evil-define-key '(normal visual) 'global (kbd "<leader>gl") 'git-link)
  (evil-define-key 'normal 'global (kbd "<leader>gb") 'magit-blame)
  (evil-define-key 'normal magit-status-mode-map (kbd "q") (lambda ()
                                                       (interactive)
                                                       (winner-undo)
                                                       (mapc #'kill-buffer (magit-mode-get-buffers))))
  (evil-define-key 'normal ruby-ts-mode-map (kbd "<leader>tt") 'rspec-toggle-spec-and-target)
  (evil-define-key 'normal ruby-ts-mode-map (kbd "<leader>tv") 'rspec-verify)
  (evil-define-key 'normal ruby-ts-mode-map (kbd "<leader>tl") 'rspec-rerun)
  (evil-define-key 'normal ruby-ts-mode-map (kbd "<leader>tf") 'rspec-run-last-failed)
  (evil-define-key 'normal ruby-ts-mode-map (kbd "<leader>tc") 'rspec-verify-single)
  (evil-define-key 'normal ruby-ts-mode-map (kbd "<leader>ta") 'rspec-verify-all)
  (evil-define-key 'normal ruby-ts-mode-map (kbd "<leader>mp") 'rubocop-check-project)
  (evil-define-key 'normal ruby-ts-mode-map (kbd "<leader>mbi") 'bundle-install)
  (evil-define-key 'normal rust-ts-mode-map (kbd "<leader>ta") 'rust-test)
  (evil-define-key 'normal rust-ts-mode-map (kbd "<leader>mr") 'rust-run)
  (evil-define-key 'normal rust-ts-mode-map (kbd "<leader>mb") 'rust-compile)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>md") 'cider-clojuredocs)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>mc") 'cider)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>tt") 'projectile-toggle-between-implementation-and-test)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>ta") 'cider-test-run-project-tests)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>tv") 'cider-test-run-test)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>tn") 'cider-test-run-ns-tests)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>eb") 'cider-eval-buffer)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>ee") 'cider-eval-last-sexp)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>rn") 'cider-repl-set-ns)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>rb") 'cider-switch-to-repl-buffer)
  (evil-define-key 'normal emacs-lisp-mode-map (kbd "<leader>eb") 'eval-buffer)
  (evil-define-key 'normal emacs-lisp-mode-map (kbd "<leader>ee") 'eval-last-sexp)
  (evil-define-key 'normal elixir-ts-mode-map (kbd "<leader>ta") 'mix-test)
  (evil-define-key 'normal elixir-ts-mode-map (kbd "<leader>tv") 'mix-test-current-test)
  (evil-define-key 'normal elixir-ts-mode-map (kbd "<leader>tt") 'gotospec)
  (evil-define-key 'normal go-mode-map (kbd "<leader>tt") 'projectile-toggle-between-implementation-and-test)
  (evil-define-key 'normal go-mode-map (kbd "<leader>tv") 'go-test-current-file)
  (evil-define-key 'normal go-mode-map (kbd "<leader>tc") 'go-test-current-test)
  (evil-define-key 'normal go-mode-map (kbd "<leader>ta") 'go-test-current-project))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

(use-package evil-collection
  :ensure t
  :custom (evil-collection-setup-minibuffer t)
  :init
  (evil-collection-init)
  (evil-collection-define-key 'insert 'vertico-map
    (kbd "<escape>") 'abort-recursive-edit)

  (evil-collection-define-key 'insert 'vertico-map
    (kbd "C-k") 'vertico-previous))

(use-package magit
  :ensure t
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

(use-package projectile
  :ensure t
  :custom
  (projectile-create-missing-test-files t)
  :init
  (projectile-mode t)
  (setq projectile-completion-system 'default
        projectile-auto-discover nil
        projectile-project-search-path '("~/Development"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
(use-package vertico
  :ensure t
  :init
  (setq completion-styles '(basic partial-completion orderless))
  (setq vertico-count 15)
  (vertico-mode)
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)
	      ("C-f" . vertico-exit)
	      :map minibuffer-local-map
	      ("M-h" . backward-kill-word)))

(use-package vertico-directory
  :elpaca nil
  :after vertico
  ;; More convenient directory navigation commands
  :init
  (load-file "~/.emacs.d/elpaca/repos/vertico/extensions/vertico-directory.el")
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package savehist
  :elpaca nil
  :ensure t
  :init
  (savehist-mode))

(use-package company
  :ensure t
  :custom
  (company-tooltip-limit 10)
  (company-idle-delay .1)
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 2)
  (company-backends '((company-files company-yasnippet :separate company-tide company-capf)))
  :init
  (add-hook 'ruby-ts-mode-hook 'company-mode)
  (add-hook 'js-ts-mode-hook 'company-mode)
  (add-hook 'js-mode-hook 'company-mode)
  (add-hook 'tsx-ts-mode-hook 'company-mode)
  (add-hook 'go-mode-hook 'company-mode)
  (add-hook 'rust-ts-mode-hook 'company-mode)
  (add-hook 'clojure-mode-hook 'company-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'elixir-mode-hook 'company-mode)
  (add-hook 'inf-ruby-mode-hook 'company-mode)
  (add-hook 'javascript-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

(use-package rg :ensure t)

(use-package inf-ruby :ensure t
  :config
  (inf-ruby-enable-auto-breakpoint))

(use-package mix
  :ensure t
  :custom
  (mix-command-test "espec"))
(use-package rspec-mode
  :ensure t
  :custom
  (rspec-primary-source-dirs '("app" "apps" "lib"))
  ;;(setq rspec-primary-source-dirs '("apps"))
  )

;; (use-package corfu
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)                 ;; Enable auto completion
;;   (corfu-popupinfo-delay 0)
;;   (completion-cycle-threshold 3)
;;   (tab-always-indent 'complete)
;;   :init
;;   (corfu-popupinfo-mode)
;;   (global-corfu-mode))

;; (use-package alchemist)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(elixir-ts-mode . ("/opt/homebrew/bin/elixir-ls")))
  (add-to-list 'eglot-server-programs
               '(ruby-ts-mode . ("/Users/gogo/.asdf/shims/solargraph" "stdio"))))
;;

(with-eval-after-load 'projectile
  (projectile-register-project-type 'elixir '("mix.exs")
                                    :project-file "mix.exs"
                                    :compile "mix compile"
                                    :src-dir "lib/"
                                    :test-dir "spec/"
                                    :test "mix espec"
                                    :test-suffix "_spec"))
(use-package flycheck
  :ensure t
  :custom
  (flycheck-indication-mode nil)
  :init
  (add-hook 'ruby-ts-mode-hook 'flycheck-mode)
  (add-hook 'elixir-ts-mode-hook 'flycheck-mode)
  (add-hook 'ruby-ts-mode-hook
            (lambda ()
              (setq-local flycheck-command-wrapper-function
                          (lambda (command) (append '("bundle" "exec") command))))))
(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:update-interval 0.1))

(use-package git-gutter-fringe
  :ensure t
  :config
  (set-fringe-mode '(3 . 4))
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center))

(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil nil) ;; no continuation indicators
      ;; '(nil right-curly-arrow) ;; right indicator only
      ;; '(left-curly-arrow nil) ;; left indicator only
      ;; '(left-curly-arrow right-curly-arrow) ;; default
      )


(setq gotospec-config
      '((ex . ((test-folder . "spec")
               (source-strip-folder . "lib")
               (strip-file-suffix . "")
               (test-suffix . "_spec.exs")))
        (exs . ((test-folder . "lib")
                (source-strip-folder . "spec")
                (strip-file-suffix . "_spec")
                (test-suffix . ".ex")))))

(defun gotospec ()
  (interactive
   (let* ((project-root (cdr (project-current)))
          (file-path (buffer-file-name))
          (relative-file-path (file-relative-name file-path project-root))
          (file (->> file-path (file-name-split) (last) (nth 0)))
          (extension (file-name-extension file))
          (config (alist-get (intern extension) gotospec-config))
          (test-folder (file-name-as-directory (alist-get 'test-folder config)))
          (test-suffix (alist-get 'test-suffix config))
          (strip-file-suffix (alist-get 'strip-file-suffix config))
          (source-strip-folder (file-name-as-directory (alist-get 'source-strip-folder config)))
          (target (concat
                   project-root
                   test-folder
                   (string-remove-prefix
                    (file-name-as-directory source-strip-folder)
                    (file-name-directory relative-file-path))
                   (concat
                    (string-remove-suffix strip-file-suffix (file-name-sans-extension file))
                    test-suffix))))
     (find-file target))))

(add-hook 'ruby-ts-mode-hook 'eglot-ensure)
(add-hook 'elixir-ts-mode-hook 'eglot-ensure)

(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode))
(add-to-list 'major-mode-remap-alist
             '(ruby-mode . ruby-ts-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0"
     "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66"
     "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2"
     "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     "9e1cf0f16477d0da814691c1b9add22d7cb34e0bb3334db7822424a449d20078"
     default))
 '(safe-local-variable-values
   '((eval set (make-local-variable 'mix-command-test)
           (setq mix-command-test "test"))
     (eval set (make-local-variable 'rspec-primary-source-dirs)
           (setq rspec-primary-source-dirs '("app" "apps" "lib"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
