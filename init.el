(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 165 :weight 'normal)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(tool-bar-mode 0)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(winner-mode t)
(electric-pair-mode t)
(setq read-process-output-max (* 1024 1024))
(setq-default mode-line-mule-info "")
(setq-default mode-line-modified "")
(setq-default mode-line-front-space "")
(setq-default mode-line-remote "")
(setq package-install-upgrade-built-in t)

(setq compilation-always-kill t)
(setq max-lisp-eval-depth 10000)
(setq ns-use-thin-smoothing t)
(setq delete-old-versions -1)
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
(setq ruby-method-call-indent nil)
(setq ruby-after-operator-indent nil)
(setq ruby-parenless-call-arguments-indent nil)
(setq ruby-method-params-indent 0)
(setq ruby-block-indent nil)
(setq ruby-align-chained-calls nil)
(setq ruby-deep-indent-paren nil)
(setq js-indent-level 2)
(setq visual-line-mode nil)
(setq frame-title-format "\n")
(setq ns-use-proxy-icon nil)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)


;; ELPACA
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
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
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
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

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package ws-butler
  :ensure t
  :hook prog-mode slim-mode)

(use-package catppuccin-theme
  :ensure t
  :custom
  (catppuccin-flavor 'mocha)
  :config
  (load-theme 'catppuccin :noconfirm))

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t ; if nil, italics is universally disabled
;;         doom-gruvbox-dark-variant "soft")
;;   ;; (load-theme 'doom-tokyo-night t)
;;   (load-theme 'doom-one t)
;;   (doom-themes-org-config))

;; (use-package kaolin-themes
;;   :config
;;   (load-theme 'kaolin-shiva t))


(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Async Shell Command\\*"
          "*rspec-compilation*"
          "*mix test*"
          "*RuboCop"
          "*cider-repl"
          "*cider-error*"
          "*cider-test-report"
          "*Help*"
          "*grep"
          "*grep*"
          "*xref*"
          "*rg*"
          "*compilation*"
          "\\*Bundler\\*"))
  (setq popper-window-height (lambda (win)
                               (->
                                window-total-height
                                (/ 2.5)
                                (floor))))
  (setq popper-display-function 'popper-display-popup-at-bottom)
  (popper-mode +1)
  (popper-echo-mode +1))
(add-hook 'compilation-mode-hook
  (lambda ()
    (setq-local compilation-scroll-output t)
    (setq-local scroll-conservatively most-positive-fixnum)
    (setq-local scroll-margin 0)))

(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t))


(defun nano-modeline-relative-buffer-name (&optional name)
  "Buffer name"

  (propertize
   (cond (name name)
         ((buffer-narrowed-p) (format"%s [narrow]" (file-relative-name buffer-file-name (projectile-project-root))))
         (t (file-relative-name buffer-file-name (projectile-project-root))))
   'face (nano-modeline-face 'name)))

;; (file-relative-name buffer-file-name projectile-project-root)

(use-package nano-modeline
  :after (projectile)
  :ensure t
  :config
  (advice-add 'nano-modeline-buffer-name :override #'nano-modeline-relative-buffer-name)
  (setq nano-modeline-position #'nano-modeline-footer)
  (nano-modeline-text-mode t))

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
  (evil-define-key 'normal 'global (kbd "<leader>/") 'counsel-ag)
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
  (evil-define-key 'normal 'global (kbd "<leader>pf") 'project-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>SPC") 'project-find-file)
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
  (evil-define-key 'normal 'global (kbd "<leader>gF") 'magit-pull)
  (evil-define-key '(normal visual) 'global (kbd "<leader>gl") 'git-link)
  (evil-define-key 'normal 'global (kbd "<leader>gb") 'magit-blame)
  (evil-define-key 'normal magit-status-mode-map (kbd "q") 'mu-magit-kill-buffers)
  (evil-define-key 'normal ruby-ts-mode-map (kbd "<leader>tt") 'rspec-toggle-spec-and-target)
  (evil-define-key 'normal ruby-ts-mode-map (kbd "<leader>tv") 'rspec-verify)
  (evil-define-key 'normal ruby-ts-mode-map (kbd "<leader>tl") 'rspec-rerun)
  (evil-define-key 'normal ruby-ts-mode-map (kbd "<leader>tf") 'rspec-run-last-failed)
  (evil-define-key 'normal ruby-ts-mode-map (kbd "<leader>tc") 'rspec-verify-single)
  (evil-define-key 'normal ruby-ts-mode-map (kbd "<leader>ta") 'rspec-verify-all)
  (evil-define-key 'normal ruby-ts-mode-map (kbd "<leader>mp") 'rubocop-project)
  (evil-define-key 'normal ruby-ts-mode-map (kbd "<leader>mbi") (lambda () (interactive) (me/run-command "bundle install")))
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>tt") 'rspec-toggle-spec-and-target)
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>tv") 'rspec-verify)
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>tl") 'rspec-rerun)
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>tf") 'rspec-run-last-failed)
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>tc") 'rspec-verify-single)
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>ta") 'rspec-verify-all)
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>mp") 'rubocop-project)
  (evil-define-key 'normal ruby-mode-map (kbd "<leader>mbi") (lambda () (interactive) (me/run-command "bundle install")))
  (evil-define-key 'normal rust-mode-map (kbd "<leader>ta") 'rust-test)
  (evil-define-key 'normal rust-mode-map (kbd "<leader>mr") 'rust-run)
  (evil-define-key 'normal rust-mode-map (kbd "<leader>mb") 'rust-compile)
  (evil-define-key 'normal rust-mode-map (kbd "<leader>mf") 'rust-format-buffer)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>md") 'cider-clojuredocs)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>mc") 'cider)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>tt") 'projectile-toggle-between-implementation-and-test)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>ta") 'cider-test-run-project-tests)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>tv") 'cider-test-run-ns-tests)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>tc") 'cider-test-run-test)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>tn") 'cider-test-run-ns-tests)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>eb") 'cider-eval-buffer)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>er") 'cider-eval-defun-at-point)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>ee") 'cider-eval-last-sexp)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>rn") 'cider-repl-set-ns)
  (evil-define-key 'normal clojure-mode-map (kbd "<leader>rb") 'cider-switch-to-repl-buffer)
  (evil-define-key 'normal emacs-lisp-mode-map (kbd "<leader>eb") 'eval-buffer)
  (evil-define-key 'normal emacs-lisp-mode-map (kbd "<leader>ee") 'eval-last-sexp)
  (evil-define-key 'normal elixir-ts-mode-map (kbd "<leader>fm") 'eglot-format)
  (evil-define-key 'normal elixir-ts-mode-map (kbd "<leader>ta") 'elixir-test-project)
  (evil-define-key 'normal elixir-ts-mode-map (kbd "<leader>tv") 'elixir-run-test)
  (evil-define-key 'normal elixir-ts-mode-map (kbd "<leader>tc") 'mix-test-current-test)
  (evil-define-key 'normal elixir-ts-mode-map (kbd "<leader>tt") 'gotospec)
  (evil-define-key 'normal elixir-ts-mode-map (kbd "<leader>mp") (lambda () (interactive) (me/run-command "mix credo")))
  (evil-define-key 'normal elixir-ts-mode-map (kbd "<leader>mf") (lambda () (interactive) (me/run-command "mix format")))
  (evil-define-key 'normal elixir-ts-mode-map (kbd "<leader>md") (lambda () (interactive) (me/run-command "mix deps.get")))
  (evil-define-key 'normal go-mode-map (kbd "<leader>tt") 'projectile-toggle-between-implementation-and-test)
  (evil-define-key 'normal go-mode-map (kbd "<leader>tv") 'go-test-current-file)
  (evil-define-key 'normal go-mode-map (kbd "<leader>tc") 'go-test-current-test)
  (evil-define-key 'normal go-mode-map (kbd "<leader>ta") 'go-test-current-project)
  (evil-define-key 'normal crystal-mode-map (kbd "<leader>tt") 'projectile-toggle-between-implementation-and-test)
  (evil-define-key 'normal crystal-mode-map (kbd "<leader>mp") (lambda () (interactive) (me/run-command "./bin/ameba")))
  (evil-define-key 'normal crystal-mode-map (kbd "<leader>mf") (lambda () (interactive) (me/run-command "crystal tool format")))
  (evil-define-key 'normal crystal-mode-map (kbd "<leader>ta") (lambda () (interactive) (me/run-command "crystal spec"))))

(use-package flycheck
  :hook ((rust-mode . flycheck-mode)
         (clojure-mode . flycheck-mode))
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled idle-buffer-switch))
  (flycheck-idle-buffer-switch-delay 4)
  :ensure t)

(use-package reformatter
  :ensure t
  :hook (clojure-mode . cljstyle-format-on-save-mode)
  :config
  (reformatter-define cljstyle-format
                   :program "cljstyle"
                   :args '("pipe")
                   :lighter " CLJ"))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :custom (flycheck-eglot-exclusive nil)
  :config
  ;; (global-flycheck-eglot-mode 1))
  )

(use-package flycheck-clj-kondo
  :ensure t)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))

(use-package cider
  :ensure t)

(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

;; (use-package eglot-booster
;;   :ensure t
;;   :after eglot
;;   :config (eglot-booster-mode))

(use-package evil-collection
  :ensure t
  :custom (evil-collection-setup-minibuffer t)
  :init
  (evil-collection-init)
  (evil-collection-define-key 'insert 'vertico-map
    (kbd "<escape>") 'abort-recursive-edit)
  (evil-collection-define-key 'insert 'ivy-minibuffer-map
    (kbd "<escape>") 'minibuffer-keyboard-quit)
  (evil-collection-define-key 'insert 'ivy-minibuffer-map
    (kbd "C-j") 'ivy-next-line)
  (evil-collection-define-key 'insert 'ivy-minibuffer-map
    (kbd "C-k") 'ivy-previous-line)

  (evil-collection-define-key 'insert 'vertico-map
    (kbd "C-k") 'vertico-previous))

(use-package rust-mode :ensure t)
(use-package transient :ensure t)
(use-package magit
  :ensure t
  :after transient
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

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package counsel :ensure t)
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
  :ensure nil
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
  :ensure nil
  :init
  (savehist-mode))

(use-package company
  :ensure t
  :hook ((emacs-lisp-mode . company-mode)
         (clojure-mode . company-mode)
         (cider-repl-mode . company-mode)
         (prog-mode . company-mode))
  :config
  (setq company-idle-delay 0.2)
  (setq company-tooltip-idle-delay 0.2)
  (setq company-minimum-prefix-length 3)
  (setq eglot-stay-out-of '(company))
  (setq company-backends '((company-yasnippet :separate company-capf company-dabbrev))))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

(use-package rg :ensure t)
(use-package ruby-end :ensure t)
(use-package inf-ruby :ensure t
  :config
  (inf-ruby-enable-auto-breakpoint))

(use-package rspec-mode
  :ensure t
  :custom
  (rspec-primary-source-dirs '("app" "apps" "lib"))
  ;;(setq rspec-primary-source-dirs '("apps"))
  )

(fset #'jsonrpc--log-event #'ignore)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
   `((elixir-ts-mode heex-ts-mode elixir-mode) .
     ("elixir-ls" "--stdio=true" :initializationOptions (:experimental (:completions (:enable t))))))
  (add-to-list 'eglot-server-programs
               '(ruby-ts-mode . ("/Users/gogo/.asdf/shims/solargraph" "stdio")))
  (add-to-list 'eglot-server-programs
               '(ruby-mode . ("/Users/gogo/.asdf/shims/solargraph" "stdio")))
  (setq eldoc-idle-delay 0.75)
  (setq company-idle-delay 0.75)
  (setq flymake-no-changes-timeout 0.5)
  (add-hook 'eglot-managed-mode-hook (lambda ()
                                       (flymake-mode-off)))
   (setq eglot-events-buffer-size 0
        eglot-ignored-server-capabilities '(:hoverProvider
                                            :documentHighlightProvider
                                            :inlayHintProvider)
        eglot-autoshutdown t))
;; (use-package flycheck
;;   :ensure t
;;   :custom
;;   (flycheck-indication-mode nil)
;;   :init
;;   ;; (add-hook 'rust-ts-mode-hook 'flycheck-mode)
;;   (add-hook 'ruby-ts-mode-hook 'flycheck-mode)
;;   (add-hook 'elixir-ts-mode-hook 'flycheck-mode)
;;   (add-hook 'ruby-mode-hook 'flycheck-mode)
;;   (add-hook 'ruby-mode-hook
;;             (lambda ()
;;               (setq-local flycheck-command-wrapper-function
;;                           (lambda (command) (append '("bundle" "exec") command))))))
(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:update-interval 0.1))

(use-package crystal-mode
  :ensure t
  :config
  (setenv "CRYSTAL_OPTS" "--link-flags=-Wl,-ld_classic"))

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

(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)
(defun elixir-run-test ()
  (interactive
   (let* ((file-path (buffer-file-name))
          (default-directory (cdr (project-current)))
          (file (->> file-path (file-name-split) (last) (nth 0)))
          (extension (file-name-extension file))
          (target (if (string= extension "ex") (find-spec) file-path)))
         (compile (concat "mix espec " target)))))

(defun me/run-command (cmd)
  (let ((default-directory (cdr (project-current))))
    (compile cmd)))

(defun elixir-test-project ()
  (interactive
   (let ((default-directory (cdr (project-current))))
     (compile "mix espec"))))

(defun rubocop-project ()
  (interactive
   (let ((default-directory (cdr (project-current))))
     (compile "bundle exec rubocop"))))

(defun find-spec ()
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
    target))

(defun gotospec ()
  (interactive
   (find-file (find-spec))))

(defun mu-magit-kill-buffers ()
   "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'bury-buffer buffers)))

(defun sw-buff ()
  "Run `switch-to-buffer' with the projects included as annotations."
  (interactive)
  (let ((completion-extra-properties
         '(:annotation-function
           (lambda (buffers)
             (mapcar (lambda (buffer)
                       (list buffer
                             (concat
                              (with-current-buffer buffer
                                (if-let ((proj (project-current)))
                                    (propertize (project-root proj)
                                                'face 'dired-directory)
                                  "<none>"))
                              "	")
                             nil))
                     buffers)))))
    (call-interactively #'projectile-switch-to-buffer)))

(defun mode-line-buffer-file-parent-directory ()
  (when buffer-file-name
    (concat "[" (file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name))) "]")))
(setq-default mode-line-buffer-identification
      (cons (car mode-line-buffer-identification) '((:eval (mode-line-buffer-file-parent-directory)))))

(add-hook 'ruby-mode-hook 'eglot-ensure)
(add-hook 'ruby-ts-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'elixir-ts-mode-hook 'eglot-ensure)
(advice-add '+emacs-lisp-truncate-pin :override (lambda () ()) )

;; (add-to-list 'auto-mode-alist '("\\.rs$" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cr$" . crystal-mode))
(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode))


(setq-default mode-line-format
              (cl-set-difference
               mode-line-format
               '(mode-line-modes mode-line-position mode-line-end-spaces evil-mode-line-tag)))
;; (add-to-list 'major-mode-remap-alist
;;              '(ruby-mode . ruby-ts-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d77d6ba33442dd3121b44e20af28f1fae8eeda413b2c3d3b9f1315fbda021992"
     "7af2a6fcd1e743d165c58fd95d20b46c2d96d9873ab67fc9371bdc8fda463de7"
     "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9"
     "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882"
     "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336"
     "0170347031e5dfa93813765bc4ef9269a5e357c0be01febfa3ae5e5fcb351f09"
     "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
     "9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3"
     "788121c96b7a9b99a6f35e53b7c154991f4880bb0046a80330bb904c1a85e275"
     "5a00018936fa1df1cd9d54bee02c8a64eafac941453ab48394e2ec2c498b834a"
     "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428"
     "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041"
     "249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b"
     "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37"
     "06ed754b259cb54c30c658502f843937ff19f8b53597ac28577ec33bb084fa52"
     "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7"
     "6f5bdd8dd506365a61c9fbfb430b58e7908796fe20d34e675ec9848b67e62350"
     "bb979d408eee95bbccdbda45bb0e4be8d55e4fc3d6f8739decafa82e6df93532"
     "2a9eb9743efe8a3602a90ef564b763f76cc9ecba73ea1f075ce3981bef7fa653"
     "2e7c6dfaaabcef29764b111a9ad4ff2037a1dc275ef191c90222b973542cd5ea"
     "d11e54ae9e37d61deb2c92c74bc52a27462f62d2ff68c5d01119b0cc30377042"
     "ad400d9c36d7257b2a5152875f5f242c61c6d40619e2d4ee9c501daf9158a674"
     "eee1e4b3207b70de3d9ea8e9b6c78b4be6f86955b0a0917a48675fe3964ed374"
     "088c0504a552e4b00f671369411919373e3563f16bb790a1e3e3fd7d39a0e050"
     "553a6676b68142a652c034cfe2179a2271240bc1e33611c2de3a7207ce7608ca"
     "e7820b899036ae7e966dcaaec29fd6b87aef253748b7de09e74fdc54407a7a02"
     "eb3141ffdf8f8070f447e51b085ef4d1e6074c47e9cae84b103a9256dca9ed56"
     "b9c804f672050817981dcc58a04e236d685417c3318554088d5552c819243281"
     "4fe4a418bb02cda8df3fe5dad1c1d177fd517c1ea20871a2e8fce329edd3952d"
     "9cf3d97bb6d526cd51b903a2db02d2034074437f202375feeca9c36c60c007bc"
     "30526ab3507de50fd504df6d7ccff2fd9a564999f831ead500f0a10b1fe43053"
     "4b0c6453047e26395cd601f9c9d034bd34e943d8f5f08f5126edd48c39d42837"
     "242f33ba517c05f45e075d8ed3d13c0a7b7d1392e0c95d66830029e561607085"
     "0d0936adf23bba16569c73876991168d0aed969d1e095c3f68d61f87dd7bab9a"
     "3454885b915a176dce4b53e35053b7ee0aa9362fb9e934057ac44b6842a97453"
     "e8ab68ce371f48623dab9658d7910458e98affec3e09585a39552dbd3fd1ecda"
     "46325e20421d2b4423cc90db5a35e0a45bd78a6f3e26c52314b189af3cc00733"
     "d31c3706f7c1b0520405c796b33f515bc481d2062cbc964f3c36925665999a6d"
     "9b64a681308383067359cf06bfa6a1bc4fa75c5b68182e4d6ba4d1816277d70e"
     "6ed8a3705a4296955010ecfcf808f02ac0d52985373e07c63f7fe5bc85206bb4"
     "d9c038dc91688c433de8e83709449563ec6475b900a21d7016856035ae4dcd32"
     "f84dbe5cfa80aa6774c57fef30d76bcdeb71bd0077665fb74f75728c42f5675d"
     "71acf47cc8cd4158e52ef63a9f8c4d128aa33d6772a0106b5a72757486047e08"
     "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "badd1a5e20bd0c29f4fe863f3b480992c65ef1fa63951f59aa5d6b129a3f9c4c"
     "8b148cf8154d34917dfc794b5d0fe65f21e9155977a36a5985f89c09a9669aa0"
     "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e"
     "38c0c668d8ac3841cb9608522ca116067177c92feeabc6f002a27249976d7434"
     "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33"
     "c517e98fa036a0c21af481aadd2bdd6f44495be3d4ac2ce9d69201fcb2578533"
     "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039"
     "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad"
     "a9dc7790550dcdb88a23d9f81cc0333490529a20e160a8599a6ceaf1104192b5"
     "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443"
     "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da"
     "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b"
     "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294"
     "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
     "89d9dc6f4e9a024737fb8840259c5dd0a140fd440f5ed17b596be43a05d62e67"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     "70e7f094987e3c6226c54078dd986e11cab7246ea1c9e58a9907afa90f3c10c9"
     "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22"
     "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68"
     "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184"
     "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554"
     "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738"
     "3de5c795291a145452aeb961b1151e63ef1cb9565e3cdbd10521582b5fd02e9a"
     "5ec088e25ddfcfe37b6ae7712c9cb37fd283ea5df7ac609d007cafa27dab6c64"
     "d43860349c9f7a5b96a090ecf5f698ff23a8eb49cd1e5c8a83bb2068f24ea563"
     "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0"
     "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66"
     "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2"
     "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     "9e1cf0f16477d0da814691c1b9add22d7cb34e0bb3334db7822424a449d20078"
     default))
 '(package-selected-packages
   '(ws-butler rust-mode popper magit exec-path-from-shell evil-surround
               ef-themes))
 '(safe-local-variable-values
   '((eval set (make-local-variable 'rspec-primary-source-dirs)
           (setq rspec-primary-source-dirs '("app")))
     (eval set (make-local-variable 'mix-command-test)
           (setq mix-command-test "test"))
     (eval set (make-local-variable 'rspec-primary-source-dirs)
           (setq rspec-primary-source-dirs '("app" "apps" "lib"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
