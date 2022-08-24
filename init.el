(load-file "~/.emacs.d/configs/defaults.el")

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("elpa" . "https://elpa.org/packages/"))

(package-initialize)
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(load-file "~/.emacs.d/configs/ui.el")
(load-file "~/.emacs.d/configs/completion.el")
(load-file "~/.emacs.d/configs/persp.el")
(load-file "~/.emacs.d/configs/git.el")
(load-file "~/.emacs.d/configs/lsp.el")

;; Languages
(load-file "~/.emacs.d/configs/ruby.el")
(load-file "~/.emacs.d/configs/elixir.el")

;; VIM
(load-file "~/.emacs.d/configs/evil.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(magit vertico evil use-package))
 '(safe-local-variable-values
   '((eval set
           (make-local-variable 'rspec-primary-source-dirs)
           (setq rspec-primary-source-dirs
                 '("app" "apps" "lib")))))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
