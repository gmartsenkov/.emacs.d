(set-frame-font "JetbrainsMono Nerd Font Mono-15")
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time)
(setq delete-old-versions -1 )
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) )
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) )
(setq ring-bell-function 'ignore )
(setq coding-system-for-read 'utf-8 )
(setq coding-system-for-write 'utf-8 )

(setq sentence-end-double-space nil)
(setq default-fill-column 80)
(setq compilation-scroll-output t)
(setq ruby-indent-level 2)
(setq ruby-insert-encoding-magic-comment nil)
(setq rspec-primary-source-dirs '("app"))

;; Performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq native-comp-deferred-compilation t)
