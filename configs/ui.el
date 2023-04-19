(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tokio-night t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (use-package doom-modeline :ensure t :init (doom-modeline-mode 1))

(defun popper-keep-focus ()
  (message "HIT")
  (other-window))

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Async Shell Command\\*"
          "*rspec-compilation*"
          "*RuboCop"
          "\\*Bundler\\*"))
  (setq popper-window-height 30)
  (add-hook 'popper-open-popup-hook 'popper-keep-focus)
  (popper-mode +1)
  (popper-echo-mode +1))

