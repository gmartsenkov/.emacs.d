(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tokyo-night t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (use-package doom-modeline :ensure t :init (doom-modeline-mode 1))

(use-package jetbrains-darcula-theme
  :config
  (load-theme 'jetbrains-darcula t))

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
          "*Help*"
          "*compilation*"
          "\\*Bundler\\*"))
  (setq popper-window-height 25)
  (setq popper-display-function 'popper-display-popup-at-bottom)
  (popper-mode +1)
  (popper-echo-mode +1))

(require 'zone)
(zone-when-idle 120)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq frame-title-format "\n")
(setq ns-use-proxy-icon nil)

(use-package nano-modeline
  :ensure t
  :config
  (setq nano-modeline-position "bottom")
  :init (nano-modeline-mode))
