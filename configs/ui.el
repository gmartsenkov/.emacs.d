(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t ; if nil, italics is universally disabled
        doom-gruvbox-dark-variant "soft")
  (load-theme 'doom-ir-black t)
  (doom-themes-org-config))

;; (use-package doom-modeline :ensure t :init (doom-modeline-mode 1))

;; (use-package jetbrains-darcula-theme
;;   :config
;;   (with-eval-after-load "jetbrains-darcula-theme"
;;     (custom-theme-set-faces
;;      'jetbrains-darcula
;;      '(line-number ((t (:background "2b2b2b" :foreground "#737980"))))))
;;   (load-theme 'jetbrains-darcula t)
;;   (set-face-attribute 'line-number nil
;;                       :background "#2b2b2b" :foreground "#737980"))

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

;; (require 'zone)
;; (zone-when-idle 120)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq frame-title-format "\n")
(setq ns-use-proxy-icon nil)

(use-package nano-modeline
  :ensure t
  :config
  (setq nano-modeline-position "bottom")
  (nano-modeline-mode))

(use-package all-the-icons-completion
  :ensure t
  :config
  (all-the-icons-completion-mode))

(use-package perspective
  :ensure t
  :config
  (setq persp-mode-prefix-key (kbd "C-x x"))
  (persp-mode))
(use-package persp-projectile
  :ensure t)
