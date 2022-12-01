(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package nano-modeline
  :ensure t 
  :init
  (setq nano-modeline-position 'bottom)
  (nano-modeline-mode))

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
  (popper-mode +1)
  (popper-echo-mode +1))

(defun nano-modeline-default-mode (&optional icon)
  (let ((icon (or icon
                  (plist-get (cdr (assoc 'text-mode nano-modeline-mode-formats)) :icon)))
        ;; We take into account the case of narrowed buffers
        (buffer-name (cond
                      ((and (derived-mode-p 'org-mode)
                            (buffer-narrowed-p)
                            (buffer-base-buffer))
                       (format"%s [%s]" (buffer-base-buffer)
                              (org-link-display-format 
                              (substring-no-properties (or (org-get-heading 'no-tags)
                                                       "-")))))
                      ((and (buffer-narrowed-p)
                            (buffer-base-buffer))
                       (format"%s [narrow]" (buffer-base-buffer)))
                      (t
                       (file-relative-name buffer-file-name (projectile-project-root)))))
        
        (mode-name   (nano-modeline-mode-name))
        (branch      (nano-modeline-vc-branch))
        (position    (format-mode-line "%l:%c")))
    (nano-modeline-render icon
                          buffer-name
                          (if branch (concat "(" branch ")") "")
                          position)))
