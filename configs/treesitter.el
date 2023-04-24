(require 'treesit)
(setq treesit-extra-load-path '("/Users/gogo/tree-sitter-module/dist" "/home/gogo/tree-sitter-module"))
(setq major-mode-remap-alist '((ruby-mode . ruby-ts-mode)
                               (rust-mode . rust-ts-mode)))
