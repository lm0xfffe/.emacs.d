;;; init-rust.el --- Rust -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package rust-mode
  :ensure t
  :defines lsp-rust-server
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (with-eval-after-load 'lsp-mode
    (when (executable-find "rust-anlyzer")
      (setq lsp-rust-server 'rust-analyzer)))
  :custom
  (rust-format-on-save (executable-find "rustfmt")))
;; Cargo support
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :after flycheck
  :hook (rust-mode . flycheck-rust-setup))
(provide 'init-rust)
