;; ;; 写网页可用的模式
(use-package 
  web-mode
  :ensure t
  :mode ("\\.html\\'" . web-mode)
  :hook (html-mode . web-mode))

(use-package emmet-mode
  :ensure t
  :hook ((web-mode . emmet-mode)
		 (css-mode . emmet-mode)
         (html-mode . emmet-mode)))

;; ;; (use-package css-mode
;; ;;   :ensure nil
;; ;;   :init (setq css-indent-offset 2))

(use-package scss-mode
  :ensure t
  :mode ("\\.scss\\'" . scss-mode)
  :init
  (setq scss-compile-at-save t))

;; ;; (unless (fboundp 'less-css-mode)
;; ;;   (use-package less-css-mode
;; ;; 	:ensure nil))

;; ;; 写js可用的模式
;; (use-package js2-mode 
;;   :mode (("\\.js\\'" . js2-mode)
;; 		 ("\\.jsx\\'" . js2-jsx-mode))
;;   :ensure t)
(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)))

(use-package prettier-js
  :ensure t
  :hook ((;; js-mode js2-mode 
                  json-mode ;; web-mode
                            css-mode sgml-mode ;; html-mode
                            )
         .
         prettier-js-mode))

;; Vue support
(use-package vue-mode
  :mode "\\.vue\\'"
  :config
  (add-hook 'vue-mdoe-hook #'lsp)
  (setq lsp-eslint-enable nil))



;; typescript support
(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . flycheck-mode))

(use-package tide
  :ensure t
  :hook ((js-mode . tide-setup)
         (typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :config
  (tide-auto-compile-file))



(provide 'init-web)
