;; Emacs对语言服务器支持的插件
(use-package lsp-mode
  :ensure t
  :defer 2
  :init (setq lsp-keymap-prefix "s-g")
  :commands (lsp)
  :hook (((java-mode web-mode scss-mode c-mode c++-mode objc-mode python-mode rust-mode) . lsp)
         (lsp-mode . (lambda ()
                       (lsp-enable-which-key-integration))))
  :custom
  (lsp-idle-delay 200)
  (lsp-auto-guess-root nil)
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1240))
  (lsp-eldoc-hook nil)
  (lsp-auto-configure t)
  (lsp-log-io nil)
  (lsp-prefer-flymake nil)
  (lsp-diagnostics-provide :flycheck)
  (lsp-enable-indentation t)
  (lsp-enable-on-type-formatting nil)
  (lsp-response-timeout 3)  
  :bind (:map lsp-mode-map
			  ("C-c C-f" . lsp-format-buffer)
			  ("M-RET" . lsp-ui-sideline-apply-code-actions))
  :config
  (define-key lsp-command-map (kbd "s-g") lsp-command-map)
  
    ;; 设置lsp的一些界面功能
  ;; 具体变量是干嘛的可以参考这里： https://emacs-lsp.github.io/lsp-mjjode/tutorials/how-to-turn-off/
  (setq lsp-lens-enable t
        lsp-headerline-breadcrumb-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-code-actions t
        lsp-modeline-code-actions-enable nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-modeline-diagnostics-enable nil
        lsp-signature-auto-activate nil
        lsp-prefer-capf t
        lsp-signature-render-documentation nil)

  (add-hook 'lsp-ui-imenu-mode-hook (lambda ()
                                      (display-line-numbers-mode -1))))

;; 微软的python语言服务器-新版
(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
  :init
  (when (executable-find "python3")
    (setq lsp-pyright-python-executable-cmd "python3"))
  :custom
  (lsp-pyright-use-library-code-for-types t))  ; or lsp-deferred

;; 美化lsp-mode
(use-package 
  lsp-ui
  :ensure t
  :commands (lsp-ui)
  ;; :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq
   ;; sideline
   lsp-ui-sideline-update-mode 'line
   ;; sideline
   lsp-ui-sideline-delay 1
   ;; lsp-ui-imenu列表自动刷新
   lsp-ui-imenu-auto-refresh t
   ;; lsp-ui-imenu列表刷新延迟
   lsp-ui-imenu-auto-refresh-delay 5.0)
  ;; peek
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  ;; doc
  (setq lsp-ui-doc-enable t
        ;; 文档显示的位置
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable nil
        lsp-signature-render-documentation nil
        ;; 显示文档的延迟
        lsp-ui-doc-delay 2))

(provide 'init-lsp)
