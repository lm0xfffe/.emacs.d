;;; ============配置dired相关=============


;; 在Emacs中使用终端程序ranger
(use-package ranger
  :disabled
  :ensure t
  :config
  (ranger-override-dired-mode 1))

;(use-package dired
;  :hook (dired-mode . dired-hide-details-mode))

(provide 'init-dired)
