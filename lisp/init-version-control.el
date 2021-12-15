;; 交互式Git工具
(use-package magit
  :ensure t
  :defer 2
  :init
  (use-package with-editor
    :ensure t
    :after after-init-hook))

;; 显示当前行修改-Git
(use-package git-gutter-fringe
  :disabled
  :ensure nil
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:update-interval 1)
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "_")
  (git-gutter:modified-sign "~")
  (git-gutter:hide-gutter t))

;; 高亮修改记录
(use-package diff-hl
  :ensure t
  :hook ((prog-mode  . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)) )

(provide 'init-version-control)
