(use-package benchmark-init
  :ensure t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; 括号匹配
(use-package smartparens
  :ensure t 
  :hook (prog-mode . smartparens-global-mode))

(use-package restart-emacs)

;; Emacs下最好用的终端仿真器
(use-package vterm
  :commands (vterm)
  :ensure t
  :bind (:map leader-key
              ("o t" . 'vterm)))

              
;; 有道词典，非常有用
(use-package youdao-dictionary
  :commands (youdao-dictionary-search-at-point-posframe)
  :ensure t 
  :config (setq url-automatic-caching t) 
  (which-key-add-key-based-replacements "C-x y" "有道翻译") 
  :bind (("C-x y t" . 'youdao-dictionary-search-at-point+) 
         ("C-x y g" . 'youdao-dictionary-search-at-point-posframe) 
         ("C-x y p" . 'youdao-dictionary-play-voice-at-point) 
         ("C-x y r" . 'youdao-dictionary-search-and-replace) 
         ("C-x y i" . 'youdao-dictionary-search-from-input)))

         ;; 折叠和收缩代码
(use-package hideshow 
  :ensure t 
  :diminish hs-minor-mode
  :bind (:map prog-mode-map
              ("C-c TAB" . hs-toggle-hiding) 
              ("C-c p +" . hs-show-all)) 
  :hook (prog-mode . hs-minor-mode))

  ;; 撤销树
(use-package undo-tree 
  :ensure t 
  :hook (after-init . global-undo-tree-mode) 
  :init (setq undo-tree-visualizer-timestamps t undo-tree-enable-undo-in-region nil undo-tree-auto-save-history nil)

  ;; HACK: keep the diff window
  (with-no-warnings (make-variable-buffer-local 'undo-tree-visualizer-diff) 
                    (setq-default undo-tree-visualizer-diff t)))

                    ;; 项目管理
(use-package projectile 
  :ensure t)

  ;; 看英语文档神器
(use-package english-teacher
  :quelpa ((english-teacher :fetcher github :repo "loyalpartner/english-teacher.el"))
  :custom
  (english-teacher-backend 'baidu)
  (english-teacher-show-result-function 'english-teacher-eldoc-show-result-function)
  :hook ((Info-mode
		  Man-mode
		  Woman-mode
		  ;; help-mode
		  ) . english-teacher-follow-mode))
;; google翻译工具
(use-package go-translate
  :ensure t
  :config
  (setq go-translate-base-url "https://translate.google.cn")
  (setq go-translate-local-language "zh-CN")
  (setq go-translate-token-current (cons 430675 2721866130)))


;; 管理员模式编辑
(use-package sudo-edit
  :ensure t)

;; 用posframe在dired模式下显示文件内容
(use-package dired-posframe
  :ensure t
  :custom
  (dired-posframe-size-limit (* 100 1024 102400))
  :bind((:map dired-mode-map)
		("C-*" . dired-posframe-mode)))
;; 更改窗格布局
(use-package rotate
  :ensure t)

;; 命令日志
(use-package command-log-mode
  :ensure t)

;; 窗口布局恢复
;(use-package winner-mode
;  :hook (after-init . winner-mode)
;  :bind (:map winner-mode-map
;              ("C-c H" . 'winner-undo)
;              ("C-c L" . 'winner-redo)))


;; ASCII艺术字
(use-package figlet
  :ensure t
  :config
  (setq figlet-default-font "standard"))

;; 写作模式，让你专注与写作状态
(use-package writeroom-mode
  :ensure t
  :hook (writeroom-mode . (lambda () (progn
									   (nlinum-mode -1)
									   (git-gutter-mode -1)
									   (toggle-truncate-lines -1)))))

;; 关闭鼠标功能
(use-package disable-mouse
  :disabled
  :ensure t
  :hook (after-init . (lambda ()
						(global-disable-mouse-mode 1))))

;; 管理生词工具-本配置文件作者写的插件
;; (use-package shengci
;;   :ensure t
;;   :quelpa (shengci
;;            :fetcher github
;;            :repo "EvanMeek/shengci.el"
;;            :files "*.el"))

(provide 'init-tools)