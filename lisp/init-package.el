;;; init-package.el --- initialize the plugins -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: https://github.com/cabins
;; Keywords:

;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;; All the icons
;; If the icons are not displayed correctly although all-the-icons fonts are installed correctly
;; please install the non-free font Symbola. This issue usually occurs on Windows.
;; [Refs] https://github.com/seagle0128/doom-modeline
;;(use-package all-the-icons)

;; Settings for company
;(use-package company
;  :diminish
;  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
;  :init (add-hook 'after-init-hook 'global-company-mode))
;; 著名的Emacs补全框架
(use-package company
  :hook ((prog-mode sly-mrepl-mode) . company-mode)
  :init (setq company-tooltip-align-annotations t company-idle-delay 0.1 company-echo-delay 0
              company-minimum-prefix-length 2 company-require-match nil company-dabbrev-ignore-case
              nil company-dabbrev-downcase nil company-show-numbers t)
  :config
  :bind (:map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-n" . #'company-select-next)
              ("C-p" . #'company-select-previous)
              (:map leader-key
        ("c s" . #'company-yasnippet)))
  )

;; Settings for exec-path-from-shell 耗时太多
;(use-package exec-path-from-shell
;  :defer nil
;  :if (memq window-system '(mac ns x))
;  :init (exec-path-from-shell-initialize))

;; Settings for projectile (use builtin project in Emacs 28)
(use-package projectile
  :when (< emacs-major-version 28)
  :diminish " Proj."
  :init (add-hook 'after-init-hook 'projectile-mode)
  :config (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Show the delimiters as rainbow color
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
(use-package highlight-parentheses
  :init (add-hook 'prog-mode-hook 'highlight-parentheses-mode))

;; Settings for which-key - suggest next key
(use-package which-key
  :defer nil
  :diminish
  :init (which-key-mode))

;; Settings for yasnippet
(use-package yasnippet
  :ensure t
  :defer 2
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/etc/snippets")))
(use-package yasnippet-snippets)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ui ;;;;;;;;;;;;;;;;;;;;;;;;;
;; 切换buffer焦点时高亮动画
(use-package beacon
  :disabled
  :ensure t
  :hook (after-init . beacon-mode))

;; 主题包
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;;(doom-themes-neotree-config)
  ;; or for treemacs users
  ;;(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;;(doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


(use-package modus-operandi-theme
  :ensure t)

(use-package modus-vivendi-theme
  :ensure t )

(use-package lab-themes
  :ensure t)


(use-package lazycat-theme
  :quelpa (lazycat-theme
            :fetcher github
            :repo "manateelazycat/lazycat-theme"))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  (message "%s" "start dash board")
  ;; 设置标题
  (setq dashboard-banner-logo-title
        "欢迎您使用此Emacs配置文件，有任何问题可加QQ群:                  ")
  ;; 设置banner
  ;(setq dashboard-startup-banner "~/.emacs.d/var/banner/evan-emacs-banner.png")
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-navigator t)
  (add-hook 'after-init-hook (lambda () (dashboard-refresh-buffer))))

(progn
  (use-package all-the-icons
	:ensure t)
  ;; dired模式图标支持
  (use-package all-the-icons-dired
	:ensure t
	:hook ('dired-mode . 'all-the-icons-dired-mode))
  ;; 表情符号
  (use-package emojify
	:ensure t
	:custom (emojify-emojis-dir "~/.emacs.d/var/emojis")
    :hook ('after-init . global-emojify-mode))
;;  (emojify-set-emoji-style '(prettify-symbol ascii unicode github))
  ;; 浮动窗口支持
  (use-package posframe
	:ensure t
	:custom
	(posframe-mouse-banish nil)))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; 竖线
(use-package page-break-lines
  :ensure t
  :hook (after-init . global-page-break-lines-mode)
  :config
  (set-fontset-font "fontset-default"
                  (cons page-break-lines-char page-break-lines-char)
                  (face-attribute 'default :family))
  (let ((table (make-char-table nil)))                   ;; make a new empty table
    (set-char-table-parent table char-width-table)       ;; make it inherit from the current char-width-table
    (set-char-table-range table page-break-lines-char 1) ;; let the width of page-break-lines-char be 1
    (setq char-width-table table)))

;; 让info帮助信息中关键字有高亮
(use-package info-colors
  :ensure t
  :hook ('Info-selection-hook . 'info-colors-fontify-node))

;; 缩进线
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'bitmap))

;; 彩虹猫进度条
(use-package nyan-mode
  :if (not (boundp 'awesome-tray-mode))
  :ensure t
  :hook (after-init . nyan-mode)
  :config
  (setq nyan-wavy-trail t
		nyan-animate-nyancat t))

;; 对齐表格
(use-package valign
  :quelpa ((valign :fetcher github :repo "casouri/valign"))
  :hook ((org-mode markdown-mode) . valign-mode)
  :config
  (setq valign-fancy-bar nil))

(provide 'init-package)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-package.el ends here
