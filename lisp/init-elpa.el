;;; init-elpa.el --- initialize the elpa repository -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins

;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;;; settings for package archives
(setq package-archives '(("melpa" . "http://mirrors.bfsu.edu.cn/elpa/melpa/")
                         ("gnu" . "http://mirrors.bfsu.edu.cn/elpa/gnu/"))
      package-check-signature nil
      load-prefer-newer t)
(require 'package)

;;; initialize the packages, avoiding a re-initialization
(unless (bound-and-true-p package--initialized) ;; To avoid warnings on 27
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

;; settings for use-package package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; configure use-package prior to loading it
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-expand-minimally t)
  (require 'use-package))

;; 安装quelpa包管理器（用于安装github上的插件）
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade))
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git")))
(require 'quelpa-use-package)

;(setq quelpa-upgrade-interval 7
;      quelpa-update-melpa-p nil)
(setq quelpa-update-melpa-p nil)

(use-package gnu-elpa-keyring-update)
(use-package diminish)
(use-package delight)

(provide 'init-elpa)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-elpa.el ends here