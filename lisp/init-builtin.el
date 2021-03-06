;;; init-builtin.el --- initialize the builtin plugins -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: https://github.com/cabins
;; Keywords:

;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:
;;; Sorted by Alphbet order

;; Abbrev
(setq-default abbrev-mode t)

;; auto save
;; `save-some-buffers' is provided by files.el (builtin)
;; `pulse-momentary-highlight-one-line' is provided by pulse.el (builtin)
(use-package pulse-and-save
  :ensure nil
  :init
  (defun pulse-save-buffers (&rest args)
    (save-some-buffers t)
    (pulse-momentary-highlight-one-line (point)))
  ;; auto save when frame lose focus, Alt-Tab
  (add-function :after after-focus-change-function #'pulse-save-buffers)
  ;; auto save when buffer changed
  (dolist (command '(other-window
                     switch-to-buffer
                     next-buffer
                     previous-buffer))
    (advice-add command :after #'pulse-save-buffers)))

;; auto revert
;; `global-auto-revert-mode' is provided by autorevert.el (builtin)
(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

;; Delete Behavior
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'after-init-hook 'delete-selection-mode)

;; Electric-Pair
(add-hook 'after-init-hook 'electric-indent-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-layout-mode)

;; Flymake
(add-hook 'prog-mode-hook 'flymake-mode)

;; HideShow Minor Mode
(use-package hideshow
  :init (add-hook 'hs-minor-mode-hook (lambda () (diminish 'hs-minor-mode)))
  :hook (prog-mode . hs-minor-mode))

;; ibuffer
(use-package ibuffer
  ;; :disabled
  :init (defalias 'list-buffers 'ibuffer))

;; Ido ( instead of ivy & counsel & swiper)
(setq-default ido-auto-merge-work-directories-length -1
	      ido-enable-flex-matching t
	      isearch-lazy-count t
	      lazy-count-prefix-format "%s/%s: ")
(setq completion-ignored-extensions '(".o" ".elc" "~" ".bin" ".bak" ".obj" ".map" ".a" ".ln" ".class"))
(fido-mode t)

;; Line Number
;; this package introduced in Emacs 26, so only enabled when 26+
(use-package display-line-numbers
  :if (> emacs-major-version 26)
  :hook (prog-mode . display-line-numbers-mode))

;; Org Mode
(setq org-hide-leading-stars t
      org-startup-indented t)

;; Parentheses
(use-package paren
  :ensure nil
  :config (setq-default show-paren-style 'mixed
                        show-paren-when-point-inside-paren t
                        show-paren-when-point-in-periphery t)
  :hook (prog-mode . show-paren-mode))

;; Recent Files
(add-hook 'after-init-hook (lambda ()
			     (recentf-mode 1)
			     (add-to-list 'recentf-exclude '("~\/.emacs.d\/elpa\/"))))
(setq-default recentf-max-menu-items 20
	      recentf-max-saved-items 20)
(setq recentf-save-file "~/.emacs.d/var/recentf")
(setq x-select-enable-clipboard t)
(set-default 'truncate-lines t)


;; ??????????????????????????????
;(delete-selection-mode +1)

;; ????????????????????????????????????
(use-package saveplace
  :ensure t
  :hook (after-init . (lambda () (save-place-mode t))))

;; ????????????
(setq make-backup-files nil auto-save-default nil)

;; ????????????????????????????????????????????????????????????
(setq create-lockfiles nil)
;; ??????????????????gc
(setq inhibit-compacting-font-caches nil)

;; ?????????????????????
(setq ring-bell-function 'ignore blink-cursor-mode nil)

;; ?????????????????????UTF-8
(set-charset-priority 'unicode) 
(setq locale-coding-system   'utf-8)    ; pretty
(set-terminal-coding-system  'utf-8)    ; pretty
(set-keyboard-coding-system  'utf-8)    ; pretty
(set-selection-coding-system 'utf-8)    ; please
(prefer-coding-system        'utf-8)    ; with sugar on top
(setq default-process-coding-system '(utf-8 . utf-8))
;; ?????????????????????
;; ?????????????????????????????????
(global-set-key (kbd "RET") 'newline-and-indent)
;; ???????????????????????????
(global-set-key (kbd "S-<return>") 'comment-indent-new-line)

;; ???????????????????????????
(setq mouse-yank-at-point nil)

;; ????????????????????????
(setq-default fill-column 80)

;; ???'_'???????????????????????????
(add-hook 'after-change-major-mode-hook (lambda () 
                                          (modify-syntax-entry ?_ "w")))
;; "-" ??????)
(add-hook 'after-change-major-mode-hook (lambda () 
                                          (modify-syntax-entry ?- "w")))
;; ?????????????????????
(setq-default indent-tabs-mode nil)
;; ???????????????
(setq-default tab-width 4)

;; ?????????????????????
(show-paren-mode 1)
;; Save Place
(save-place-mode 1)

;; ????????????
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
	;; ????????????
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0 :foreground "magenta")))))))
;; ?????????????????????
(use-package swiper
  :ensure t
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)
   ("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file))
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

(use-package amx
  :ensure t
  :config
  ;; ??????amx?????????????????????
  (setq amx-save-file "~/.emacs.d/var/amx-items"))
;; ???????????????????????????????????????
(use-package counsel
  :ensure t
  :bind
  (("C-x C-r" . 'counsel-recentf) 
   ("C-x d" . 'counsel-dired))
  :config
  ;; ????????? rg ??????
  ;; (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s")
  (setq counsel-rg-base-command (list "rg"
									  "-M" "240"
									  "--with-filename" "--no-heading" "--line-number" "--color"
									  "never" "%s"
									  "-g" "!package-config.org"
									  "-g" "!site-lisp"
									  "-g" "!doc"
									  "-g" "!themes"
                                      "-g" "!quelpa"
                                      "-g" "!etc-cache"))
  (setq counsel-fzf-cmd "fd -I --exclude={site-lisp,etc/snippets,themes,/eln-cache,/var,/elpa,quelpa/,/url,/auto-save-list,.cache,doc/} --type f | fzf -f \"%s\" --algo=v1")
  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy)))

;; ???????????????????????????
(use-package avy
  :defer 0
  :ensure t
  :bind (("M-g :" . 'avy-goto-char)
         ("M-g '" . 'avy-goto-char-2)
         ("M-g \"" . 'avy-goto-char-timer)
         ("M-g f" . 'avy-goto-line)
         ("M-g w" . 'avy-goto-word-1)
         ("M-g e" . 'avy-goto-word-0)))
;;; ??????avy????????????
(use-package link-hint
  :ensure t
  :bind (("M-g l" . link-hint-open-link)))

;; ????????????
(use-package auto-save
  :quelpa ((auto-save :fetcher github :repo "manateelazycat/auto-save"))
  :config
  (setq auto-save-slient t
        auto-save-idle 5) ;; 5s ??????????????? 
  (auto-save-enable))
  ;; so-long
(use-package so-long
  :config
  (push '(read-only-mode ) so-long-variable-overrides))

;; ???????????????~/.emacs.d/
(cd "~/.emacs.d/")
;; only use spaces instead of TAB, use C-q TAB to input the TAB char
(setq-default indent-tabs-mode nil)

;; Diminish Builtins
(dolist (elem '(abbrev-mode eldoc-mode))
  (diminish elem))
(add-hook 'hs-minor-mode-hook (lambda () (diminish 'hs-minor-mode)))

(provide 'init-builtin)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-builtin.el ends here
