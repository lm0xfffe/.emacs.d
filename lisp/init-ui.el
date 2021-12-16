;;; init-ui.el --- settings for Emacs UI -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: https://github.com/cabins
;; Keywords:

;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

; 最大化窗口
(toggle-frame-maximized)

;; 高亮当前行
(global-hl-line-mode 1)

;; adjust the fonts
(defun available-font (font-list)
  "Get the first available font from FONT-LIST."
  (catch 'font
    (dolist (font font-list)
      (if (member font (font-family-list))
	  (throw 'font font)))))

(setq
 ;; 英文字体
 en-font-name "Cascadia Code" ;Sarasa Mono SC"
 en-font-style "ExtraLight" ;Regular"
 en-font-size 17
 ;; 中文字体
 zh-font-name "Yuanti SC" ;Kaiti SC" ;Sarasa Mono SC"
 zh-font-style "Regular"
 zh-font-size 15)

;;;###autoload
(defun cabins/setup-font ()
  "Font setup."

  (interactive)
  (let* ((efl '("Cascadia Code" "Source Code Pro" "JetBrains Mono" "Courier New" "Monaco" "Ubuntu Mono"))
	 (cfl '("Yuanti SC" "黑体" "STHeiti" "STKaiti"))
	 (cf (available-font cfl))
	 (ef (available-font efl)))
    (when ef
      (dolist (face '(default fixed-pitch fixed-pitch-serif variable-pitch))
	;(set-face-attribute face nil :family ef)
  (set-face-attribute 'default nil
						  :font (font-spec
								 :name en-font-name
								 :style en-font-style
								 :size en-font-size))
  ))
    (when cf
      (dolist (charset '(kana han cjk-misc bopomofo))
	;(set-fontset-font t charset cf)
  (set-fontset-font t 'han (font-spec
                                :name zh-font-name
                                :style zh-font-style
                                :size zh-font-size))
  )
      (setq face-font-rescale-alist
	    (mapcar (lambda (item) (cons item 1.2)) cfl)))))

;; settings for daemon mode
(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (cabins/setup-font))))
  (add-hook 'after-init-hook #'cabins/setup-font))


(defun font-size-adjust (step)
  "修改全局字体大小"
  (interactive "n步长:")
  (unless step
    (setq step 1))
  (let ((en-size en-font-size)
        (zh-size zh-font-size))
    (setq en-size (+ en-size step)
          zh-size (+ zh-size step)
          en-font-size en-size
          zh-font-size zh-size)
    (progn
	  (set-face-attribute 'default nil
						  :font (font-spec
								 :name en-font-name
								 :style en-font-style
								 :size en-font-size))
      (set-fontset-font t 'han (font-spec
                                :name zh-font-name
                                :style zh-font-style
                                :size zh-font-size)))))
(defun font-size-increase ()
  "增加字体大小"
  (interactive)
  (font-size-adjust 1))
(defun font-size-decrease ()
  "减少字体大小"
  (interactive)
  (font-size-adjust -1))

(defun font-size-orginal ()
  "复原字体大小"
  (interactive)
  (setq evan/en-font-size original-en-font-size
        evan/zh-font-size original-zh-font-size)
  (font-size-adjust 0))
(provide 'init-ui)

;;; init-ui.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
