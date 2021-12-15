
;; counsel提供对项目管理的支持
(use-package 
  counsel-projectile 
  :ensure t 
  :hook ((counsel-mode . counsel-projectile-mode)) 
  :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)) 
  :bind (:map leader-key
              ("p" . #'projectile-command-map)))

(use-package ivy-fuz
  :ensure t)

;;;###autoload
(defun ivy-telega-chat-highlight (chat)
  (let ((unread (funcall (telega--tl-prop :unread_count) chat))
        (title (telega-chat-title chat 'with-identity))
        (not-muted-p (not (telega-chat-muted-p chat)))
        (mentions (funcall (telega--tl-prop :unread_mention_count) chat)))

    (if (and not-muted-p (> (+ unread mentions) 0))
        (ivy-append-face (format "%s %d@%d" title unread mentions) 'ivy-highlight-face)
      title)))


(use-package ivy-rich
  :ensure t
  :hook (after-init . ivy-rich-mode))

(use-package all-the-icons-ivy-rich
  :ensure t
  :after ivy-rich
  :config
  (all-the-icons-ivy-rich-mode 1))


(provide 'init-ivy)
