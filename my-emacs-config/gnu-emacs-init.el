;;; Gnu Emacs init

;;(add-to-list 'load-path (expand-file-name "~/emacs"))
;;(load (expand-file-name "~/my-key-bindings.el"))


;; Custom os code
(defun my-appt-send-notification (title msg)
  (save-window-excursion
    (shell-command (concat my-notifier-path " -m " msg " -t " title " -w &"))) )

(package-initialize)
;; modes
(load-config 'diary-config)
(load-config 'c-mode-config)
(load-config 'org-mode-config)
(load-config 'article-file-support)
(load-config 'utf8-config)
(load-config 'nov-mode-config)

(load (expand-file-name "~/emacs/my-emacs-config/my-key-bindings.el"))
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
