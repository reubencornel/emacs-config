;;; Gnu Emacs init

;;(add-to-list 'load-path (expand-file-name "~/emacs"))
;;(require 'osx-itunes)
;;(load (expand-file-name "~/my-key-bindings.el"))

;(load-config 'haskell-mode)

;; Custom os code
(defvar my-notifier-path   "/usr/local/bin/growlnotify")

(defun my-appt-send-notification (title msg)
  (save-window-excursion
    (shell-command (concat my-notifier-path " -m " msg " -t " title " -w &"))) )

(package-initialize)
;; modes
(load-config 'tramp)
(load-config 'org-mode-config)
(load-config 'itunes-config)
(load-config 'c-mode-config)
(load-config 'ido-mode)
(load-config 'htmlize)
(load-config 'org-publish-config)
(load-config 'article-file-support)
(load-config 'color-theme)
(load-config 'twit)
(load-config 'thin-cursor-config)
(load-config 'magit)
(load-config 'yasnippet)
(load-config 'anything-mode)
(load-config 'nav)
(load-config 'text-mode-config)

(load (expand-file-name "~/emacs/my-emacs-config/my-key-bindings.el"))
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
;(set-frame-font "Inconsolata 19")
