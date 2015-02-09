;;; Gnu Emacs init

;;(add-to-list 'load-path (expand-file-name "~/emacs"))
;; Custom os code
(defvar my-notifier-path "/usr/bin/zenity")

(defun my-appt-send-notification (title msg)
  (shell-command (concat my-notifier-path " --warning " " --text " msg " &")))

;; Custom modes
(load-config 'tramp)
(load-config 'linux-org-mode-config)
(load-config 'c-mode-config)
(load-config 'ido-mode)
(load-config 'htmlize)
(load-config 'color-theme)
(load-config 'nav)
(load-config 'anything-mode)
(load-config 'thin-cursor-config)
(load-config 'cls-files-config)
(load-config 'yasnippet)
(load-config 'magit)

(load (expand-file-name "~/emacs/my-emacs-config/my-key-bindings.el"))
