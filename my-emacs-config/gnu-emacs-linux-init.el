;;; Gnu Emacs init

;;(add-to-list 'load-path (expand-file-name "~/emacs"))
;;(require 'osx-itunes)
;;(load (expand-file-name "~/my-key-bindings.el"))


;; Custom os code
(defvar my-notifier-path "/usr/bin/zenity")

(defun my-appt-send-notification (title msg)
  (shell-command (concat my-notifier-path " --warning " " --text " msg " &")))

;; Custom modes
;(load-config 'haskell-mode)
(load-config 'tramp)
(load-config 'cl-config)
(load-config 'linux-org-mode-config)
;(load-config 'itunes-config)
;(load-config 'ocaml-support)
(load-config 'c-mode-config)
(load-config 'ido-mode)
;(load-config 'icicles-mode-config)
(load-config 'htmlize)
;(load-config 'org-publish-config)
;(load-config 'article-file-support)
(load-config 'color-theme)
(load-config 'nav)
;(load-config 'twit)
(load-config 'anything-mode)
;(load-config 'emacs-graphical-font)
(load-config 'thin-cursor-config)
;(load-config 'select-enable-clipboard)
(load-config 'cls-files-config)
(load-config 'yasnippet)
(load-config 'magit)
;(load-config 'nxhtml-mode-config)
;(load-config 'mmm-mode-config)
;(load-config 'emacs-graphical-font-linux)
(load-config 'auto-save-config)

(load (expand-file-name "~/emacs/my-emacs-config/my-key-bindings.el"))
