;;; Gnu Emacs init

;;(add-to-list 'load-path (expand-file-name "~/emacs"))
;;(require 'osx-itunes)
;;(load (expand-file-name "~/my-key-bindings.el"))

(load-config 'haskell-mode)
(load-config 'tramp)
(load-config 'cl-config)
(load-config 'org-mode-config)
;(load-config 'itunes-config)
(load-config 'ocaml-support)
(load-config 'c-mode-config)
(load-config 'ido-mode)
(load-config 'htmlize)
(load-config 'org-publish-config)
(load-config 'article-file-support)
;(load-config 'color-theme)
(load-config 'twit)
;(load-config 'emacs-graphical-font)
(load-config 'thin-cursor-config)


(load (expand-file-name "~/emacs/my-emacs-config/my-key-bindings.el"))