
(load-config 'w3m-config)
(load-config 'cl-config)
(load-config 'org-mode-config)
(load-config 'itunes-config)
(load-config 'ocaml-support)
(load-config 'c-mode-config)
(load-config 'ido-mode)
(load-config 'article-file-support)
(load-config 'color-theme)
(load-config 'twit)
(load-config 'thin-cursor-config)
(load-config 'icicles-mode-config)
(load-config 'carbon-emacs-22-font-config)


(load (expand-file-name "~/emacs/my-emacs-config/my-key-bindings.el"))
(global-set-key [(meta return)] 'toggle-fullscreen) 