(require 'use-package)

(use-package helm
  :ensure t
  :bind    (("C-c h" .  'helm-command-prefix)
	    ("M-x" .  'helm-M-x)
	    ("C-x C-f" . 'helm-find-files)
	    ( "C-x b" . 'helm-mini)
	 :map helm-command-map
	      (("TAB" . 'helm-execute-persistent-action)
	       ("C-i" . 'helm-execute-persistent-action)
	       ("C-z" . 'helm-select-action)))
  :config
  (progn
    (require 'helm-config)
    (setq
     helm-quick-update                     t
     helm-split-window-in-side-p           t
     helm-buffers-fuzzy-matching           t
     helm-move-to-line-cycle-in-source     t
     helm-ff-search-library-in-sexp        t
     helm-scroll-amount                    8
     helm-ff-file-name-history-use-recentf t
     helm-semantic-fuzzy-match t
     helm-imenu-fuzzy-match    t)
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-unset-key (kbd "C-x c"))   
    (helm-mode 1)))

(use-package helm-org-rifle
  :ensure t
  :after (helm))

(use-package swiper-helm
  :ensure t
  :after (helm))

