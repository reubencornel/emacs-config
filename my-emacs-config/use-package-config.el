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
    (global-set-key (kbd "M-s o") 'helm-occur)
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

(use-package plantuml-mode
  :ensure t
  :mode ("\\.uml$" . plantuml-mode)
  :config
  (progn
    (setq plantuml-jar-path "~/bin/plantuml.jar")))

(use-package color-theme-modern
  :ensure t)

(use-package magit
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("\\.md" . markdown-mode))

(use-package eshell
  :preface
  (defun my/truncate-eshell-buffers ()
    "Truncates all eshell buffers"
    (interactive)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
	(set-buffer buffer)
	(when (eq major-mode 'eshell-mode)
	  (eshell-truncate-buffer)))))
  
  :config
  (progn
    (setq eshell-buffer-maximum-lines 20000)
    (setq my/eshell-truncate-timer
	  (run-with-idle-timer 5 t #'my/truncate-eshell-buffers))))

(use-package eshell-git-prompt
  :ensure t
  :init
  (eshell-git-prompt-use-theme 'robbyrussell))
