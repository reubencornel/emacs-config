;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs lisp load paths
;;


;; (defconfig nav
;;   (require 'nav))

;; (defconfig sentence-highlight-mode
;;   (require 'sentence-highlight))

;; (defconfig nrepl
;;   (require 'paredit)
;;   (require 'clojure-mode)
;;   (require 'nrepl))

;;;;;; Haskell mode
;; (defconfig haskell-mode
;;   (require 'haskell-mode)
;;   (require 'inf-haskell)
;;   (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;   (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;   (setf haskell-program-name "/Library/Frameworks/GHC.framework/Versions/Current/usr/bin/ghci")
;;   (setq auto-mode-alist (cons '("\\.hs$" . haskell-mode) auto-mode-alist)))



(defconfig org-mode-config
  (require 'org-install)
  (check-and-install-if-absent 'org-bullets)
  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-hide-leading-stars 't)
  (setq org-log-done 'time)
  (add-hook 'after-save-hook 'sync-index-org)

  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
		(sequence "TOBUY(b)" "TOPACK(p)" "|" "BOUGHT(g)" "PACKED")
		(sequence "QUESTION(q)" "|" "ANSWERED(a)"))))

  (setq org-todo-state-tags-triggers
	(quote (("CANCELLED" ("CANCELLED" . t))
		("WAITING" ("WAITING" . t))
		(done ("WAITING"))
		("TODO" ("WAITING") ("CANCELLED") )
		("NEXT" ("WAITING") ("CANCELLED") )
		("DONE" ("WAITING") ("CANCELLED") ))))

  (setq org-agenda-custom-commands
        '(("wt" tags-todo "+WORK+TASKS")
          ("ht" tags-todo "+HOME+TASKS")
          ("wp" tags-todo "+WORK+PROJECTS")
          ("hp" tags-todo "+HOME+PROJECTS")
	  ("wq" todo "+QUESTION")
  	  ("d" "Daily Agenda" ((agenda "" ((org-agenda-span 1)
					   (org-agenda-overriding-header "Daily Agenda")))
			       (todo "NEXT" ((org-agenda-overriding-header "Next Items")))
			       (todo "QUESTION" ((org-agenda-overriding-header "Open Questions")))
			       (todo "WAITING" ((org-agenda-overriding-header "Waiting tasks")))
			       (tags-todo "+UNFILED" ((org-agenda-overriding-header "Unfiled Tasks")))
			       (stuck "" ((org-use-tag-inheritance nil)
					  (org-agenda-overriding-header "Stuck Projecs")))))))

  (setq org-stuck-projects
	'("+PROJECT-DONE-TEMPLATE" ("NEXT") ()
	  "\\<IGNORE\\>"))

  (setq org-directory "~/Dropbox")
  (setq org-mobile-inbox-for-pull "~/Dropbox/inbox.org")

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  (setq org-directory (expand-file-name "~/Dropbox"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-default-journal-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
	   "* TODO %^{entry} :UNFILED:\n%T\n %?\n %a")
	  ("r" "Lookup Entry in region" entry (file+headline org-default-notes-file "Lookup")
	   "* %i :LOOKUP:\n")
	  ("l" "Lookup Entry" entry (file+headline org-default-notes-file "Lookup")
	   "* %?  :LOOKUP:\n %i \n")
	  ("j" "Journal" entry (file+datetree org-default-journal-file)
	   "* %^{title} %^G \n\n%?\n\nEntered on %U\n %i\n")))

  (setq org-todo-keyword-faces
	(quote (("TODO" :foreground "red" :weight bold)
		("NEXT" :foreground "blue" :weight bold)
		("DONE" :foreground "forest green" :weight bold)
		("WAITING" :foreground "orange" :weight bold)
		("HOLD" :foreground "magenta" :weight bold)
		("CANCELLED" :foreground "forest green" :weight bold)
		("QUESTION" :foreground "orange" :weight bold)
		("ANSWERED" :foreground "forest green" :weight bold)))))

(defconfig c-mode-config
  (setq basic-c-offset 8)
  (add-hook 'c-mode-hook 'my-c-mode-hook))


(defconfig ido-mode
  (ido-mode 1)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t))

;; Article file support
(defconfig article-file-support
  (setq auto-mode-alist (cons '("\\.article$" . html-mode) auto-mode-alist)))


(defconfig select-enable-clipboard
  (setq x-select-enable-clipboard t))

(defconfig text-mode-config
  (fringe-mode -1))

(defconfig magit
  (check-and-install-if-absent 'magit)
  (require 'magit))

(defconfig markdown-mode
  (check-and-install-if-absent 'markdown-mode)
  (require 'markdown-mode)
  (setq auto-mode-alist
        (cons '("\\.md" . markdown-mode) auto-mode-alist)))

(defconfig firacode-config
  ;; ;; Fira code specific settings.
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
		 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
		 (36 . ".\\(?:>\\)")
		 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
		 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
		 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
		 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
		 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
		 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
		 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
		 (48 . ".\\(?:x[a-zA-Z]\\)")
		 (58 . ".\\(?:::\\|[:=]\\)")
		 (59 . ".\\(?:;;\\|;\\)")
		 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
		 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
		 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
		 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
		 (91 . ".\\(?:]\\)")
		 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
		 (94 . ".\\(?:=\\)")
		 (119 . ".\\(?:ww\\)")
		 (123 . ".\\(?:-\\)")
		 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
		 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
		 )
	       ))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
			    `([,(cdr char-regexp) 0 font-shape-gstring])))))

(defconfig color-theme
  (check-and-install-if-absent 'color-theme)
  (check-and-install-if-absent 'zenburn-theme)
  (check-and-install-if-absent 'solarized-theme)
  (require 'color-theme))

(defconfig helm-config
  (check-and-install-if-absent 'helm-core)
  (check-and-install-if-absent 'helm)
  (check-and-install-if-absent 'helm-org-rifle)
  (require 'helm)
  (require 'helm-config)
  (require 'helm-org-rifle)

  (global-set-key (kbd "C-c h") 'helm-command-prefix)

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

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action)

  (helm-mode 1)
  )


(defconfig plantuml-mode-config
  (check-and-install-if-absent 'plantuml-mode)
  (require 'plantuml-mode)
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode)))
