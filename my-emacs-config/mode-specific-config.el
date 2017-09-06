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
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-hide-leading-stars 't)
  (setq org-log-done 'time)
  (add-hook 'after-save-hook 'sync-index-org)

  (setq org-agenda-custom-commands
        '(("wt" tags-todo "+WORK+TASKS")
          ("ht" tags-todo "+HOME+TASKS")
          ("wp" tags-todo "+WORK+PROJECTS")
          ("hp" tags-todo "+HOME+PROJECTS")))

  (setq org-todo-keywords '((sequence "TODO" "STARTED" "WAITING" "|" "DONE")
                            (sequence "NOTPICKEDUP" "|" "PICKEDUP")))

  (setq org-directory "~/Dropbox")
  (setq org-mobile-inbox-for-pull "~/Dropbox/inbox.org")
 
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  
  (setq org-directory (expand-file-name "~/Dropbox"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-default-journal-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
	   "* TODO %? \n %i\n %a")
	  ("r" "Lookup Entry in region" entry (file+headline org-default-notes-file "Lookup")
	   "* %i :LOOKUP:\n")
	  ("l" "Lookup Entry" entry (file+headline org-default-notes-file "Lookup")
	   "* %?  :LOOKUP:\n %i \n")
	  ("j" "Journal" entry (file+datetree org-default-journal-file)
	   "* %^{title} %^G \n\n%?\n\nEntered on %U\n %i\n"))))

;; (defconfig auto-install
;;   (require 'auto-install)
;;   (setq auto-install-directory "~/emacs/auto-install/")
;;   (setq auto-install-save-confirm nil))


;; (defconfig synonym-support
;;   (require 'synonyms)
;;   (setq synonyms-file "~/emacs/synonyms/mthesaur.txt")
;;   (setq synonyms-cache-file "~/emacs/synonyms/thesaurus.cache.txt"))

(defconfig c-mode-config
  (setq basic-c-offset 8)
  (add-hook 'c-mode-hook 'my-c-mode-hook))


(defconfig ido-mode
  (ido-mode 1)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t))

(defconfig htmlize
  (require 'htmlize))

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
