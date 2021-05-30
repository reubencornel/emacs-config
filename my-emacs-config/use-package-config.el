(require 'use-package)


  (use-package selectrum
    :ensure t
    :config
    (selectrum-mode +1))

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

(use-package prescient
  :ensure t)

(use-package selectrum-prescient
  :ensure t
  :after (selectrum prescient)
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

;; Example configuration for Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ;; ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;; ("M-g o" . consult-outline)
         ;; ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ;; ("M-g i" . consult-imenu)
         ;; ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line))                 ;; required by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Selectrum, Vertico etc.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep consult-bookmark consult-recent-file
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
)

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  ;; :bind (("M-A" . marginalia-cycle)
  ;;        :map minibuffer-local-map
  ;;        ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))


(use-package embark
  :ensure t

  :bind
  (("C-S-a" . embark-act)       ;; pick some comfortable binding
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer


  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; (use-package swiper-helm
;;   :ensure t
;;   :after (helm)
;;   :defer t
;;   :config
;;   (global-set-key (kbd "C-s") 'swiper-helm))

;; (use-package helm
;;   :ensure t
;;   :bind    (("C-c h" .  'helm-command-prefix)
;; 	    ("M-x" .  'helm-M-x)
;; 	    ("C-x C-f" . 'helm-find-files)
;; 	    ( "C-x b" . 'helm-mini)
;; 	 :map helm-command-map
;; 	      (("TAB" . 'helm-execute-persistent-action)
;; 	       ("C-i" . 'helm-execute-persistent-action)
;; 	       ("C-z" . 'helm-select-action)))
;;   :config
;;   (progn
;;     (require 'helm-config)
;;     (setq
;;      helm-quick-update                     t
;;      helm-split-window-in-side-p           t
;;      helm-buffers-fuzzy-matching           t
;;      helm-move-to-line-cycle-in-source     t
;;      helm-ff-search-library-in-sexp        t
;;      helm-scroll-amount                    8
;;      helm-ff-file-name-history-use-recentf t
;;      helm-semantic-fuzzy-match t
;;      helm-imenu-fuzzy-match    t)
;;     (global-set-key (kbd "C-c h") 'helm-command-prefix)
;;     (global-set-key (kbd "M-x") 'helm-M-x)
;;     (global-set-key (kbd "M-s o") 'helm-occur)
;;     (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; ;    (global-set-key (kbd "C-x b") 'helm-mini)
;;     (global-unset-key (kbd "C-x c"))
;;     (helm-mode 1)))

;; (use-package helm-org-rifle
;;   :ensure t
;;   :defer t
;;   :after (helm))

(use-package plantuml-mode
  :ensure t
  :defer t
  :mode ("\\.uml$" . plantuml-mode)
  :config
  (progn
    (setq plantuml-jar-path "~/bin/plantuml.jar")))

(use-package color-theme-modern
  :defer t
  :ensure t)

(use-package magit
  :defer t
  :ensure t)

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.md" . markdown-mode))

(use-package eshell
  :defer t
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
    (setq eshell-scroll-to-bottom-on-input 'all)
    (setq eshell-buffer-maximum-lines 2000)
    (setq my/eshell-truncate-timer
	  (run-with-idle-timer 5 t #'my/truncate-eshell-buffers))))

(use-package eshell-git-prompt
  :ensure t
  :init
  (eshell-git-prompt-use-theme 'robbyrussell))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SCALA CONFIG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (use-package sbt-mode
;;   :defer t
;;   :pin melpa)

;; (use-package scala-mode
;;   :pin melpa
;;   :interpreter
;;   ("scala" . scala-mode)
;;   ("sc" . scala-mode))


(use-package org
  :defer t
  :ensure org-plus-contrib
  :bind  (:map org-mode-map
               ([f3] . org-narrow-to-subtree)
               ([f4] . widen)
          )
  :custom
  (org-hide-leading-stars 't)
;  (setq org-use-property-inheritance nil)
					;(org-log-done 'time)
  (org-agenda-text-search-extra-files ;; This variable instructs org agenda to search through the archives
   '(agenda-archives "~/Dropbox/org/work.org_archive" "~/Dropbox/org/main.org_archive"))
  (org-refile-use-outline-path 3)
  (org-agenda-files
   '("~/Dropbox/org/log.org" "~/Dropbox/org/notes.org" "~/Dropbox/org/inbox.org" "~/Dropbox/org/work.org" "~/Dropbox/org/main.org" "~/Dropbox/org/slipbox.org" "~/Dropbox/org/slipbox_raw.org" "~/Dropbox/org/someday.org" "~/Dropbox/org/daily.org"))
  (org-startup-folded t)
  (org-export-with-section-numbers nil)
  (org-export-with-toc nil)
  (org-pretty-entities t)
  (org-id-link-to-org-use-id t)
  (org-hide-emphasis-markers t)
  (org-reverse-note-order t)
  (org-log-into-drawer "LOGBOOK")
  (org-clock-persist t)
  (org-use-speed-commands t)
  (org-clock-idle-time 60)
  (org-emphasis-regexp-components '("-[:space:]('\"{" "-[:space:].,:!?;'\")}\\[" "\x200B" "." 1))
  (org-clock-history-length 35)
  (org-clock-in-resume t)
  (org-image-actual-width '(500))
  (org-completion-use-ido nil)
  (org-outline-path-complete-in-steps nil)
  (org-clock-out-remove-zero-time-clocks t)
  (org-id-method 'uuidgen)
  (org-enforce-todo-dependencies t)
  (org-hide-leading-stars t)
  (org-refile-targets '((org-agenda-files :maxlevel . 5)))
  (org-image-toggle-inline  t)
  (org-catch-invisible-edits 'show-and-error)
  (org-log-done 'note)
  (org-agenda-block-separator ? )
  (org-log-reschedule 'note)
  (org-log-redeadline 'note)
  (org-log-delschedule 'note)
  (org-log-deldeadline 'note)
  (org-emphasis-alist '(("*" bold) ("/" italic) ("_" underline) ("=" org-verbatim verbatim) ("~" (:background "yellow1" :weight bold)) ("+" (:strike-through t))))
  ;; Setup log note templates. Add "to [new date]" in reschedule and redeadline
  (org-log-note-headings '((done        . "CLOSING NOTE %t")
                           (state       . "State %-12s from %-12S %t")
                           (note        . "Note taken on %t")
                           (reschedule  . "Schedule changed on %t: %S -> %s")
                           (delschedule . "Not scheduled, was %S on %t")
                           (redeadline  . "Deadline changed on %t: %S -> %s")
                           (deldeadline . "Removed deadline, was %S on %t")
                           (refile      . "Refiled on %t")
			   (clock-out . "")))
  
  (org-todo-keywords
   (quote ((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "DEFERRED(e)" "|" "DONE(d!)" "CANCELLED(c@)")
	   (sequence "TOBUY(b)" "TOPACK(p)" "|" "BOUGHT(g)" "PACKED")
	   (sequence "TOREAD(r)" "|" "READ")
	   (sequence "QUESTION(q)" "|" "ANSWERED(a@)")
	   (sequence "TOREVIEW" "INREVIEW" "REWORK" "|" "APPROVED"))))
  
  (org-todo-keyword-faces
   (quote (("TODO" :foreground "red1" :weight bold)
	   ("NEXT" :foreground "turquoise" :weight bold)
	   ("DONE" :foreground "light green" :weight bold)
	   ("WAITING" :foreground "DarkOrange2" :weight bold)
	   ("DEFERRED" :foreground "DarkOrange2" :weight bold)
	   ("TOREAD" :foreground "DarkOrange2" :weight bold)
	   ("HOLD" :foreground "magenta" :weight bold)
	   ("CANCELLED" :foreground "light green" :weight bold)
	   ("READ"  :foreground "light green" :weight bold)
	   ("QUESTION" :foreground "DarkOrange2" :weight bold)
	   ("ANSWERED" :foreground "light green" :weight bold)
	   ("TOREVIEW" :foreground "red1" :weight bold)
	   ("INREVIEW" :foreground "DarkOrange2" :weight bold)
	   ("REWORK"  :foreground "magenta" :weight bold)
	   ("APPROVED" :foreground "light green" :weight bold)
	   )))

  (org-todo-state-tags-triggers
   (quote (("CANCELLED" ("CANCELLED" . t))
	   ("WAITING" ("WAITING" . t))
	   (done ("WAITING"))
	   ("DEFERRED" ("DEFERRED"))
	   ("TODO" ("WAITING") ("CANCELLED") )
	   ("NEXT" ("WAITING") ("CANCELLED") )
	   ("DONE" ("WAITING") ("CANCELLED") ))))

  (unscheduled-tasks-search-string "+TODO=\"TODO\"-SCHEDULED={.+}-DEADLINE={.+}-TEMPLATE-IGNORE_UNSCHEDULED")

  (org-agenda-custom-commands
   '(
	("a" "All Agenda" ((agenda "plain" ((org-agenda-span 1)
					 (org-super-agenda-groups
					  '((:name "Schedule"
					           :time-grid t)
					    (:name "Today"
					           :scheduled today)
					    (:habit t)
					    (:name "Due today"
					           :deadline today)
					    (:name "Overdue"
					           :deadline past)
					    (:name "Due soon"
					           :deadline future)))))))
	("q" tags-todo "TODO=\"QUESTION\"")
	("i" "Inbox Review" ((tags-todo "TODO=\"TODO\"|TODO=\"NEXT\""
				        ((org-agenda-overriding-header "Inbox Tasks")
					 (org-agenda-files '("~/Dropbox/org/inbox.org"))))
			     (org-ql-block '(and (not (todo "TODO"))
			   		         (not (todo "DONE")))
			   		   ((org-ql-block-header "Notes")
					    (org-agenda-overriding-header "Other Items")
			   		    (org-agenda-files '("~/Dropbox/org/inbox.org"))))))
        ("r"  "Report" ((tags "ENTRY_TYPE=\"PROJECT\"&TODO=\"DONE\"&CLOSED>\"<-1w>\""
			      ((org-super-agenda-groups '((:auto-parent t)))
                               (org-agenda-span "-7d")
			        (org-agenda-files '("~/Dropbox/org/inbox.org"
						    "~/Dropbox/org/inbox.org_archive"
						    "~/Dropbox/org/work.org"
						    "~/Dropbox/org/work.org_archive"
						    "~/Dropbox/org/main.org_archive"
						    "~/Dropbox/org/main.org"))
			        (org-agenda-overriding-header "Projects completed in the last week")))
		         (tags "TODO=\"DONE\"&CLOSED>\"<-1w>\"&ENTRY_TYPE=\"\""
			       ((org-agenda-overriding-header "Items Closed in the last week")
                                (org-agenda-span "-7d")
			        (org-agenda-files '("~/Dropbox/org/inbox.org"
						    "~/Dropbox/org/inbox.org_archive"
						    "~/Dropbox/org/work.org"
						    "~/Dropbox/org/work.org_archive"
						    "~/Dropbox/org/main.org_archive"
						    "~/Dropbox/org/main.org"))
			        (org-super-agenda-groups '((:auto-parent t)))))))
        ("he" "Execution Agenda" ((tags-todo "TODO=\"NEXT\"&SCHEDULED<\"<+1w>\"|TODO=\"NEXT\"-SCHEDULED={.+}-DEADLINE={.+}|TODO=\"NEXT\"&DEADLINE<\"<+1w>\""
					     ((org-agenda-overriding-header "Next Items")
					      (org-agenda-files '("~/Dropbox/org/main.org"))
					      (org-super-agenda-groups '((:auto-parent t)
									 ))))
                                  (stuck "" ((org-agenda-files '("~/Dropbox/org/main.org"))))))
	("hr" "Work Review" (
			     (org-ql-block '(and (parent (tags-local "PROJECT"))
						 (descendants (todo "NEXT"))
						 (not (or (tags-all "TEMPLATE")
							  (tags-all "DEPRIORITIZED_PROJECT")
							  (tags-all "DONE")
							  (todo "DONE"))))
					   ((org-agenda-files '("~/Dropbox/org/main.org"))
                                            (org-ql-block-header "Active Projects")))
                                           (stuck "" ((org-agenda-files '("~/Dropbox/org/main.org"))))
      			                   (todo "WAITING" ((org-agenda-overriding-header "Waiting tasks")
					                    (org-super-agenda-groups '((:auto-parent t)))))
			                   (tags-todo "TODO=\"TODO\"-DEPRIORITIZED_PROJECTS-DEPRIORITIZED_PROJECT-TEMPLATE&DEADLINE<\"<+2w>\""
					              ((org-agenda-overriding-header "Tasks in the next 2 weeks")
					               (org-agenda-files '("~/Dropbox/org/main.org"))))
			                   (tags-todo "TODO=\"TODO\"-TEMPLATE-PROJECT-SCHEDULED={.+}-DEADLINE={.+}"
					              ((org-agenda-overriding-header "Unplanned Todos")
					               (org-agenda-files '("~/Dropbox/org/main.org"))
					               (org-super-agenda-groups '((:auto-parent t)))))
			                   (tags "TODO=\"DONE\"&CLOSED>\"<-1d>\""
				                 ((org-agenda-overriding-header "Closed today")
				                  (org-super-agenda-groups '((:auto-parent t)))
				                  (org-agenda-files '("~/Dropbox/org/main.org"))))))
         ("we" "Execution Agenda" ((tags-todo "TODO=\"NEXT\"&SCHEDULED<\"<+1w>\"|TODO=\"NEXT\"-SCHEDULED={.+}-DEADLINE={.+}|TODO=\"NEXT\"&DEADLINE<\"<+1w>\""
					      ((org-agenda-overriding-header "Next Items")
					       (org-agenda-files '("~/Dropbox/org/work.org"))
					       (org-super-agenda-groups '((:auto-parent t)
									  ))))
                                   (stuck "" ((org-agenda-files '("~/Dropbox/org/work.org"))))))
	 ("wr" "Work Review" (
			      (org-ql-block '(and (parent (tags-local "PROJECT"))
						  (descendants (todo "NEXT"))
						  (not (or (tags-all "TEMPLATE")
							   (tags-all "DEPRIORITIZED_PROJECT")
							   (tags-all "DONE")
							   (todo "DONE"))))
					    ((org-ql-block-header "Active Projects")
                                             (org-agenda-files '("~/Dropbox/org/work.org"))))
                              (stuck "" ((org-agenda-files '("~/Dropbox/org/work.org"))))
      			      (todo "WAITING" ((org-agenda-overriding-header "Waiting tasks")
					       (org-super-agenda-groups '((:auto-parent t)))))
			      (tags-todo "TODO=\"TODO\"-DEPRIORITIZED_PROJECTS-DEPRIORITIZED_PROJECT-TEMPLATE&DEADLINE<\"<+2w>\""
					 ((org-agenda-overriding-header "Tasks in the next 2 weeks")
					  (org-agenda-files '("~/Dropbox/org/work.org"))))
			      (tags-todo "TODO=\"TODO\"-TEMPLATE-PROJECT-SCHEDULED={.+}-DEADLINE={.+}"
					 ((org-agenda-overriding-header "Unplanned Todos")
					  (org-agenda-files '("~/Dropbox/org/work.org"))
					  (org-super-agenda-groups '((:auto-parent t)))))
			      (tags "TODO=\"DONE\"&CLOSED>\"<-1d>\""
				    ((org-agenda-overriding-header "Closed today")
				     (org-super-agenda-groups '((:auto-parent t)))
				     (org-agenda-files '("~/Dropbox/org/work.org"))))))
         ("u" "Standup" ((tags "+STANDUP+ENTRYDATE>=\"<-3d>\"" ((org-agenda-overriding-header "Standup updates")
							        (org-agenda-overriding-columns-format )
							        (org-agenda-sorting-strategy '(time-down ts-down tsia-down))))))))

  (org-stuck-projects
   '("+ENTRY_TYPE=\"PROJECT\"-DONE-TEMPLATE-DEFERRED-CANCELLED-TODO=\"DONE\"" ("NEXT") ()
        "\\<IGNORE\\>"))


  (org-directory "~/Dropbox/org")
  ;; (org-mobile-directory "~/Dropbox/Apps/MobileOrg/")
  ;; (org-mobile-inbox-for-pull "~/Dropbox/inbox.org")
  (org-columns-default-format "%50ITEM %TODO %CLOCKSUM %Area")
  (org-agenda-include-diary t)
  (org-journal-template-entry (concat "* %T [" (system-name)  "]| %^{title} %^G"))

  :config
  (add-to-list 'org-modules 'org-id )
  (add-to-list 'org-modules 'org-habit)
  (require 'org-crypt)
  (require 'org-depend)
  (require 'org-protocol)
  (require 'org-checklist)
  (require 'gus-links)
  (add-to-list 'org-modules 'org-crypt)
  (add-to-list 'org-modules 'org-checklist)
  (setq org-crypt-disable-auto-save t)
  (org-crypt-use-before-save-magic)
  (setq org-crypt-key nil)

  (setq org-directory (expand-file-name "~/Dropbox/org"))
  (setq org-default-inbox-file (concat org-directory "/inbox.org"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-default-journal-file (concat org-directory "/notes.org"))
  (setq org-default-slipbox-file (concat org-directory "/slipbox.org"))
  (setq org-default-log-file   "~/Dropbox/log.org")

  (setq org-show-context-detail (assq-delete-all 'agenda org-show-context-detail))
  (add-to-list 'org-show-context-detail '(agenda . lineage))


  (setq org-capture-templates
  	'(("t" "Todo" entry (file org-default-inbox-file)
  	   "* TODO %^{entry}\n:PROPERTIES:\n:ENTRYDATE:   %U\n:END:\n %?\n")
	  ("n" "Note" entry (file org-default-inbox-file)
	   "* %^{title}\n:PROPERTIES:\n:ENTRYDATE:   %U\n:ID: %(uuid-create)\n:END:\n\n%?\n")
  	  ("r" "Lookup Entry in region" entry (file org-default-inbox-file)
  	   "* %i :LOOKUP:\n")
	  ("l" "Link" entry (file org-default-inbox-file)
	   "* %a\n:PROPERTIES:\n:ENTRYDATE:   %U\n:END:\n %i" :immediate-finish t)
  	  ("q" "Question" entry (file org-default-inbox-file)
  	   "* QUESTION %^{question} \n%?\n\nEntered on %U\n %i\n")
  	  ("j" "Journal" entry (file org-default-inbox-file)
  	   "* %^{title} %^G \n:PROPERTIES:\n:ENTRYDATE:   %U\n:END:\n\n%?\n\nEntered on %U\n %i\n")
	  ("i" "Time checkin" entry (file org-default-log-file)
	   "* %T [%(car (split-string (system-name)  \"[\.]\"))]| [ check in ] |%^{title}"
	   :immediate-finish t)
	  ("o" "Time checkout" entry (file org-default-log-file)
	   "* %T [%(car (split-string (system-name)  \"[\.]\"))]| [ check out ] |%^{title}"
	   :immediate-finish t)
  	  ("g" "log" entry (function custom-log-finder)
  	   "* %T [%(car (split-string (system-name)  \"[\.]\"))]| %^{title}  %(add-tag) " :immediate-finish t)))

  (add-hook 'org-mode-hook
            (lambda()
              (visual-line-mode t)
              (setq line-spacing 10)
              (setq left-margin-width 10 right-margin-width 10)))

  (defun skip-done-functions-or-projects()
    (org-agenda-skip-entry-if 'todo '("DONE" "WAITING" "NEXT")))

  (defun org-checkbox-todo ()
    "Switch header TODO state to either DONE, NEXT, or TODO depending on the number of check boxes ticked"
    (let ((todo-state (org-get-todo-state)) beg end)
      (unless (not todo-state)
  	(save-excursion
  	  (org-back-to-heading t)
  	  (let* ((line-start (point))
  		 (line-end (line-end-position)))
  	    (if (re-search-forward "\\[\\([0-9]*\\)%\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
  				   end t)
  		(if (match-end 1)
  		    (let ((percent-done (string-to-number (match-string 1))))
  		      (handle-percent-case percent-done))
  		  (let ((tasks-done (string-to-number (match-string 2)))
  			(tasks-remaining (string-to-number (match-string 3))))
  		    (handle-task-number-case tasks-done tasks-remaining)))))))))

  (defun handle-percent-case(percent-done)
    (if (= percent-done 100)
  	(org-todo "DONE")
      (if (> percent-done 0)
  	  (org-todo "NEXT")
  	(org-todo "TODO"))))

  (defun handle-task-number-case(tasks-done tasks-remaining)
    (if (= tasks-done tasks-remaining)
  	(org-todo "DONE")
      (if (= tasks-done 0)
  	  (org-todo "TODO")
  	(org-todo "NEXT"))))

  (defun find-list-of-points-with-log-entry-headings()
    (org-map-entries (lambda ()
  		       (let ((current-headline (buffer-substring-no-properties
  						(line-beginning-position)
  						(line-end-position))))
  			 (if (string-match-p " Log Entries" current-headline)
  			     (line-beginning-position)
  			   nil)))
  		     nil
  		     'tree))

  (defun find-log-header()
    (interactive)
    (let ((filtered-list  (seq-filter (lambda(x)
  					(not (null x)))
  				      (find-list-of-points-with-log-entry-headings))))
      (if (null filtered-list)
  	  nil
  	(car filtered-list))))

  (defun insert-log-entry-heading()
    (interactive)
    (let ((depth (org-current-level)))
      (outline-next-heading)
      (let ((heading-string (concat
  				    (make-string (+ depth 1) ?*)
  				    " Log Entries\n")))
  	(insert heading-string)
  	(goto-char (- (line-beginning-position) 1)))))


  (defun find-or-insert-entry()
    (interactive)
    (let ((log-header-point (find-log-header)))
      (if (null log-header-point)
  	  (insert-log-entry-heading)
  	(goto-char log-header-point))))


  (defun custom-log-finder()
    (if (and (fboundp 'org-clocking-p) (org-clocking-p))
  	(let ()
  	  (org-clock-goto)
  	  (find-or-insert-entry))
      (let ()
  	(find-file org-default-log-file)
  	(goto-char (point-max)))))

  (defun goto-last-heading ()
    (interactive)
    (org-end-of-subtree))

  (defun add-tag()
    "This function adds a tag to the log entry if the entry is not going to be appended to an entry that is clocked in."
    (if (and (fboundp 'org-clocking-p)
	     (org-clocking-p))
  	""
      "%^G"))


  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let ((todo-state (org-get-todo-state)) beg end)
      (unless (not todo-state)
        (let (org-log-done org-log-states)   ; turn off logging
          (if (= n-not-done 0)
              (progn
                (org-todo "DONE")
                (save-excursion
                  (end-of-line)
                  (insert "\n   CLOSED:")
                  (insert (reuben/get-inactive-org-date-time))))
              (org-todo (if (> n-done 0) "NEXT" "TODO")))))))
  
  (defun jump-to-org-agenda ()
    (interactive)
    (push-window-configuration)
    (let ((recordings-dir "~/Dropbox/Apps/Dropvox"))
      (ignore-errors
  	(if (directory-files recordings-dir nil "\\`[^.]")
  	    (find-file recordings-dir))))
    (let ((buf (get-buffer "*Org Agenda*"))
  	  wind)
      (if buf
  	  (if (setq wind (get-buffer-window buf))
  	      (when (called-interactively-p 'any)
  		(select-window wind)
  		(org-fit-window-to-buffer))
  	    (if (called-interactively-p 'any)
  		(progn
  		  (select-window (display-buffer buf t t))
  		  (org-fit-window-to-buffer))
  	      (with-selected-window (display-buffer buf)
  		(org-fit-window-to-buffer))))
  	(org-agenda "a" "d"))))
  (add-hook 'org-checkbox-statistics-hook 'org-checkbox-todo)

  (require 'seq)
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  (defun org-count-todos-in-state (state)
    (let ((count 0))
      (org-scan-tags (lambda ()
		       (when (string= (org-get-todo-state) state)
			 (setq count (1+ count))))
		     t t)
      count))


  (defvar org-wip-limit 20 "Work-in-progress limit")
  (defvar org-wip-state "NEXT")

  (defun org-block-wip-limit (change-plist)
    (catch 'dont-block
      (when (or (not (eq (plist-get change-plist :type) 'todo-state-change))
		(not (string= (plist-get change-plist :to) org-wip-state)))
	(throw 'dont-block t))

      (when (>= (org-count-todos-in-state org-wip-state) org-wip-limit )
	(setq org-block-entry-blocking (format "Number of items in NEXT limit(org-wip-limit): %s" org-wip-state))
	(throw 'dont-block nil))

      t)) ; do not block

  (add-hook 'org-blocker-hook #'org-block-wip-limit)

  ;; (global-unset-key [(f9)])
  ;; (global-set-key [(f9)] 'org-mark-ring-goto)
  (define-key org-mode-map [(f9)] 'org-mark-ring-goto)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (scheme . t)
     (python . t)
     (lisp . t)
     (R . t)))

  (setq org-babel-python-command "python3")
  )

(use-package org-ref
  :defer t
  :ensure t
  :after org
  :custom
  (reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))
  (org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org")
  (org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib"))
  (org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")
  :config
  (require 'org-ref))

(use-package org-bullets
  :ensure t
  :config ;; executed after loading package
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-ql
  :ensure t
  :defer t
  :after org)

(use-package org-super-agenda
  :after org
  :ensure t
  :config
  (org-super-agenda-mode))

;;;;;;;;;;;;;;; Auto update emacs package ;;;;;;;;;;;;;;;
(use-package auto-package-update
  :ensure t
  :defer 5
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package ibuffer
  :defer t
  :custom
  (ibuffer-saved-filter-groups
   (quote (("default"
	    ("org" (mode . org-mode))
	    ("dired" (mode . dired-mode))
	    ("emacs" (or
		      (name . "^\\*scratch\\*$")
		      (name . "^\\*Messages\\*$")))
	    ("gnus" (or
		     (mode . message-mode)
		     (mode . bbdb-mode)
		     (mode . mail-mode)
		     (mode . gnus-group-mode)
		     (mode . gnus-summary-mode)
		     (mode . gnus-article-mode)
		     (name . "^\\.bbdb$")
		     (name . "^\\.newsrc-dribble")))))))
  :config
  (add-hook 'ibuffer-mode-hook
	    (lambda ()
	      (ibuffer-switch-to-saved-filter-groups "default"))))

;; --------------- Spacemacs theming ---------------

(use-package spaceline
  :defer t
  :ensure t)

(use-package spaceline-all-the-icons
  :ensure t
  :defer t
  :after spaceline
  :config
  (setq spaceline-all-the-icons-separator-type 'arrow))

;; --------------- fly check mode ---------------

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint json-jsonlist)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; --------------- company mode ---------------
(use-package company
  :ensure t
  :defer t
  :custom (company-idle-delay 0.5)
  :init (global-company-mode)
  :bind
  (:map company-active-map
	("C-n". company-select-next)
	("C-p". company-select-previous)
	("M-<". company-select-first)
	("M->". company-select-last)))

;; --------------- Hydra mode ---------------
(use-package hydra
  :ensure t
  :config
  (defhydra jethro/hydra-draw-box (:color pink)
    "Draw box with IBM single line box characters (ESC to Quit)."
    ("ESC" nil :color blue) ;; Esc to exit.
    ("'" (lambda () (interactive) (insert "┌")) "top left ┌")
    ("," (lambda () (interactive) (insert "┬")) "top ┬")
    ("." (lambda () (interactive) (insert "┐")) "top right ┐")
    ("a" (lambda () (interactive) (insert "├")) "left ├")
    ("o" (lambda () (interactive) (insert "┼")) "center ┼")
    ("e" (lambda () (interactive) (insert "┤")) "right ┤")
    (";" (lambda () (interactive) (insert "└")) "bottom left └")
    ("q" (lambda () (interactive) (insert "┴")) "bottom ┴")
    ("j" (lambda () (interactive) (insert "┘")) "bottom right ┘")
    ("k" (lambda () (interactive) (insert "─")) "horizontal ─")
    ("x" (lambda () (interactive) (insert "│")) "vertical │"))

  (bind-key "C-c h d" 'jethro/hydra-draw-box/body)

  (defhydra jethro/hydra-smerge (:color pink
                                        :hint nil
                                        :pre (smerge-mode 1)
                                        ;; Disable `smerge-mode' when quitting hydra if
                                        ;; no merge conflicts remain.
                                        :post (smerge-auto-leave))
    "
   ^Move^       ^Keep^               ^Diff^                 ^Other^
   ^^-----------^^-------------------^^---------------------^^-------
   _n_ext       _b_ase               _<_: upper/base        _C_ombine
   _p_rev       _u_pper              _=_: upper/lower       _r_esolve
   ^^           _l_ower              _>_: base/lower        _k_ill current
   ^^           _a_ll                _R_efine
   ^^           _RET_: current       _E_diff
   "
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-mine)
    ("l" smerge-keep-other)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-mine)
    ("=" smerge-diff-mine-other)
    (">" smerge-diff-base-other)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("q" nil "cancel" :color blue))

  (bind-key "C-c h s" 'jethro/hydra-smerge/body)

  (defhydra jethro/hydra-zoom ()
    "zoom"
    ("i" text-scale-increase "in")
    ("o" text-scale-decrease "out"))

  (bind-key "C-c h z" 'jethro/hydra-zoom/body)
  )


(use-package bury-successful-compilation
  :ensure t
  :hook
  (prog-mode . bury-successful-compilation))

;; --------------- Rust Config ---------------
;; (use-package rust-mode
;;   :ensure t
;;   :defer t
;;   :mode "\\.rs"
;;   :config
;;   (use-package racer
;;   :ensure t)
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   (add-hook 'racer-mode-hook #'eldoc-mode))

;; (use-package company-racer
;;   :ensure t
;;   :defer t
;;   :after (company)
;;   :config
;;   (add-to-list 'company-backends 'company-racer))

;; (use-package flycheck-rust
;;   :ensure t
;;   :defer t
;;   :after (rust-mode)
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))


(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))


(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))


(use-package tide
  :ensure t
  :defer t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package restclient
  :defer t
  :ensure t)

(use-package ledger-mode
  :defer t
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :config
  (exec-path-from-shell-initialize))

(use-package slime
  :defer t
  :ensure t
  :config
  (progn
    (setq inferior-lisp-program "sbcl"
	  slime-contribs '(slime-fancy))))

;; --------------- Writing ---------------
(use-package olivetti
  :ensure t
  :after wc-goal-mode
  :config
  (defun writing-mode()
    (interactive)
    (olivetti-mode 1)
    (set-background-color "#FCFCFC")
    (set-foreground-color "#1A1A1A")
    (set-cursor-color "#07BBF2")
    (set-face-attribute 'default nil :family "Fira Mono" :width 'normal)
    (set-face-attribute 'variable-pitch nil :family "Source Sans Pro")
    (set-face-attribute 'fixed-pitch nil :family "Fira Mono")
    (setq-default line-spacing 6)
    (set-face-attribute 'mode-line (selected-frame) :background "#FFFFFF" :overline "#FCFCFC" :foreground "grey")
    (set-fringe-mode 0)
    (require 'wc-goal-mode)))

(use-package visual-fill-column
  :ensure t)

(use-package wc-goal-mode
  :ensure t)

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package doom-themes
  :ensure t)
(use-package doom-modeline
  :ensure t)


(use-package elfeed
  :ensure t)
(use-package elfeed-org
  :ensure t
  :config
  (require 'elfeed-org)
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Dropbox/feeds.org")))
(use-package elfeed-goodies
  :ensure t
  :config
  (elfeed-goodies/setup))
(use-package elfeed-protocol
  :ensure t  )
(use-package elfeed-score
  :ensure t)


(use-package deft
  :ensure t
  :bind ("<f8>" . deft)
  :commands (deft)
  
  :config (setq deft-directory "~/Dropbox/org-roam"
                deft-extensions '("md" "org")
		deft-use-filename-as-title t
		deft-file-naming-rules   '((noslash . "-")
					   (nospace . "-")
					   (case-fn . downcase))))

(use-package hyperbole
  :ensure t)

(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/Dropbox/org-roam/")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

;; --------------- Web Mode ---------------
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  
  (defun web-mode-init-hook ()
    "Hooks for Web mode.  Adjust indent."
    (setq web-mode-markup-indent-offset 4))
  
  (add-hook 'web-mode-hook  'web-mode-init-hook))


(use-package add-node-modules-path
  :ensure t
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook 'add-node-modules-path))

(use-package prettier-js
  :ensure t
  :config
  (defun web-mode-init-prettier-hook ()
    (add-node-modules-path)
    (prettier-js-mode))
  
  (add-hook 'web-mode-hook  'web-mode-init-prettier-hook))

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook  'emmet-mode))

(use-package typescript-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode)))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; --------------- Projectile ---------------

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; --------------- Avy ---------------
(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char))

;; --------------- Neotree ---------------
(use-package neotree
  :ensure t)

(use-package geiser
  :ensure t)

(use-package geiser-mit
  :ensure t
  :after geiser
  :config
  (setq geiser-active-implementations '(mit)))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode t))


(provide 'use-package-config)
;;; use-package-config.el

