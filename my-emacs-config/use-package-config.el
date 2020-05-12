(require 'use-package)

(use-package swiper-helm
  :ensure t
  :after (helm)
  :defer t
  :config
  (global-set-key (kbd "C-s") 'swiper-helm))

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
;    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-unset-key (kbd "C-x c"))
    (helm-mode 1)))

(use-package helm-org-rifle
  :ensure t
  :defer t
  :after (helm))

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
  :bind    (("C-c n" .  'org-narrow-to-subtree)
	    ("C-c w" .  'widen))
  :custom
  (org-hide-leading-stars 't)
					;(org-log-done 'time)
  (org-agenda-text-search-extra-files ;; This variable instructs org agenda to search through the archives
   '(agenda-archives "~/Dropbox/work.org_archive" "~/Dropbox/main.org_archive"))
  (org-refile-use-outline-path 3)
  (org-agenda-files
   '("~/Dropbox/log.org" "~/Dropbox/notes.org" "~/Dropbox/inbox.org" "~/Dropbox/work.org" "~/Dropbox/main.org" "~/Dropbox/slipbox.org" "~/Dropbox/slipbox_raw.org"))
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
   '(("q" tags-todo "TODO=\"QUESTION\"")
     ;; ("d" "Daily Agenda" ((agenda "Daily Agenda" ((org-agenda-span 1)
     ;; 						  (org-agenda-skip-function 'skip-done-functions-or-projects)
     ;; 						  (org-agenda-overriding-header "Daily Agenda")))
     ;; 			  (tags-todo "TODO=\"NEXT\"&SCHEDULED<\"<+1w>\"|TODO=\"NEXT\"-SCHEDULED={.+}-DEADLINE={.+}|TODO=\"NEXT\"&DEADLINE<\"<+1w>\""
     ;; 				     ((org-agenda-overriding-header "Next Items")))
     ;; 			  (tags-todo "TODO=\"TOREVIEW\"-TEMPLATE|TODO=\"INREVIEW\"-TEMPLATE|TODO=\"REWORK\"-TEMPLATE" ((org-agenda-overriding-header "Pending Code Reviews")))
     ;; 			  (todo "QUESTION" ((org-agenda-overriding-header "Open Questions")))
     ;; 			  (todo "WAITING" ((org-agenda-overriding-header "Waiting tasks")))
     ;; 			  (tags-todo  unscheduled-tasks-search-string ((org-agenda-overriding-header "Unscheduled Tasks")))
     ;; 			  (tags "TODO=\"DONE\"&CLOSED>\"<-1d>\"" ((org-agenda-overriding-header "Closed today")))
     ;; 			  (stuck "" ((org-use-tag-inheritance nil)
     ;; 				     (org-agenda-overriding-header "Stuck Projects")))))
     ("we" "Execution Agenda" ((tags-todo "TODO=\"NEXT\"&SCHEDULED<\"<+1w>\"|TODO=\"NEXT\"-SCHEDULED={.+}-DEADLINE={.+}|TODO=\"NEXT\"&DEADLINE<\"<+1w>\""
					    ((org-agenda-overriding-header "Next Items")
					     (org-agenda-files '("~/Dropbox/work.org"))
					     (org-super-agenda-groups '((:auto-parent t)
									))))
				 (org-ql-block '(and (parent (tags-local "PROJECT"))
						     (descendants (todo "NEXT"))
						     (not (or (tags-all "TEMPLATE")
							      (tags-all "DEPRIORITIZED_PROJECT")
							      (tags-all "DONE")
							      (todo "DONE"))))
					       ((org-ql-block-header "Active Projects")
						(org-agenda-files '("~/Dropbox/work.org"))))
				 (org-ql-block '(and (parent (tags-local "PROJECT"))
						     (not (descendants (todo "NEXT")))
						     (not (or (tags-all "TEMPLATE")
							      (tags-all "DEPRIORITIZED_PROJECT")
							      (tags-all "DONE")
							      (todo "DONE"))))
					       ((org-ql-block-header "Stuck Projects")
						(org-agenda-files '("~/Dropbox/work.org"))))))
	("wr" "Work Review" (
			     (org-ql-block '(and (parent (tags-local "PROJECT"))
						 (descendants (todo "NEXT"))
						 (not (or (tags-all "TEMPLATE")
							  (tags-all "DEPRIORITIZED_PROJECT")
							  (tags-all "DONE")
							  (todo "DONE"))))
					   ((org-ql-block-header "Active Projects")))
			     (org-ql-block '(and (parent (tags-local "PROJECT"))
						 (not (descendants (todo "NEXT")))
						 (not (or (tags-all "TEMPLATE")
							  (tags-all "DEPRIORITIZED_PROJECT")
							  (tags-all "DONE")
							  (todo "DONE"))))
					   ((org-ql-block-header "Stuck Projects")))
      			     (todo "WAITING" ((org-agenda-overriding-header "Waiting tasks")
					      (org-super-agenda-groups '((:auto-parent t)))))
			     (tags-todo "TODO=\"TODO\"-DEPRIORITIZED_PROJECTS-DEPRIORITIZED_PROJECT-TEMPLATE&DEADLINE<\"<+2w>\""
					((org-agenda-overriding-header "Tasks in the next 2 weeks")
					 (org-agenda-files '("~/Dropbox/work.org"))))
			     (tags-todo "TODO=\"TODO\"-TEMPLATE-PROJECT-SCHEDULED={.+}-DEADLINE={.+}"
					((org-agenda-overriding-header "Unplanned Todos")
					 (org-agenda-files '("~/Dropbox/work.org"))
					 (org-super-agenda-groups '((:auto-parent t)))))
			     (tags "TODO=\"DONE\"&CLOSED>\"<-1d>\""
				   ((org-agenda-overriding-header "Closed today")
				    (org-super-agenda-groups '((:auto-parent t)))
				    (org-agenda-files '("~/Dropbox/work.org"))))))
     ("u" "Standup" ((tags "+STANDUP+ENTRYDATE>=\"<-3d>\"" ((org-agenda-overriding-header "Standup updates")
							    (org-agenda-overriding-columns-format )
							    (org-agenda-sorting-strategy '(time-down ts-down tsia-down))))))))

  (org-stuck-projects
   '("+PROJECT-DONE-TEMPLATE-DEFERRED-CANCELLED-TODO=\"DONE\"" ("NEXT" "WAITING") ()
     "\\<IGNORE\\>"))

  (org-directory (expand-file-name "~/Dropbox"))
  (org-default-inbox-file (concat org-directory "/inbox.org"))
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-default-journal-file (concat org-directory "/notes.org"))
  (org-default-slipbox-file (concat org-directory "/slipbox.org"))
  (org-default-log-file   "~/Dropbox/log.org")

  (org-directory "~/Dropbox")
  (org-mobile-directory "~/Dropbox/Apps/MobileOrg/")
  (org-mobile-inbox-for-pull "~/Dropbox/inbox.org")

  (org-agenda-include-diary t)
  (org-journal-template-entry (concat "* %T [" (system-name)  "]| %^{title} %^G"))

  (org-capture-templates
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

  :config
  (add-to-list 'org-modules 'org-id)
  (require 'org-crypt)
  (require 'org-depend)
  (require 'org-protocol)
  (require 'org-checklist)
  (add-to-list 'org-modules 'org-crypt)
  (add-to-list 'org-modules 'org-checklist)
  (setq org-crypt-disable-auto-save t)
  (org-crypt-use-before-save-magic)
  (setq org-crypt-key nil)

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
  	  (org-todo (if (= n-not-done 0) "DONE"
  		      (if (> n-done 0) "NEXT" "TODO")))))))

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
  
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
  ;;  '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
    '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
    '(org-link ((t (:foreground "royal blue" :underline t))))
    '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
    '(org-property-value ((t (:inherit fixed-pitch))) t)
    '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
    '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
    '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
    '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
  
  (let* ((variable-tuple
	  (cond
	   ((x-list-fonts "Fira Mono") '(:font "Fira Mono"))
	   ((x-list-fonts "Playfair Display") '(:font "Playfair Display"))
	   ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
	   ((x-list-fonts "Verdana")         '(:font "Verdana"))
	   ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
	   (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
	 (base-font-color     (face-foreground 'default nil 'default))
	 (headline           `(:inherit default )))
    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2 :foreground "DarkTurquoise"))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3 :foreground "LimeGreen"))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5 :foreground "Violetred1"))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))


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
  :init (global-flycheck-mode))

;; --------------- company mode ---------------
(use-package company
  :ensure t
  :defer t
  :init (global-company-mode))

(provide 'use-package-config)
;;; use-package-config.el



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
(use-package rust-mode
  :ensure t
  :defer t
  :mode "\\.rs"
  :config
  (use-package racer
  :ensure t)



  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package company-racer
  :ensure t
  :defer t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-racer))

(use-package flycheck-rust
  :ensure t
  :defer t
  :after (rust-mode)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

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
