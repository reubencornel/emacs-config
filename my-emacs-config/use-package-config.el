(require 'use-package)

(use-package swiper-helm
  :ensure t
  :after (helm)
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
    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-unset-key (kbd "C-x c"))   
    (helm-mode 1)))

(use-package helm-org-rifle
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
    (setq eshell-buffer-maximum-lines 10000)
    (setq my/eshell-truncate-timer
	  (run-with-idle-timer 5 t #'my/truncate-eshell-buffers))))

(use-package eshell-git-prompt
  :ensure t
  :init
  (eshell-git-prompt-use-theme 'robbyrussell))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SCALA CONFIG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ensime
  :ensure t
  :pin melpa)

(use-package sbt-mode
  :pin melpa)

(use-package scala-mode
  :pin melpa
  :interpreter
  ("scala" . scala-mode)
  ("sc" . scala-mode))


(use-package org
  :ensure t
  
  :custom 
  (org-hide-leading-stars 't)
  (org-log-done 'time)
  (org-export-with-section-numbers nil)
  (org-export-with-toc nil)
  (org-pretty-entities t)
  (org-reverse-note-order t)
  (org-log-into-drawer "LOGBOOK")
  (org-clock-persist t)
  (org-clock-idle-time 60)
  (org-clock-history-length 35)
  (org-clock-in-resume t)
  (org-clock-out-remove-zero-time-clocks t)
  (org-id-method 'uuidgen)
  (org-enforce-todo-dependencies t)
  (org-hide-leading-stars t)
  (org-refile-targets '((org-agenda-files :maxlevel . 5)))

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
     ("d" "Daily Agenda" ((agenda "Daily Agenda" ((org-agenda-span 1)
						  (org-agenda-skip-function 'skip-done-functions-or-projects)
						  (org-agenda-overriding-header "Daily Agenda")))
			  (tags-todo "TODO=\"NEXT\"&SCHEDULED<\"<+1w>\"|TODO=\"NEXT\"-SCHEDULED={.+}-DEADLINE={.+}|TODO=\"NEXT\"&DEADLINE<\"<+1w>\""
				     ((org-agenda-overriding-header "Next Items")))
			  (tags-todo "TODO=\"TOREVIEW\"-TEMPLATE|TODO=\"INREVIEW\"-TEMPLATE|TODO=\"REWORK\"-TEMPLATE" ((org-agenda-overriding-header "Pending Code Reviews")))
			  (todo "QUESTION" ((org-agenda-overriding-header "Open Questions")))
			  (todo "WAITING" ((org-agenda-overriding-header "Waiting tasks")))
			  (tags-todo  unscheduled-tasks-search-string ((org-agenda-overriding-header "Unscheduled Tasks")))
			  (tags "TODO=\"DONE\"&CLOSED>\"<-1d>\"" ((org-agenda-overriding-header "Closed today")))
			  (stuck "" ((org-use-tag-inheritance nil)
				     (org-agenda-overriding-header "Stuck Projects")))))
     ("w" "Weekly Review" ((stuck "" ((org-use-tag-inheritance nil)
				      (org-agenda-overriding-header "Stuck Projects")))
			   (tags-todo unscheduled-tasks-search-string  ((org-agenda-overriding-header "Unscheduled Tasks")))
			   (tags-todo "+TODO=\"WAITING\"+TIMESTAMP_IA<\"<-1w>\"" ((org-agenda-overriding-header "Tasks waiting for more than a week")))
			   (tags-todo "+TODO=\"NEXT\"+TIMESTAMP_IA<\"<-1w>\""  ((org-agenda-overriding-header "Tasks in progress for more than a week")))
			   (tags "TODO=\"DONE\"&CLOSED>\"<-1w>\"" ((org-agenda-overriding-header "Closed in the last week")))))
     ("u" "Standup" ((tags "+STANDUP+ENTRYDATE>=\"<-3d>\"" ((org-agenda-overriding-header "Standup updates")
							    (org-agenda-overriding-columns-format )
							    (org-agenda-sorting-strategy '(time-down ts-down tsia-down))))))))

  (org-stuck-projects
   '("+PROJECT-DONE-TEMPLATE-DEFERRED-CANCELLED-TODO=\"DONE\"" ("NEXT" "WAITING") ()
     "\\<IGNORE\\>"))

  (org-directory (expand-file-name "~/Dropbox"))
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-default-journal-file (concat org-directory "/notes.org"))
  (org-default-log-file   "~/Dropbox/log.org")

  (org-directory "~/Dropbox")
  (org-mobile-directory "~/Dropbox/Apps/MobileOrg/")
  (org-mobile-inbox-for-pull "~/Dropbox/inbox.org")


  (org-agenda-include-diary t)

  (org-journal-template-entry (concat "* %T [" (system-name)  "]| %^{title} %^G"))

  (org-capture-templates
  	'(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
  	   "* TODO %^{entry}\n:PROPERTIES:\n:ENTRYDATE:%U\n:END:\n %?\n")
  	  ("r" "Lookup Entry in region" entry (file+headline org-default-notes-file "Lookup")
  	   "* %i :LOOKUP:\n")
  	  ("l" "Lookup Entry" entry (file+headline org-default-notes-file "Lookup")
  	   "* %?  :LOOKUP:\n %i \n")
  	  ("q" "Question" entry (file+datetree org-default-notes-file)
  	   "* QUESTION %^{question} \n%?\n\nEntered on %U\n %i\n")
  	  ("j" "Journal" entry (file+datetree org-default-journal-file)
  	   "* %^{title} %^G \n\n%?\n\nEntered on %U\n %i\n")
  	  ("g" "log" entry (function custom-log-finder)
  	   "* %T [%(car (split-string (system-name)  \"[\.]\"))]| %^{title}  %(add-tag) " :immediate-finish t)
  	  ("s" "Standup" entry (file+datetree org-default-notes-file)
  	   "*   %^{title} :STANDUP:\n:PROPERTIES:\n:COLUMNS: %50ITEM %ENTRYDATE\n:ENTRYDATE: %u\n:END:\n%?\n\nEntered on %U\n %i\n")))

  :config
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
p  			   nil)))
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
    (if (fboundp 'org-clocking-p) (org-clocking-p))
  	""
      "%^G")


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
  )

(use-package org-bullets
  :ensure t
  :requires org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  ;; (check-and-install-if-absent 'org-bullets)
  ;; (require 'org-habit)
  ;; ;; org-habit 
  ;; (add-to-list 'org-modules 'org-habit)
  ;; (setq org-habit-preceding-days 15
  ;; 	org-habit-following-days 1
  ;; 	org-habit-graph-column 65
  ;; 	org-habit-show-habits-only-for-today t
  ;; 	org-habit-show-all-today t)
  
  ;; (add-hook 'after-save-hook 'sync-index-org)
  ;; ;; search 5 levels deep in org files.
  
