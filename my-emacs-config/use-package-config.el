;; -*- lexical-binding: t -*-

(straight-use-package 'use-package)

(use-package completion-preview
  :custom
  (completion-auto-select t) ;; Show completion on first call
  (completion-auto-help 'visible) ;; Display *Completions* upon first request
  (completions-format 'one-column) ;; Use only one column
  (completions-sort 'historical) ;; Order based on minibuffer history
  (completions-max-height 20) ;; Limit completions to 20
  (completion-ignore-case t)
  :hook (after-init . global-completion-preview-mode))

(use-package orderless
  :straight t
  :custom
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)))

(use-package vertico
  :straight t
  :defer t
  :hook (after-init . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-count 15))

(defun swiper-isearch-backward-thing-at-point ()
  "Start swiper-isearch-backward with thing at point."
  (interactive)
  (let ((thing (thing-at-point 'word)))
    (if thing
        (swiper-isearch-backward thing)
      (swiper-isearch-backward))))

(use-package swiper
  :straight t
  :bind (("C-c s s" . swiper-isearch-thing-at-point)
         ("C-c s r" . swiper-isearch-backward-thing-at-point)))

(use-package ace-window
  :straight t
  :defer t)

(defun reuben/consult-search-org-helper (org-param keyword directory)
  (let ((old-value consult-ripgrep-args))
    (unwind-protect
	(progn
	  (customize-set-variable
	   'consult-ripgrep-args
	   (concat "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\  --smart-case --no-heading --line-number --search-zip"
		   " "
		   org-param
		   " "
		   "."))
	  (let ((vertico-count 10))
	    (consult-ripgrep directory keyword)))
      (progn
	(customize-set-variable
	 'consult-ripgrep-args
	 old-value)))))


(defun reuben/consult-search-org ()
  "Call `consult-ripgrep' for my org agenda files."
  (interactive)
  (reuben/consult-search-org-helper "-g \"*.org\"" "" "~/Dropbox/org"))

(defun reuben/consult-search-all-org ()
  "Call `consult-ripgrep' for my org agenda files."
  (interactive)
  (reuben/consult-search-org-helper "-t org" "" "~/Dropbox/org"))

(defun reuben/consult-search-org-roam()
  "Call `consult-ripgrep' for my org roam files."
  (interactive)
  (reuben/consult-search-org-helper "-g \"*.org\"" "" "~/Dropbox/org-roam/org-roam1"))

(defun reuben/consult-search-howm()
  "Call `consult-ripgrep' for my howm files."
  (interactive)
  (reuben/consult-search-org-helper "-g \"*.org\"" "" "~/Dropbox/howm"))


(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (
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
         ("<help> a" . consult-apropos)            ;; or
	 ;;	 ig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
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
         ("M-s m" . consult-multi-occur))
  :custom (consult-narrow-key "<")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))


(use-package howm
  :straight t
  :defer t
  :bind (("C-c , ," . howm-menu)
         ("C-c , c" . howm-create)
         ("C-c , a" . howm-list-all)
         ("C-c , r" . howm-list-recent)
         ("C-c , s" . howm-list-grep)
         ("C-c , k" . howm-keyword-search)
         ("C-c , ." . howm-list-today))
  :custom
  (howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.org")
  (howm-view-use-grep t)
  (howm-view-grep-command "rg")
  (howm-view-grep-option "-nH -i --no-heading --color never --line-buffered")
  (howm-view-grep-extended-option nil)
  (howm-view-grep-fixed-option "-F")
  (howm-view-grep-expr-option nil)
  (howm-view-grep-file-stdin-option nil)
  :hook ((howm-mode . howm-mode-set-buffer-name)
         (after-save . howm-mode-set-buffer-name))
  :config
  (setq howm-home-directory "~/Dropbox/howm/")
  (setq howm-directory "~/Dropbox/howm/")
  (define-key howm-menu-mode-map "\C-h" nil)
  (define-key riffle-summary-mode-map "\C-h" nil)
  (define-key howm-view-contents-mode-map "\C-h" nil)
  (advice-add 'howm-list-recent :after #'howm-view-sort-by-mtime)
  (advice-add 'howm-list-all :after #'(lambda () (howm-view-sort-by-date t)))

  (setq howm-keyword-file (expand-file-name ".howm-keys" howm-home-directory))
  (setq howm-history-file (expand-file-name ".howm-history" howm-home-directory))
)

(use-package plantuml-mode
  :straight t
  :defer t
  :mode ("\\.uml$" . plantuml-mode)
  :config
  (progn
    (setq plantuml-jar-path "~/bin/plantuml.jar")))

(use-package magit
  :straight t
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch)))

(use-package markdown-mode
  :straight t
  :mode ("\\.md" . markdown-mode))

(defun my/truncate-eshell-buffers ()
  "Truncates all eshell buffers"
  (interactive)
  (save-current-buffer
    (dolist (buffer (buffer-list t))
      (set-buffer buffer)
      (when (eq major-mode 'eshell-mode)
	(eshell-truncate-buffer)))))

(use-package eshell
  :defer t
  :custom
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-buffer-maximum-lines 2000)
  :config
  (setq my/eshell-truncate-timer
	(run-with-idle-timer 5 t #'my/truncate-eshell-buffers)))

;; Setup function to run once
(defun my/setup-eshell-git-prompt ()
  "Set up eshell git prompt theme."
  (eshell-git-prompt-use-theme 'robbyrussell)
  ;; Remove hook after first run
  (remove-hook 'eshell-mode-hook #'my/setup-eshell-git-prompt))

(use-package eshell-git-prompt
  :straight t
  :hook (eshell-mode . my/setup-eshell-git-prompt))

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


(defun rasmus/remove-schedule ()
  "Remove SCHEDULED-cookie is switching state to WAITING."
  (save-excursion
    (and (equal (org-get-todo-state) "DONE")
	 (org-get-scheduled-time (point))
	 (when (search-forward-regexp org-scheduled-time-regexp nil t)
	   (or (delete-region (match-beginning 0) (match-end 0)) t))
	 (get-buffer "*Org Agenda*")
	 (with-current-buffer "*Org Agenda*"
	   (org-agenda-redo)))))

(add-hook 'org-after-todo-state-change-hook
	  'rasmus/remove-schedule)

(defun org-count-todos-in-state (state)
  (let ((count 0))
    (org-scan-tags (lambda ()
		     (when (string= (org-get-todo-state) state)
		       (setq count (1+ count))))
		   t t)
    count))

(defun org-block-wip-limit (change-plist)
  (catch 'dont-block
    (when (or (not (eq (plist-get change-plist :type) 'todo-state-change))
	      (not (string= (plist-get change-plist :to) org-wip-state)))
      (throw 'dont-block t))

    (when (>= (org-count-todos-in-state org-wip-state) org-wip-limit )
      (setq org-block-entry-blocking (format "Number of items in NEXT limit(org-wip-limit): %s" org-wip-state))
      (throw 'dont-block nil))

    t)) ; do not block

(defun reuben/org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0)))

(defun get-plain-date-string()
  (format-time-string "%Y-%m-%d %a %H:%M:%S %z"))

(defun get-inactive-date-string()
  (concat "[" (get-plain-date-string) "]"))

(defun get-active-date-string()
  (concat "<" (get-plain-date-string) ">"))

(use-package org
  :defer t
  :straight org-contrib
  :bind  (:map org-mode-map
               ;; ([f3] . org-narrow-to-subtree)
               ;; ([f4] . widen)
               ([M-return] . org-meta-return)
	       ("C-c ," . howm-menu)
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
   (quote ((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "DEFERRED(e)" "|" "DONE(d!)" "CANCELLED(c@)" "DELEGATED")
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

  (org-agenda-custom-commands
   '(("d" "Daily Tasks" ((agenda ""
				 ((org-agenda-overriding-header "Tasks to work on today")
                                  (org-agenda-entry-types '(:scheduled :deadline))
				  (org-agenda-files '("~/Dropbox/org/main.org" "~/Dropbox/org/work.org" "~/Dropbox/org/inbox.org"))
                                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                                  (org-agenda-span 1)
                                  (org-agenda-show-all-dates nil)
                                  (org-agenda-time-grid nil)
                                  (org-super-agenda-groups '((:name "Deadline past Tasks"
                                                                    :deadline past)
                                                             (:name "Deadline today Tasks"
                                                                    :deadline today)
                                                             (:name "Today Tasks"
                                                                    :scheduled today)
                                                             (:name "Past Scheduled Tasks"
                                                                    :scheduled past)
                                                             (:name "Future Tasks"
                                                                    :deadline future)
							     (:auto-parent))
                                                           )))
                         ))
     ("i" "Inbox Review" ((tags-todo "TODO=\"TODO\"&SCHEDULED=\"\"|TODO=\"NEXT\"&SCHEDULED=\"\""
        			     ((org-agenda-overriding-header "Inbox Tasks")
                                      (org-agenda-files '("~/Dropbox/org/inbox.org"))))
        		  (org-ql-block '(and (not (todo "TODO"))
         		   		      (not (todo "DONE"))
                                              (not (todo "CANCELLED")))
        		   	        ((org-ql-block-header "Notes")
                                         (org-agenda-overriding-header "Other Items")
                                         (org-agenda-files '("~/Dropbox/org/inbox.org"))
                                         ))
                          ))
     ("t" "Inbox Entries TODAY" ((tags "ENTRYDATE>=\"<today>\"&SCHEDULED=\"\""
        			       ((org-agenda-overriding-header "Inbox Tasks")
					(org-agenda-time-grid nil)
					(org-agenda-files '("~/Dropbox/org/inbox.org"))))
				 ))
     ("r" "Review" (
		    (org-ql-block '(and (property "ENTRY_TYPE" "PROJECT")
					(not (descendants (scheduled))))
        		   	  ((org-ql-block-header "Stuck Projects")
                                   (org-agenda-overriding-header "Other Items")
                                   (org-agenda-files '("~/Dropbox/org/main.org" "~/Dropbox/org/work.org"))
				   (org-super-agenda-groups  '((:auto-category t)))
                                   ))
		    (agenda ""
                            ((org-agenda-overriding-header "Tasks in the next 2 weeks")
                             (org-agenda-entry-types '(:scheduled :deadline))
			     (org-agenda-files '("~/Dropbox/org/main.org" "~/Dropbox/org/work.org" "~/Dropbox/org/inbox.org"))
                             (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                             (org-agenda-span 1)
                             (org-agenda-show-all-dates nil)
                             (org-agenda-time-grid nil)
                             (org-super-agenda-groups '((:name "Deadline past Tasks"
                                                               :deadline past)
                                                        (:name "Past Scheduled Tasks"
                                                               :scheduled past)
                                                        (:name "Today Tasks"
                                                               :scheduled today
                                                               :deadline today)
                                                        (:name "Future Tasks"
                                                               :deadline future))
                                                      )))
		    (org-ql-block '(and (property "ENTRY_TYPE" "PROJECT")
					(null (property "GOAL")))
        		   	  ((org-ql-block-header "Projects With no Goal - Add a :goal: property in the drawer")
                                   (org-agenda-overriding-header "Other Items")
                                   (org-agenda-files '("~/Dropbox/org/main.org" "~/Dropbox/org/work.org"))
				   (org-super-agenda-groups  '((:auto-category t)))
                                   ))
		    (org-ql-block '(and (property "ENTRY_TYPE" "PROJECT")
					(null (property "AREA")))
        		   	  ((org-ql-block-header "Projects With Area of responbility - Add a :area: property in the drawer")
                                   (org-agenda-overriding-header "Other Items")
                                   (org-agenda-files '("~/Dropbox/org/main.org" "~/Dropbox/org/work.org"))
				   (org-super-agenda-groups  '((:auto-category t)))
                                   ))
		    (org-ql-block '(and (property "ENTRY_TYPE" "PROJECT")
					(null (deadline)))
        		   	  ((org-ql-block-header "Projects With no Deadline - Add a Deadline to make projects disappear")
                                   (org-agenda-overriding-header "Other Items")
                                   (org-agenda-files '("~/Dropbox/org/main.org" "~/Dropbox/org/work.org"))
				   (org-super-agenda-groups  '((:auto-category t)))
                                   ))
		    (tags-todo "TODO=\"TODO\"-DEPRIORITIZED_PROJECTS-TEMPLATE-PROJECT-SCHEDULED={.+}-DEADLINE={.+}"
			       ((org-agenda-overriding-header "Unplanned Todos")
				(org-agenda-files '("~/Dropbox/org/main.org" "~/Dropbox/org/work.org"))
				(org-super-agenda-groups '((:auto-parent t))))))
      ((org-agenda-block-separator "===================================================================="))
      )

     ("p"  "Report" ((tags "ENTRY_TYPE=\"PROJECT\"&TODO=\"DONE\"&CLOSED>\"<-1w>\""
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
			    (org-super-agenda-groups '((:auto-parent t)))))
		     (tags "improvement&ENTRYDATE>\"<-1w>\""
			   ((org-agenda-span "-7d")
			    (org-agenda-overriding-header "Improvements in the last week")
			    (org-agenda-files '("~/Dropbox/org/log.org"))))
		     ))
     ))

  (org-stuck-projects
   '("+ENTRY_TYPE=\"PROJECT\"-DONE-TEMPLATE-DEFERRED-CANCELLED-TODO=\"DONE\"" ("") ("")
     "\\<IGNORE\\>\\|SCHEDULED:\\|DEADLINE:"))


  (org-directory "~/Dropbox/org")
  ;; (org-mobile-directory "~/Dropbox/Apps/MobileOrg/")
  ;; (org-mobile-inbox-for-pull "~/Dropbox/inbox.org")
  (org-columns-default-format "%50ITEM %TODO %CLOCKSUM %Area %Goal")
  (org-agenda-include-diary t)
  (org-journal-template-entry (concat "* %T [" (system-name)  "]| %^{title} %^G"))
  (org-agenda-window-setup 'only-window)
  (org-crypt-disable-auto-save t)
  (org-crypt-key nil)
  (org-babel-python-command "python3")

  :hook ((org-mode . visual-line-mode)
         (org-mode . reuben/org-mode-hook)
         (org-checkbox-statistics . org-checkbox-todo)
         (org-after-todo-statistics . org-summary-todo)
         (org-blocker . org-block-wip-limit)
	 (org-mode . auto-revert-mode))

  :config
  (add-to-list 'org-modules 'org-id )
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-crypt)
  (add-to-list 'org-modules 'org-checklist)

  (require 'org-crypt)
  (require 'org-depend)
  (require 'org-protocol)
  (require 'org-checklist)
  (require 'gus-links)

  (org-crypt-use-before-save-magic)


  (setq org-directory (expand-file-name "~/Dropbox/org"))
  (setq org-default-inbox-file (concat org-directory "/inbox.org"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-default-journal-file (concat org-directory "/notes.org"))
  (setq org-default-slipbox-file (concat org-directory "/slipbox.org"))
  (setq org-default-log-file (concat org-directory "/log.org"))
  (setq org-show-context-detail (assq-delete-all 'agenda org-show-context-detail))
  (add-to-list 'org-show-context-detail '(agenda . lineage))

  (setq org-capture-templates
  	'(("t" "Todo" entry (file org-default-inbox-file)
  	   "* TODO %^{entry}\n:PROPERTIES:\n:ENTRYDATE:   %(get-inactive-date-string)\n:END:\n %?\n")
	  ("n" "Note" entry (file org-default-inbox-file)
	   "* %^{title}\n:PROPERTIES:\n:ENTRYDATE:   %(get-inactive-date-string)\n:ID: %(uuid-create)\n:END:\n\n%?\n")
  	  ("r" "Code Review" entry (file org-default-inbox-file) "* TOREVIEW Code Review %^{author} [[%^{link}][%^{description}]] [/]\n:PROPERTIES:\n:ENTRYDATE:   %U\n:ID: %(uuid-create)\n:END:\n\n** TODO Questions before the code review [/]\n    - [ ] Describe the problem the author is trying to solve\n    - [ ] Do I think its required\n    - [ ] Backward compatibility: Will this code execute against existing data?\n    - [ ] List the areas of the code the author has changed [0/0]\n** TODO Questions after a code review [/]\n    - [ ] Describe the code change in detail.\n    - [ ] Does the code satisfy the original intent?\n    - [ ] If the code will execute against older data, how does the author handle backward compatibility?\n    - [ ] Does the author have tests to show his method of handling backward compatibility works?\n    - [ ] Does the author have tests that cover all the areas of the code change?\n    - [ ] What are the error scenarios for the code in question?\n    - [ ] Does the author handle these scenarios well\n    - [ ] Are there variables names you don't understand?\n** Questions unrelated to the code review\n" :immediate-finish t)
	  ("l" "Link" entry (file org-default-inbox-file)
	   "* %a\n:PROPERTIES:\n:ENTRYDATE:   %(get-inactive-date-string)\n:END:\n %i" :immediate-finish t)
  	  ("q" "Question" entry (file org-default-inbox-file)
  	   "* QUESTION %^{question} \n%?\n\nEntered on %U\n %i\n")
  	  ("j" "Journal" entry (file org-default-notes-file)
  	   "* %^{title}\n :PROPERTIES:\n :ENTRYDATE:  %(get-active-date-string) \n :DEVICE: %(system-name)\n :END:\n\n" :immediate-finish t)
	  ("i" "Time checkin" entry (file org-default-log-file)
	   "* %T [%(car (split-string (system-name)  \"[\.]\"))]| [ check in ] |%^{title}"
	   :immediate-finish t)
	  ("o" "Time checkout" entry (file org-default-log-file)
	   "* %T [%(car (split-string (system-name)  \"[\.]\"))]| [ check out ] |%^{title}"
	   :immediate-finish t)
  	  ("g" "log" entry (function custom-log-finder)
  	   "* %T [%(car (split-string (system-name)  \"[\.]\"))]| %^{title}  %(add-tag) " :immediate-finish t)))


					;(add-hook 'org-checkbox-statistics-hook 'org-checkbox-todo)

  (require 'seq)
					; (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  (defvar org-wip-limit 20 "Work-in-progress limit")
  (defvar org-wip-state "NEXT")

					;(add-hook 'org-blocker-hook #'org-block-wip-limit)

  (define-key org-mode-map [(f10)] 'org-mark-ring-goto)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (scheme . t)
     (python . t)
     (lisp . t)
     (R . t))))


(use-package org-anki
  :straight t
  :defer t
  :after org)

(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode))

(use-package org-ql
  :straight t
  :defer t
  :after org)

(use-package org-super-agenda
  :straight t
  :hook (after-init . org-super-agenda-mode))

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

;; --------------- Themeing ---------------
(use-package spaceline
  :straight t
  :bind (("C-c t s" . spaceline-toggle-all-the-icons-separator))  ; Add way to trigger loading
  :config
  (spaceline-compile))

(use-package borland-blue-theme
  :straight t)

(use-package spaceline-all-the-icons
  :straight t
  :after spaceline
  :custom
  (spaceline-all-the-icons-separator-type 'arrow))

(use-package spacemacs-theme
  :straight t
  :defer t)

(use-package doom-themes
  :straight t
  :defer t)

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)  ; Load after Emacs starts
  :config
  ;; Height of the mode-line
  (setq doom-modeline-height 25)
  
  ;; Width of the bar on the left
  (setq doom-modeline-bar-width 4)
  
  ;; Show file icon
  (setq doom-modeline-icon t)
  
  ;; Show project name
  (setq doom-modeline-project-detection 'auto)
  
  ;; Show buffer encoding (UTF-8, etc.)
  (setq doom-modeline-buffer-encoding nil) ; Set to t if you want to see it
  
  ;; Show indentation info
  (setq doom-modeline-indent-info nil)
  
  ;; Show checker (flycheck/flymake) info
  (setq doom-modeline-checker-simple-format t)
  
  ;; Show line/column numbers
  (setq doom-modeline-lsp t)
  
  ;; Show workspace/persp name
  (setq doom-modeline-persp-name t)
  
  ;; Show version control info
  (setq doom-modeline-vcs-max-length 12)
  
  ;; Show time (optional)
  (setq doom-modeline-time t)
  (setq doom-modeline-time-icon nil))

(use-package color-theme-modern
  :straight t)


(use-package modus-themes
  :straight t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-custom-auto-reload t)
  (modus-themes-disable-other-themes t)
  (modus-themes-prompts '(italic bold))
  (modus-themes-completions '((matches . (extrabold))
                              (selection . (semibold italic text-also))))
  (modus-themes-org-blocks 'gray-background))

;; --------------- fly check mode ---------------

(use-package flycheck
  :straight t
  :defer t
  :custom
  (flycheck-check-syntax-automatically '(save mode-enable))
  :hook (after-init . global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint json-jsonlist)))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; --------------- company mode ---------------
(use-package company
  :straight t
  :defer t
  :custom
  (company-idle-delay 0.5)
  (company-minimum-prefix-length 2)
  (add-to-list 'company-backends 'company-capf)
  (add-to-list 'company-backends 'company-abbrev)
  (add-to-list 'company-backends 'company-clang)
  (add-to-list 'company-backends 'company-dabbrev)
  (add-to-list 'company-backends 'company-files)
  :hook  ((prog-mode . company-mode))
  :bind
  (:map company-active-map
	("C-n". company-select-next)
	("C-p". company-select-previous)
	("M-<". company-select-first)
	("M->". company-select-last)))

;; --------------- Hydra mode ---------------
(use-package hydra
  :straight t
  :after org
  :config

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

					;  (bind-key "C-c h s" 'jethro/hydra-smerge/body)

;;   (defhydra process-inbox(:exit nil :hint nil
;;                                 :pre (setq rfc/resume-hydra nil)
;;                                 :post (if rfc/resume-hydra
;;                                           (progn
;;                                             (print rfc/resume-hydra)
;;                                             (process-inbox/body))
;;                                         (print rfc/resume-hydra)))
;;     "
;;   _r_: Refile     _a_: Archive     _K_: Kill
;;   _d_: Done       _c_: Cancel      _td_: Schedule for today
;;   _n_: Next Line  _p_: Prev Line
;;   _q_: quit
;; "
;;     ("n" next-line nil :color pink)
;;     ("p" previous-line nil :color pink)
;;     ("a" org-agenda-archive nil :color red)
;;     ("r" org-agenda-refile nil :color red)
;;     ("td" org-agenda-schedule :color blue)
;;     ("K" org-agenda-kill nil :color red :exit nil)
;;     ("d" (lambda () (org-agenda-todo "DONE")) nil :color blue)
;;     ("c" (org-agenda-todo "CANCELLED") nil :color blue)
;;     ("q" nil nil :color red))
;    (define-key org-agenda-mode-map [(f9)] 'process-inbox/body)
  )


(use-package ledger-mode
  :defer t
  :straight t)

(use-package exec-path-from-shell
  :straight t
  :defer t
  :config
  (exec-path-from-shell-initialize))

;; --------------- Writing ---------------
;; Move fringe setup to a proper function
(defun my/olivetti-setup ()
  "Set up olivetti with custom fringe settings."
  (set-window-fringes (selected-window) 0 0))

(use-package olivetti
  :straight t
  :custom
  (olivetti-body-width 0.65)
  (olivetti-minimum-body-width 72)
  (olivetti-recall-visual-line-mode-entry-state t)
  :hook (olivetti-mode . my/olivetti-setup))

(use-package visual-fill-column
  :straight t)

(use-package wc-goal-mode
  :straight t)

;; --------------- Projectile ---------------

(use-package projectile
  :straight t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; --------------- Avy ---------------
(use-package avy
  :straight t
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char))

;; --------------- Neotree ---------------
(use-package neotree
  :straight t)

(use-package smartparens
  :straight t
  :bind (:map smartparens-mode-map
              ("C-<down>" . sp-down-sexp)
              ("C-<up>"   . sp-up-sexp)
              ("M-<down>" . sp-backward-down-sexp)
              ("M-<up>"   . sp-backward-up-sexp)


              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)

              ("C-<right>" . sp-forward-slurp-sexp)
              ("M-<right>" . sp-forward-barf-sexp)
              ("C-<left>"  . sp-backward-slurp-sexp)
              ("M-<left>"  . sp-backward-barf-sexp)

              ("C-M-k" . sp-kill-sexp)
              ("M-k"   . sp-backward-kill-sexp)

              ("M-[" . sp-backward-unwrap-sexp)
              ("M-]" . sp-unwrap-sexp))
  :hook
  (after-init . (lambda() (smartparens-global-mode t))))


(use-package which-key
  :straight t
  :hook (after-init . which-key-mode))

(use-package frame
)

(use-package emacs
  :config
  (setq-default scroll-preserve-screen-position t)
  (setq-default scroll-conservatively 1) ; affects `scroll-step'
  (setq-default scroll-margin 0)

  (define-minor-mode prot/scroll-centre-cursor-mode
    "Toggle centred cursor scrolling behaviour."
    :init-value nil
    :lighter " S="
    :global nil
    (if prot/scroll-centre-cursor-mode
        (setq-local scroll-margin (* (frame-height) 2)
                    scroll-conservatively 0
                    maximum-scroll-margin 0.5)
      (dolist (local '(scroll-preserve-screen-position
                       scroll-conservatively
                       maximum-scroll-margin
                       scroll-margin))
        (kill-local-variable `,local))))

  ;; C-c l is used for `org-store-link'.  The mnemonic for this is to
  ;; focus the Line and also works as a variant of C-l.
  :bind ("C-c L" . prot/scroll-centre-cursor-mode))

(use-package emacs
  :commands (prot/hidden-mode-line-mode prot/cursor-type-mode)
  :hook ((prog-mode . prot/cursor-type-mode)
	 (text-mode . prot/cursor-type-mode)
	 (pascal-mode . (lambda ()
                          ;; Indentation settings
                          (setq tab-width 8)
                          (setq pascal-indent-level 8)
                          (setq pascal-case-indent 8)
                          (setq pascal-auto-newline nil)
                          ;; Set compile command
                          (set (make-local-variable 'compile-command)
                               (concat "fpc " (file-name-nondirectory buffer-file-name))))))
  :config
  ;; Set up FreePascal error recognition for compilation mode
  (with-eval-after-load 'compile
    ;; Clean up any existing freepascal entries
    (setq compilation-error-regexp-alist 
          (remove 'freepascal compilation-error-regexp-alist))
    (setq compilation-error-regexp-alist-alist
          (assq-delete-all 'freepascal compilation-error-regexp-alist-alist))
    
    ;; Add FreePascal error format
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(freepascal
                   "^\\([^( \t\n]+\\)(\\([0-9]+\\),\\([0-9]+\\))[ \t]*\\([^:\n]*\\):"
                   1 2 3))
    
    ;; Activate the freepascal error recognition
    (add-to-list 'compilation-error-regexp-alist 'freepascal))
  
  (setq mode-line-percent-position '(-3 "%p"))
  (setq mode-line-defining-kbd-macro
        (propertize " Macro" 'face 'mode-line-emphasis))
  (setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "  "
                  mode-line-position
                  (vc-mode vc-mode)
                  " "
                  mode-line-modes
                  " "
                  mode-line-misc-info
                  mode-line-end-spaces))

  (setq-default scroll-preserve-screen-position t)
  (setq-default scroll-conservatively 1) ; affects `scroll-step'
  (setq-default scroll-margin 0)

  (define-minor-mode prot/scroll-centre-cursor-mode
    "Toggle centred cursor scrolling behaviour."
    :init-value nil
    :lighter " S="
    :global nil
    (if prot/scroll-centre-cursor-mode
        (setq-local scroll-margin (* (frame-height) 2)
                    scroll-conservatively 0
                    maximum-scroll-margin 0.5)
      (dolist (local '(scroll-preserve-screen-position
                       scroll-conservatively
                       maximum-scroll-margin
                       scroll-margin))
        (kill-local-variable `,local))))

  ;; C-c l is used for `org-store-link'.  The mnemonic for this is to
  ;; focus the Line and also works as a variant of C-l.

  (define-minor-mode prot/hidden-mode-line-mode
    "Toggle modeline visibility in the current buffer."
    :init-value nil
    :global nil
    (if prot/hidden-mode-line-mode
        (setq-local mode-line-format nil)
      (kill-local-variable 'mode-line-format)
      (force-mode-line-update)))

  (blink-cursor-mode -1)

  (define-minor-mode prot/cursor-type-mode
    "Toggle between static block and pulsing bar cursor."
    :init-value nil
    :global t
    (if prot/cursor-type-mode
        (progn
          (setq-local blink-cursor-interval 0.7
                      cursor-type '(bar . 3)
                      cursor-in-non-selected-windows 'hollow)
          (blink-cursor-mode 1))
      (dolist (local '(blink-cursor-interval
                       cursor-type
                       cursor-in-non-selected-windows))
        (kill-local-variable `,local))
      (blink-cursor-mode -1)))  

  ;; (defun setup-theme(frame)
  ;;   (with-selected-frame frame
  ;;     (load-theme 'spacemacs-dark 'no-confirm))
  ;;   (remove-hook 'after-make-frame-functions #'setup-theme)
  ;;   (fmakunbound 'setup-theme))
  )


(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package diminish
  :straight t
  :config
  (diminish 'hyperbole-mode "")
  (diminish 'yas-minor-mode "")
  (diminish 'company-mode "")
  (diminish 'org-roam-ui-mode "")
  (diminish 'projectile-mode "")
  (diminish 'eldoc-mode ""))

(use-package gptel
  :straight t
  :config
  (global-set-key (kbd "C-c g s") 'gptel-send)
  (global-set-key (kbd "C-c g m") 'gptel-menu)
  (global-set-key (kbd "C-c g c") 'gptel)
  (if (fboundp 'my-anthropic-key)
      (setq gptel-backend (gptel-make-anthropic "Claude"
			    :stream t
			    :key 'my-anthropic-key))))

(use-package drag-stuff
  :straight t
  :bind (:map prog-mode-map
	      ("M-<up>" . drag-stuff-up)
	      ("M-<down>" . drag-stuff-down)
	      ("M-<left>" . drag-stuff-left)
	      ("M-<right>" . drag-stuff-right)))

;; (use-package undo-tree
;;   :straight t
;;   :config
;;   (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
;;   (global-undo-tree-mode)
;;   (defadvice undo-tree-make-history-save-file-name
;;       (after undo-tree activate)
;;     (setq ad-return-value (concat ad-return-value ".gz"))))

(use-package change-inner
  :straight t
  :bind ("C-c i" . change-inner))

(use-package devdocs
  :straight t
  :defer t)

(defun my-setup-new-frame (frame)
  "Set up new frames with desired size/position."
  (with-selected-frame frame
    (when (display-graphic-p frame)
      ;; Choose one of these:
      ;; Full maximize:
      (toggle-frame-maximized)
      
      ;; Or horizontal maximize:
      ;; (set-frame-parameter frame 'fullscreen 'maximized-horizontally)
      
      ;; Or specific size:
      ;; (set-frame-width frame 120)
      ;; (set-frame-height frame 50)
      )))

(use-package emacs
  :custom
  ;; Core completion behavior
  (completion-styles '(orderless basic))           ; Use basic + flexible matching
  ;; (completion-ignore-case t)                  ; Case-insensitive completion
  ;; (completion-auto-select t)                  ; Auto-select first completion
  ;; (completion-auto-help 'visible)             ; Show *Completions* buffer when needed

  ;; ;; Completions buffer formatting
  ;; (completions-format 'one-column)            ; Single column layout
  ;; (completions-sort 'historical)              ; Sort by minibuffer history
  (completions-max-height 20)                 ; Limit to 20 completions

  ;; Performance settings
  (completion-cycle-threshold 3)              ; Cycle if <= 3 completions
  (completion-category-overrides              ; Per-category completion styles
   '((file (styles . (partial-completion)))))

  :config
  (add-hook 'after-make-frame-functions 'my-setup-new-frame)
  (windmove-default-keybindings)
  (require 'misc)
  (global-set-key (kbd "M-f") 'forward-to-word)
  
  ;; Also apply to the initial frame if starting GUI Emacs directly
  (when (display-graphic-p)
    (my-setup-new-frame (selected-frame)))

  ;; Enable helpful completion features
  (minibuffer-depth-indicate-mode 1) )

(use-package completion-preview
  :hook (after-init . global-completion-preview-mode))

;; ------- Programming ---------------
(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (java-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (typescript-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ;; Essential navigation
              ("M-." . eglot-find-implementation)
              ("M-," . pop-tag-mark)
              
              ;; Language server actions - flat C-c s prefix
              ("C-c s a" . eglot-code-actions)
              ("C-c s r" . eglot-rename)
              ("C-c s f" . eglot-format-buffer)
              ("C-c s h" . eldoc-doc-buffer)              ; ← Changed this
              ("C-c s i" . eglot-find-implementation)
              ("C-c s t" . eglot-find-typeDefinition)
              ("C-c s o" . eglot-code-action-organize-imports)
              ("C-c s d" . flymake-show-buffer-diagnostics)
              ("C-c s D" . flymake-show-project-diagnostics)
              ("C-c s R" . eglot-reconnect)
              ("C-c s S" . eglot-shutdown)
              
              ;; Keep error navigation simple
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect nil)
  (eglot-extend-to-xref t))

(use-package java-ts-mode
  :mode "\\.java\\'"
  :hook ((java-ts-mode . (lambda ()
                           (setq c-basic-offset 4
                                 tab-width 4
                                 indent-tabs-mode nil))))
  :custom
  (java-ts-mode-indent-offset 4))

(straight-use-package '(eglot :type built-in))
(straight-use-package '(project :type built-in))


(use-package tuareg
  :straight t
  :hook (tuareg-mode . eglot-ensure))

(use-package ocaml-eglot
  :straight t
  :after (tuareg  eglot)
  :hook
  (tuareg-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure))

(use-package geiser
  :straight t
  :hook (scheme-mode . geiser-mode))

(use-package geiser-mit
  :straight t
  :defer t
  :after geiser
  :custom
  (geiser-active-implementations '(mit)))


;; --------------- Web Mode ---------------

(defun web-mode-init-hook ()
  "Hooks for Web mode. Adjust indent."
  (setq web-mode-markup-indent-offset 4))

(defun web-mode-init-prettier-hook ()
  "Set up prettier for web-mode."
  (add-node-modules-path)
  (prettier-js-mode))


(use-package web-mode
  :straight t
  :mode (("\\.jsx?\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :custom
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  :hook (web-mode . web-mode-init-hook))

(use-package add-node-modules-path
  :straight t
  :hook (flycheck-mode . add-node-modules-path))

(use-package prettier-js
  :straight t
  :hook (web-mode . web-mode-init-prettier-hook))

(use-package emmet-mode
  :straight t
  :hook (web-mode . emmet-mode))

(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'")

(use-package tide
  :straight t
  :after (typescript-mode flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))


(use-package zig-mode
  :straight t
  :custom (zig-format-on-save nil)
  :mode "\\.zig\\'")

(use-package slime
  :straight t
  :mode "\\.lisp\\'"
  :custom
  (inferior-lisp-program "sbcl")
  (slime-contribs '(slime-fancy)))


(use-package bury-successful-compilation
  :straight t
  :hook
  (c-mode . bury-successful-compilation))

;; --------------- Rust Config ---------------
(use-package rustic
  :straight t  ; Fixed: was missing 't'
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :custom
  (lsp-eldoc-hook nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-signature-auto-activate nil)
  (rustic-format-on-save t)
  :hook (rustic-mode . rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package yasnippet
  :straight t
  :custom
  (yas-indent-line nil)
  :hook (after-init . yas-global-mode)
  :config
  (add-to-list 'yas-snippet-dirs "~/Dropbox/yassnippet/"))

(use-package tide
  :straight t
  :defer t
  :after (typescript-mode company-mode flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package restclient
  :defer t
  :straight t)

;; (use-package ligature
;;   :straight t
;;   :config
;;     (ligature-set-ligatures 'haskell-mode
;;                         '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
;;                           ;; =:= =!=
;;                           ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
;;                           ;; ;; ;;;
;;                           (";" (rx (+ ";")))
;;                           ;; && &&&
;;                           ("&" (rx (+ "&")))
;;                           ;; !! !!! !. !: !!. != !== !~
;;                           ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
;;                           ;; ?? ??? ?:  ?=  ?.
;;                           ("?" (rx (or ":" "=" "\." (+ "?"))))
;;                           ;; %% %%%
;;                           ("%" (rx (+ "%")))
;;                           ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
;;                           ;; |->>-||-<<-| |- |== ||=||
;;                           ;; |==>>==<<==<=>==//==/=!==:===>
;;                           ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
;;                                           "-" "=" ))))
;;                           ;; \\ \\\ \/
;;                           ("\\" (rx (or "/" (+ "\\"))))
;;                           ;; ++ +++ ++++ +>
;;                           ("+" (rx (or ">" (+ "+"))))
;;                           ;; :: ::: :::: :> :< := :// ::=
;;                           (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
;;                           ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
;;                           ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
;;                                           "="))))
;;                           ;; .. ... .... .= .- .? ..= ..<
;;                           ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
;;                           ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
;;                           ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
;;                           ;; *> */ *)  ** *** ****
;;                           ("*" (rx (or ">" "/" ")" (+ "*"))))
;;                           ;; www wwww
;;                           ("w" (rx (+ "w")))
;;                           ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
;;                           ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
;;                           ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
;;                           ;; << <<< <<<<
;;                           ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
;;                                           "-"  "/" "|" "="))))
;;                           ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
;;                           ;; >> >>> >>>>
;;                           (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
;;                           ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
;;                           ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
;;                                        (+ "#"))))
;;                           ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
;;                           ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
;;                           ;; __ ___ ____ _|_ __|____|_
;;                           ("_" (rx (+ (or "_" "|"))))
;;                           ;; Fira code: 0xFF 0x12
;;                           ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
;;                           ;; Fira code:
;;                           "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
;;                           ;; The few not covered by the regexps.
;;                           "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
;;     (add-hook 'haskell-mode-hook 'ligature-mode))

;;; -----  hyperbole
  
  (defun looking-at-work-item()
    (or
     (looking-at "W-[0-9]+")
     (save-excursion
       (backward-word-strictly 2)
       (looking-at "W-[0-9]+"))
     (save-excursion
       (backward-word-strictly 1)
       (looking-at "W-[0-9]+"))
     (looking-at "a07.*")
     (save-excursion
       (backward-word-strictly 1)
       (looking-at "a07.*"))))
       
  (defun get-work-item-text()
    (let* ((match-data (match-data))
           (start (first match-data))
           (end (second match-data)))
      (list (buffer-substring-no-properties start end) start end)))
      
  (defun in-org-property()
    (and (hsys-org-mode-p)
	 (org-at-property-p)))
	 
  (defun org-properties-search()
    (interactive)
    (if (in-org-property)
	(let* ((property-name (org-read-property-name))
	       (property-value (org-entry-get (point) property-name)))
	  (hact 'org-tags-view nil (concat property-name "={" property-value "}")))))

(use-package hyperbole
  :straight t
  :defer t
  :hook ((org-mode . (lambda () (require 'hyperbole)))
         (text-mode . (lambda () (require 'hyperbole))))
  :bind (("<f6>" . gbut:act)
         ("C-<return>" . action-key))
  :config
  (global-unset-key (kbd "M-<return>"))
	  
  (defib gus()
    "Gus links"
    (if (looking-at-work-item)
	(cl-destructuring-bind (text start end) (get-work-item-text)
          (ibut:label-set text start end)
          (hact 'www-url (concat "https://gus.my.salesforce.com/apex/ADM_WorkLocator?bugorworknumber=" text)))
      nil))
      
(defun org-property-button-info ()
  "Get button info for org property at point."
  (when (in-org-property)
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds)))
      (when (and start end)
        (list (buffer-substring-no-properties start end) start end)))))

(defib org-property-search()
  "org property search"
  (when-let ((button-info (org-property-button-info)))
    (cl-destructuring-bind (text start end) button-info
      (ibut:label-set text start end)
      (org-properties-search)))))


(use-package rg
  :straight t
  :defer t
  :commands (rg rg-project rg-dwim rg-literal rg-regexp)
  :config
  (rg-enable-default-bindings))
;;; -----


(use-package pdf-tools
  :straight t
  :custom
  (doc-view-resolution 300))

(provide 'use-package-config)
;;; use-package-config.el ends here
