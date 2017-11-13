;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs lisp load paths
;;


(defconfig org-mode-config
  (require 'org-install)
  (check-and-install-if-absent 'org-bullets)
  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-hide-leading-stars 't)
  (setq org-log-done 'time)
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-toc nil)
  (setq org-pretty-entities t)
  (setq org-reverse-note-order t)
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-clock-persist t)
  (setq org-clock-idle-time 60)
  (setq org-clock-history-length 35)
  (setq org-clock-in-resume t)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-id-method 'uuidgen)
  (setq org-enforce-todo-dependencies t)
  (setq org-hide-leading-stars t)

  (add-hook 'after-save-hook 'sync-index-org)
  ;; search 5 levels deep in org files.
  (setq org-refile-targets '((org-agenda-files :maxlevel . 5)))

  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")
		(sequence "TOBUY(b)" "TOPACK(p)" "|" "BOUGHT(g)" "PACKED")
		(sequence "TOREAD(r)" "|" "READ")
		(sequence "QUESTION(q)" "|" "ANSWERED(a@)"))))

  (setq org-todo-keyword-faces
	(quote (("TODO" :foreground "red1" :weight bold)
		("NEXT" :foreground "turquoise" :weight bold)
		("DONE" :foreground "light green" :weight bold)
		("WAITING" :foreground "DarkOrange2" :weight bold)
		("TOREAD" :foreground "DarkOrange2" :weight bold)
		("HOLD" :foreground "magenta" :weight bold)
		("CANCELLED" :foreground "light green" :weight bold)
		("READ"  :foreground "light green" :weight bold)
		("QUESTION" :foreground "DarkOrange2" :weight bold)
		("ANSWERED" :foreground "light green" :weight bold))))

  (setq org-todo-state-tags-triggers
	(quote (("CANCELLED" ("CANCELLED" . t))
		("WAITING" ("WAITING" . t))
		(done ("WAITING"))
		("TODO" ("WAITING") ("CANCELLED") )
		("NEXT" ("WAITING") ("CANCELLED") )
		("DONE" ("WAITING") ("CANCELLED") ))))

  (setq unscheduled-tasks-search-string "+TODO=\"TODO\"-SCHEDULED={.+}-DEADLINE={.+}-TEMPLATE-IGNORE_UNSCHEDULED")

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

  (add-hook 'org-checkbox-statistics-hook 'org-checkbox-todo)


  (setq org-agenda-custom-commands
	'(
	  ("q" tags-todo "TODO=\"QUESTION\"")
  	  ("d" "Daily Agenda" ((agenda "Daily Agenda" ((org-agenda-span 1)
						       (org-agenda-skip-function 'skip-done-functions-or-projects)
						       (org-agenda-overriding-header "Daily Agenda")))
			       (tags-todo "TODO=\"NEXT\"&SCHEDULED<\"<+1w>\"|TODO=\"NEXT\"-SCHEDULED={.+}-DEADLINE={.+}|TODO=\"NEXT\"&DEADLINE<\"<+1w>\"" ((org-agenda-overriding-header "Next Items")))
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

  (setq org-stuck-projects
	'("+PROJECT-DONE-TEMPLATE-TODO=\"DONE\"" ("NEXT") ()
	  "\\<IGNORE\\>"))

  (setq org-directory (expand-file-name "~/Dropbox"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-default-journal-file (concat org-directory "/notes.org"))

  (setq org-directory "~/Dropbox")
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg/")
  (setq org-mobile-inbox-for-pull "~/Dropbox/inbox.org")

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let ((todo-state (org-get-todo-state)) beg end)
      (unless (not todo-state)
	(let (org-log-done org-log-states)   ; turn off logging
	  (org-todo (if (= n-not-done 0) "DONE"
		      (if (> n-done 0) "NEXT" "TODO")))))))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  ;; include emacs diary entries in agenda view
  (setq org-agenda-include-diary t)

  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
	   "* TODO %^{entry}\n:PROPERTIES:\n:ENTRYDATE:%U\n:END:\n %?\n")
	  ("r" "Lookup Entry in region" entry (file+headline org-default-notes-file "Lookup")
	   "* %i :LOOKUP:\n")
	  ("l" "Lookup Entry" entry (file+headline org-default-notes-file "Lookup")
	   "* %?  :LOOKUP:\n %i \n")
	  ("q" "Question" entry (file+datetree org-default-journal-file)
	   "* QUESTION %^{question} \n%?\n\nEntered on %U\n %i\n")
	  ("j" "Journal" entry (file+datetree org-default-journal-file)
	   "* %^{title} %^G \n\n%?\n\nEntered on %U\n %i\n")
	  ("s" "Standup" entry (file+datetree org-default-notes-file)
	   "*   %^{title} :STANDUP:\n:PROPERTIES:\n:COLUMNS: %50ITEM %ENTRYDATE\n:ENTRYDATE: %u\n:END:\n%?\n\nEntered on %U\n %i\n"))))

(defconfig c-mode-config
  (setq basic-c-offset 8)
  (add-hook 'c-mode-hook 'my-c-mode-hook))

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
  (check-and-install-if-absent 'swiper-helm)

  (require 'helm)
  (require 'helm-config)
  (require 'helm-org-rifle)
  (require 'swiper-helm)
 
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-set-key (kbd "C-s") 'swiper-helm)
  (global-set-key (kbd "C-r") 'swiper-helm)
  
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

  (helm-mode 1))

(defconfig plantuml-mode-config
  (check-and-install-if-absent 'plantuml-mode)
  (require 'plantuml-mode)
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode)))

(defconfig diary-config
  (setq diary-file "~/Dropbox/emacsdiary.txt")
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files))

(defconfig utf8-config
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  ;; set the default encoding system
  (prefer-coding-system 'utf-8)
  (setq default-file-name-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  ;; backwards compatibility as default-buffer-file-coding-system
  ;; is deprecated in 23.2.
  (if (boundp buffer-file-coding-system)
      (setq buffer-file-coding-system 'utf-8)
    (setq default-buffer-file-coding-system 'utf-8)))

(defconfig nov-mode-config
  (check-and-install-if-absent 'nov)
  (require 'nov)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
			     :height 1.0))
  (add-hook 'nov-mode-hook 'my-nov-font-setup))

(defconfig eshell-config
  (setq eshell-buffer-maximum-lines 20000)
  (defun my/truncate-eshell-buffers ()
    "Truncates all eshell buffers"
    (interactive)
  (save-current-buffer
    (dolist (buffer (buffer-list t))
      (set-buffer buffer)
      (when (eq major-mode 'eshell-mode)
        (eshell-truncate-buffer)))))
  
  ;; After being idle for 5 seconds, truncate all the eshell-buffers if
  ;; needed. If this needs to be canceled, you can run `(cancel-timer
  ;; my/eshell-truncate-timer)'
  (setq my/eshell-truncate-timer
	(run-with-idle-timer 5 t #'my/truncate-eshell-buffers)))
