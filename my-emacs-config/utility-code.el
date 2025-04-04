
(require 'cl)

(defun aquamacs-p()
  (string-match "Aquamacs" (version)))

(defun gnu-emacs-p()
  (string-match "GNU Emacs" (version)))

(defun linux-p()
  (string= system-type "gnu/linux"))

(defun darwin-p()
  (string= system-type "darwin"))

(defun make-backup-file-name(file)
  (concat "~/trash/emacsAutosave/" (file-name-nondirectory file) "~"))

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun grep-ffap ()
  "This is a wrapper over ffap, to jump to the line in file found by grep -n."
  (interactive)
  ;; Figure out if the current line is of the format <filename>:<line number>: <text>
  (let* ((split-string (split-string (buffer-substring-no-properties (line-beginning-position)
								     (line-end-position))
				     ":"))
	 (file-name (car split-string))
	 (line-number (cadr split-string)))
    (if (and  (> (length split-string) 2)
	      (not (null line-number)))
	(progn
	  (ffap (car split-string))
	  (goto-line (string-to-number line-number)))
      (ffap))))

(defun fullscreen (&optional f)
  (interactive)
  (set-frame-parameter f 'fullscreen
		       (if (frame-parameter f 'fullscreen) nil 'fullboth)))


(defun make-emacs-translucent()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha 80))

(defun make-emacs-almost-tranparent()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha 60))


(defun make-emacs-opaque()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha 100))

(defun make-emacs-transparent()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha 40))


(defun get-number-str(day)
  (interactive)
  (cl-destructuring-bind (first-char second-char)
      (butlast (rest (split-string day "")))
    (if (equal first-char "1")
	"th"
      (cond ((equal second-char "1") "st")
	    ((equal second-char "2") "nd")
	    ((equal second-char "3") "rd")
	    (t "th")))))


(defun indent()
  (interactive)
  (when indent-region-function
    (save-excursion
      (funcall indent-region-function
	       (line-beginning-position)
	       (line-end-position)))))

(defun get-date()
  "Return a formatted date [Month Day Year, DayOfWeek]"
  (interactive "*")
  (let* ((cur-time (current-time))
         (day (format-time-string "%d" cur-time))
         (date (format-time-string (concat "[%B %d"
					   (get-number-str day)
					   " %Y, %A]") cur-time)))
    date))

(defun get-date-time ()
  "Return a formatted date as [Month Day Year, DayOfWeek Hour:Minute AM/PM Timezone]"
  (interactive "*")
  (let* ((cur-time (current-time))
         (day (format-time-string "%d" cur-time))
         (date (format-time-string (concat "[%B %d"
                                           (get-number-str day)
                                           " %Y, %A %R %p %Z]") cur-time)))
      date))



(defun reuben/get-org-date-time()
  "Return date time in a format that org mode understands."
  (let* ((cur-time (current-time)))
    (format-time-string "%Y-%m-%d %a %H:%M:%S %z" cur-time)))

(defun reuben/get-active-org-date-time()
  "Return an active timestamp for org mode."
  (concat "<" (reuben/get-org-date-time) ">"))

(defun reuben/get-inactive-org-date-time()
  "Return an inactive time stamp string for org mode."
  (concat "[" (reuben/get-org-date-time) "]"))

(defun insert-date()
  (interactive)
  (insert (get-date)))

(defun insert-date-time()
  (interactive)
  (insert (get-date-time)))

(defun earliest-splunk-date()
  (let* ((cur-time (current-time))
         (date (format-time-string (concat "%m/%d/%Y:00:00:00") cur-time)))
     date))

(defun latest-splunk-date()
  (let* ((cur-time (current-time))
         (date (format-time-string (concat "%m/%d/%Y:23:59:59") cur-time)))
     date))


(defun init-c-file()
  (interactive)
  (when (= (point-max) 1) ;; Insert these lines if and only if we have
    ;; a new buffer
    (insert "#include<stdio.h>\n")
    (insert "#include<stdlib.h>\n")
    (insert "#include<assert.h>\n\n\n")
    (insert "#define ASSERTS\n")
    (goto-line (- (line-number-at-pos) 3))))

(defun insert-assert()
  (interactive)
  (let ((start-point (point)))
    (insert "#ifdef ASSERTS\n")
    (insert "assert();\n")
    (insert "#endif\n")
    (indent-region start-point (point))
    (goto-line (- (line-number-at-pos) 2))
    (goto-char (search-forward "("))
    (indent-line)))

(defun my-c-mode-hook()
  (define-key c-mode-map "\C-cn" 'insert-assert)
  (init-c-file))

(defun has-disk-file-p(buffer)
  "Checks if a file has been saved."
  (if (bufferp buffer)
      (not (null (buffer-file-name buffer)))
    (print "Function requires a buffer")))

(defun set-my-margins(arg)
  (interactive "nMargin Width: ")
  (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) arg arg ))


(defun switch-to-shell-with-current-cwd()

  "Switches to a shell buffer with the current working directory
being the working directory of the buffer"
  (interactive)
  (if (has-disk-file-p (current-buffer))
      (let ((current-directory (file-name-directory (buffer-file-name (current-buffer)))))
	(shell)
	(pop-to-buffer (get-buffer "*shell*"))
	(insert (concat "cd " current-directory ";")))
    (print "This buffer needs to be saved")))



;;;;;;;;;;;;;;;;;;;;;;;
;; Config code

(defvar *config-table* (make-hash-table))

(defmacro defconfig(config-name &rest body)
  `(puthash ',config-name
	    (lambda()
	      ,@body) *config-table*))

(defun load-config(config-name)
  (interactive)
  (let ((config-value (gethash config-name *config-table* nil)))
    (if config-value
	(funcall config-value)
      (progn
	(message "Config for %S not available" config-name)
	(format "Could not load config for " config-name)))))

(defun list-configs()
  (interactive)
  (maphash '(lambda(key value)
	      (print key))
	   *config-table*))

(defun diary-config()
  (interactive)
  (load-config 'color-theme)
  (color-theme-dark-laptop)
  (load-config 'emacs-graphical-font)
  (load-config 'thin-cursor-config)
  (forward-char (point-max))
  (auto-fill-mode)
  (text-mode)
  (load (expand-file-name "~/emacs/my-emacs-config/my-key-bindings.el")))

(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
					 'fullboth)))

(defun sync-index-org()
  "Copies the index.org file from the dropbox dir to the mobile org dir so that it can be accessed on my phone"
  (if (string-match "index\.org$" (buffer-file-name))
      (progn
        (copy-file (buffer-file-name)
                   (expand-file-name "~/Dropbox/Apps/MobileOrg/")
                   t)
        (shell-command (concat (print (if (equal system-type 'darwin)
					  "md5 "
					"md5sum "))
			       (buffer-file-name) " > "
                               (expand-file-name "~/Dropbox/Apps/MobileOrg/checksums.dat"))))))

(defun my-appt-display (min-to-app new-time msg)
  (my-appt-send-notification
   (format "'Appointment in %s minutes'" min-to-app)
   (format "'%s'" msg)))

(defun set-writing-width()
  (interactive)
  (let ((width (max (floor (/ (* 0.6 (window-total-width)) 2)) 30)))
    (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) width width)
    (print width)))

;; (defun reset-window-margin()
;;   (interactive)
;;   (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 0 0)
;;   (set-fringe-mode '(4 . 4)))



;; (defun writing-mode()
;;   (interactive)
;;   (require 'wc-goal-mode)
;;   (wc-goal-mode)
;;   (visual-line-mode)
;;   (set-background-color "#FCFCFC")
;;   (set-foreground-color "#1A1A1A")
;;   (set-cursor-color "#07BBF2")
;;   (setq-default line-spacing 4)
;;   (set-face-attribute 'mode-line (selected-frame) :background "#fFCFCFC" :overline "#fFCFCFC" :foreground "gray")
;;   (set-frame-font "Helvetica 55 Roman 12")
;;   (set-writing-width)
;;   (set-fringe-mode 0))

(defun count-words (start end)
  "Print number of words in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (count-matches "\\sw+"))))

(defun check-and-install-if-absent(package-name)
  "This function checks if a package is installed, if not it installs it.

It requires the standard emacs package manager to be working."
  (if (not (package-installed-p package-name))
      (progn (package-install package-name)
	     t)
    nil))

(defun font-exists-p(name)
  "Checks if the font exists. This function expects name to be a string"
  (find-font (font-spec :name name)))

(defun org-buffer-p(buffer)
  (string-suffix-p ".org" (buffer-name buffer)))

(defun refresh-org-buffers-helper(buffer-list)
  (when (not (null buffer-list))
    (save-excursion
      (switch-to-buffer (car buffer-list))
      (revert-buffer t (not (buffer-modified-p)) t)
      (refresh-org-buffers-helper (cdr buffer-list)))))

(defun refresh-org-buffers ()
    "Reloads all open org buffers from disk if they have not been changed"
    (interactive)
    (let ((current-buffer (current-buffer)))
      (save-excursion
	(refresh-org-buffers-helper (remove-if-not #'org-buffer-p (buffer-list)))
	(switch-to-buffer current-buffer))))

 (defvar saved-window-configuration nil)
(defun push-window-configuration ()
  (interactive)
  (push (current-window-configuration) saved-window-configuration))

(defun pop-window-configuration ()
  (interactive)
  (let ((config (pop saved-window-configuration)))
    (if config
	(set-window-configuration config)
      (if (> (length (window-list)) 1)
	  (delete-window)
	(bury-buffer)))))

(defun disable-all-themes()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun uuid-create ()
  "Return a newly generated UUID. This uses a simple hashing of variable data."
  (let ((s (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                        (user-uid)
                        (emacs-pid)
                        (system-name)
                        (user-full-name)
                        user-mail-address
                        (current-time)
                        (emacs-uptime)
                        (garbage-collect)
                        (random)
                        (recent-keys)))))
    (format "%s-%s-3%s-%s-%s"
            (substring s 0 8)
            (substring s 8 12)
            (substring s 13 16)
            (substring s 16 20)
            (substring s 20 32))))

(defun insert-uuid ()
  "Inserts a new UUID at the point."
  (interactive)
  (insert (uuid-create)))

(defadvice org-archive-subtree (around fix-hierarchy activate)
  (let* ((fix-archive-p (and (not current-prefix-arg)
                             (not (use-region-p))))
         (afile (car (org-archive--compute-location
		      (or (org-entry-get nil "ARCHIVE" 'inherit) org-archive-location))))
         (buffer (or (find-buffer-visiting afile) (find-file-noselect afile))))
    ad-do-it
    (when fix-archive-p
      (with-current-buffer buffer
        (goto-char (point-max))
        (while (org-up-heading-safe))
        (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
               (path (and olpath (split-string olpath "/")))
               (level 1)
               tree-text)
          (when olpath
            (org-mark-subtree)
            (setq tree-text (buffer-substring (region-beginning) (region-end)))
            (let (this-command) (org-cut-subtree))
            (goto-char (point-min))
            (save-restriction
              (widen)
              (-each path
                (lambda (heading)
                  (if (re-search-forward
                       (rx-to-string
                        `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
                      (org-narrow-to-subtree)
                    (goto-char (point-max))
                    (unless (looking-at "^")
                      (insert "\n"))
                    (insert (make-string level ?*)
                            " "
                            heading
                            "\n"))
                  (cl-incf level)))
              (widen)
              (org-end-of-subtree t t)
              (org-paste-subtree level tree-text))))))))

(defun rfc-2822-time-format()
  (let ((cur-time (current-time)))
    (format-time-string "%a, %d %b %Y %H:%M:%S %z" cur-time)))


(defun reuben/goto-parent()
  (outline-previous-heading)
  (while (> (outline-level) 2)
    (org-up-heading-all 1)))

(defun reuben/set-id-property(id custom-id date-entry)
  (if (null id)
      (let ((new-id (org-id-new)))
	(org-set-property  "ID" new-id)
	(if (null custom-id)
	    (org-set-property  "CUSTOM_ID" new-id))
        (if (null date-entry)
            (org-set-property "ENTRYDATE"  (reuben/get-inactive-org-date-time))))))

(defun reuben/set-ids()
  (interactive)
  (if (or (equalp (buffer-name) "inbox.org" )
          (equalp (buffer-name) "slipbox_raw.org"))
      (save-excursion
	(goto-char (point-max))
	;; Go up the tree
	(reuben/goto-parent)
	(while (> (point) 1)
	  (let* ((id (org-id-get))
		 (custom-id (org-entry-get (point) "CUSTOM_ID"))
                 (date-entry (org-entry-get (point) "ENTRYDATE")))
	    (reuben/set-id-property id custom-id date-entry))
	  (outline-get-last-sibling))))
	(save-buffer))


(defun reuben/remove-text-properties(text)
  (if (zerop (length text))
      ""
    (let* ((start 0)
	   (end (length text))
	   (str (substring text start end)))
      (set-text-properties start end nil str)
      str)))

(defun reuben/get-heading-text(point)
  (let* ((heading (org-get-heading))
	 (start 0)
	 (end (length heading)))
    (set-text-properties start end nil heading)
    heading))

(defun reuben/org-has-child-p ()
  (interactive)
  (save-excursion
    (org-goto-first-child)))

;; -- used to set areas for PARA
(defun reuben/get-areas()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((areas '())
	   (areas-search-string (if (equalp (buffer-name) "work.org") "^* Areas Work" "^* Areas Home"))
	   (areas-heading-location (search-forward-regexp areas-search-string nil t)))
      (if (not (null areas-heading-location))
	  (progn
	    (if (reuben/org-has-child-p)
		(progn
		  (org-goto-first-child)
		  (setq areas (append areas (list (reuben/get-heading-text (point)))))
		  (cl-assert (org-at-heading-p) t "We should be at a heading")
		  (let ((previous-point (point)))
		    (org-forward-heading-same-level 1 t)
		    (append areas (list (reuben/get-heading-text (point))))
		    (while (not (equalp previous-point (point)))
		      (setq areas (append areas (list (reuben/get-heading-text (point)))))
		      (setq previous-point (point))
		      (org-forward-heading-same-level 1 t))
		     areas))
	      '()))
	'()))))

(defun reuben/mark-word()
  (interactive)
  (backward-word)
  (set-mark (point))
  (forward-word))

(defun reuben/update-areas-string()
  (interactive)
  (if (not (null (string-match "\.org$"
			       (buffer-name))))
    (save-excursion
      (goto-char (point-min))
      (let* ((areas (reuben/get-areas))
	     (areas-string (apply 'concat (mapcar (lambda(g)
						    (concat "\"" g "\" "))
						  areas)))
	     (location-of-property  (search-forward "#+PROPERTY: Area_ALL " nil t)))
	(if (and (null location-of-property)
		 (not (null areas)))
	    (progn
	      (goto-char (point-min))
	      (insert "#+PROPERTY: Area_ALL ")
	      (insert areas-string)
	      (newline)))
	(if (and (null areas)
		 (not (null location-of-property)))
	    (progn
	      (goto-char location-of-property)
	      (let* ((a	      (beginning-of-line))
		     (beginning-of-line  (point))
		     (p (end-of-line))
		     (end-of-line (point)))
		(delete-region beginning-of-line end-of-line))))
	(if (and (not (null location-of-property))
		 (not (null areas)))
	    (let* ((point-start (point))
		   (p (end-of-line))
		   (point-end (point))
		   (existing-areas-string (reuben/remove-text-properties (buffer-substring point-start point-end))))
	      (if (not (equalp existing-areas-string
			       areas-string))
		  (progn
		    (delete-region point-start point-end)
		    (insert areas-string))))))
      (save-buffer))))


(defun reuben/update-projects-hook()
  (reuben/update-projects)
  (save-buffer))

(defun reuben/update-projects()
  (if (or (equalp (buffer-name) "main.org")
	  (equalp (buffer-name) "work.org"))
      ;; Find the Projects Home or Projects Work
      (save-excursion
	(goto-char (point-min))
	(let* ((projects-search-string (if (equalp (buffer-name) "work.org") "^* Projects Work" "^* Projects Home"))
	       (projects-heading-location (search-forward-regexp projects-search-string nil t))
	       (count ?A))
	  (if (reuben/org-has-child-p)
	      (progn
		(org-goto-first-child)
		(cl-assert (org-at-heading-p) t "We should be at a heading")
		(let ((previous-point -1))
		  (while (not (equalp previous-point (point)))
		    ;; -- Code for number and update
		    (if (reuben/is-number-present-and-has-changed count)
			(reuben/update-heading-text count))
		    (setq count (+ count  1))

		    (if (null (org-entry-get (point) "ENTRY_TYPE"))
			(org-entry-put (point) "ENTRY_TYPE" "PROJECT"))
		    ;; -- Loop code
		    (setq previous-point (point))
		    (org-forward-heading-same-level 1 t))
		  )))))))

(defun reuben/is-number-present-and-has-changed(new-count)
  (save-excursion
    (beginning-of-line)
    (let* ((line-end    (line-end-position))
	   (point-start (search-forward "<" line-end t))
	   (point-end   (search-forward ">" line-end t))
	   (str         (if (and (not (null point-start)) (> point-start 0)
			     (not (null point-end)) (> point-end 0))
			    (buffer-substring point-start (- point-end 1))
			  nil)))
      (if (null str)
	  t
	(not (equal (string-to-char str) new-count))))))

(defun reuben/update-heading-text(count)
  (save-excursion
    (beginning-of-line)
    (let* ((line-end (line-end-position))
	   (point-start (search-forward "<" line-end t))
	   (point-end (search-forward ">" line-end t)))
      (if (and (not (null point-start)) (> point-start 0)
	       (not (null point-end)) (> point-end 0))
	  (progn
	    (delete-region (- point-start 1)  (+ point-end 1) ))))
    (let* ((contains-todo (third (org-heading-components)))
	   (num (char-to-string count)))
      (beginning-of-line)
      (if contains-todo
	  (progn
	    (forward-word)
	    (forward-word)
	    (backward-word))
	(progn
	  (forward-word)
	  (backward-word)))
      (insert "<")
      (insert num)
      (insert "> "))))

