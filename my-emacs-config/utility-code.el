(require 'cl)

(defun aquamacs-p()
  (string-match "Aquamacs" (version)))

(defun gnu-emacs-p()
  (string-match "GNU Emacs" (version)))

(defun carbon-emacs-p()
  (and (string-match "GNU Emacs" (version))
       (string-match "Carbon Version" (version))))

(defun linux-p()
  (string= system-type "gnu/linux"))

(defun darwin-p()
  (string= system-type "darwin"))

(print system-type)


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
  (destructuring-bind (first-char second-char)
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

(defun get-date ()
  "Insert the current date according to the variable
\"insert-date-format\"."
  (interactive "*")
  (let* ((cur-time (current-time))
         (day (format-time-string "%d" cur-time))
         (date (format-time-string (concat "[%B %d" 
                                           (get-number-str day) 
                                           " %Y, %A %R %p %Z]") cur-time)))
      date))

(defun insert-date()
  (interactive)
  (insert (get-date)))

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

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))


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

(defun reset-window-margin()
  (interactive)
  (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 0 0)
  (set-fringe-mode '(4 . 4)))

(defun writing-mode()
  (interactive)
  (wc-goal-mode)
  (longlines-mode)
  (set-cursor-color "#07BBF2")
  (setq-default line-spacing 5)
  (set-frame-font "Inconsolata 18")
  (set-writing-width)
  (set-fringe-mode 0))

  (defun count-words (start end)
    "Print number of words in the region."
    (interactive "r")
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (count-matches "\\sw+"))))
