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
  (let ((last-digit (substring day
			       (- (length day) 1)
			       (length day))))
    (cond ((equal last-digit "1") "st")
	  ((equal last-digit "2") "nd")
	  ((equal last-digit "3") "rd")
	  (t "th"))))

(defun indent-line()
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
  (let ((cur-time (current-time)))
    (let ((month (format-time-string "%B " cur-time))
	  (day (format-time-string "%d" cur-time))
	  (year (format-time-string "%Y" cur-time)))
      (concat month
	      day
	      (get-number-str day) 
	      ", "
	      year))))
	

(defun insert-date()
  (interactive)
  (insert (get-date)))

(defun init-assert()
  (interactive)
  (when (= (point-max) 1) ;; Insert these lines if and only if we have
			  ;; a new buffer
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
  (init-assert))

(defun has-disk-file-p(buffer)
  "Checks if a file has been saved."
  (if (bufferp buffer)
	(not (null (buffer-file-name buffer)))
    (print "Function requires a buffer")))


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
