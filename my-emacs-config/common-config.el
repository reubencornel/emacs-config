;; Disabling the annoying bell
(setq ring-bell-function 'ignore)

;; Basic config
(setq user-full-name "Reuben Francis Cornel")
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

(setq tabs-indent-mode nil)

(setq scroll-step 1)

;; Turn on transient mark mode
(transient-mark-mode t)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;;Toggle off scroll bar
(toggle-scroll-bar -1)
(scroll-bar-mode 0)

;;Toggle off the tool bar
(tool-bar-mode -1)

;; Add /usr/local/bin to the emacs shell path
(add-to-list 'exec-path "/usr/local/bin")

;; Turn off fringe
;;(fringe-mode -1)


;; Prevent the start screen from showing up
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

(setq-default line-spacing 5)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq-default cursor-type 'box)


;; Make delete work as it work on windows o
(delete-selection-mode 1)

(setq eshell-aliases-file "~/Dropbox/emacs.alias")


(add-hook 'text-mode-hook
	  (lambda ()
	    (variable-pitch-mode 1)))

(add-hook 'after-save-hook
	  'reuben/set-ids)
(add-hook 'after-save-hook
 	  'reuben/update-areas-string)
(add-hook 'after-save-hook
 	  'reuben/update-projects-hook)

(set-face-attribute 'default nil :family "Jetbrains Mono 15")
(set-face-attribute 'variable-pitch nil :family "DejaVu Sans Mono")
(set-face-attribute 'fixed-pitch nil :family "Jetbrains Mono 15")

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq make-backup-files nil)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq create-lockfiles nil)


;; --------------- electric pair mode ---------------
(setq electric-pair-preserve-balance nil)
(setq create-lockfiles nil)

(add-hook 'prog-mode-hook (lambda ()
			    (setq display-line-numbers 'relative)))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defun center-buffer-text()
  "This function centers the text in a buffer. Use this for writing."
  
  (interactive)
  (visual-fill-column-mode)
  (auto-fill-mode)
  (setq visual-fill-column-center-text t))


(defvar-local hidden-mode-line-mode nil)
  (define-minor-mode hidden-mode-line-mode
    "Minor mode to hide the mode-line in the current buffer."
    :init-value nil
    :global t
    :variable hidden-mode-line-mode
    :group 'editing-basics
    (if hidden-mode-line-mode
	(setq hide-mode-line mode-line-format
              mode-line-format nil)
      (setq mode-line-format hide-mode-line
            hide-mode-line nil))
    (force-mode-line-update)
    ;; Apparently force-mode-line-update is not always enough to
    ;; redisplay the mode-line
    (redraw-display)
    (when (and (called-interactively-p 'interactive)
               hidden-mode-line-mode)
      (run-with-idle-timer
       0 nil 'message
       (concat "Hidden Mode Line Mode enabled.  "
               "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(show-paren-mode)
(setq-default blink-cursor-blinks -1)
(setq-default blink-cursor-interval .6)
(setq-default blink-cursor-delay .6)
(setq-default cursor-type '(bar . 3))
(setq-default cursor-in-non-selected-windows 'box)
(setq default-frame-alist '((font . "Jetbrains Mono 15")))
