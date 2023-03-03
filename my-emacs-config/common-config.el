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
(fringe-mode -1)


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

(set-face-attribute 'default nil :family "Fira Mono")
(set-face-attribute 'variable-pitch nil :family "DejaVu Sans Mono")
(set-face-attribute 'fixed-pitch nil :family "Fira Mono")

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


;; ----------------------- LINUM MODE ---------------------------
(defvar my-linum-format-string " %-5d")
(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)

(defun my-linum-get-format-string ()
  (let* ((width (+ 1 (length (number-to-string
                         (count-lines (point-min) (point-max))))))
         (format (concat " %-" (number-to-string width) "d")))
    (setq my-linum-format-string format)))

(setq linum-format 'my-linum-format)
(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number) 'face 'linum))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(show-paren-mode)
