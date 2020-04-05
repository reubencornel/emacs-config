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

;;Toggle off the tool bar
(tool-bar-mode -1)

;; Add /usr/local/bin to the emacs shell path
(add-to-list 'exec-path "/usr/local/bin")

;; Turn off fringe
(fringe-mode -1)


;; Prevent the start screen from showing up
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

;; Make delete work as it work on windows o
(delete-selection-mode 1)

(setq eshell-aliases-file "~/Dropbox/emacs.alias")


(add-hook 'text-mode-hook
	  (lambda ()
	    (variable-pitch-mode 1)))

(set-face-attribute 'default nil :family "Fira Mono")
(set-face-attribute 'variable-pitch nil :family "Source Sans Pro")
(set-face-attribute 'fixed-pitch nil :family "Fira Mono")
