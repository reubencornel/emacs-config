
(if (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
  (message "Native compilation is available")
  (message "Native complation is *not* available"))

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (setq comp-deferred-compilation t))

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
;; End of Emacs Soupiness 

(package-initialize)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("non-gnu-elpa" . "https://elpa.nongnu.org/nongnu/")))
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(add-to-list 'load-path "/home/rcornel/emacs/my-emacs-config")
(load "common-config.el")
(load "utility-code.el")
(load "mode-specific-config.el")
(load "use-package-config.el")
(load "my-key-bindings.el")

(setq exec-path (append exec-path
			(split-string (getenv "PATH") ":")))
			
(put 'dired-find-alternate-file 'disabled nil)

(setq org-roam-v2-ack t)

(put 'narrow-to-region 'disabled nil)

(setq inhibit-splash-screen t)
(setq package-check-signature nil)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode t)
(setq-default cursor-type 'box)

(message (emacs-init-time))

