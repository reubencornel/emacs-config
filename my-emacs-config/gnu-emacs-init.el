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

;;;; Straight.el init
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;;;; Straight.el init


;; commented for move to straight
;; (package-initialize)
;; (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;;                          ("melpa" . "https://melpa.org/packages/")
;; 			 ("non-gnu-elpa" . "https://elpa.nongnu.org/nongnu/")))
;; (when (not package-archive-contents)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; commented for move to straight

;(require 'use-package)

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

