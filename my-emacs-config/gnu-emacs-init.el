(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
			 ("non-gnu-elpa" . "https://elpa.nongnu.org/nongnu/")))
(add-to-list 'load-path "/home/rcornel/emacs/my-emacs-config")


(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

 (load "common-config.el")
 (load "utility-code.el")
 (load "mode-specific-config.el")
 (load "use-package-config.el")
 (load "my-key-bindings.el")

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin" "~/bin" "/home/reuben/.cargo/bin")))

(put 'narrow-to-region 'disabled nil)

(setq inhibit-splash-screen t)
(setq package-check-signature nil)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode t)
(setq-default cursor-type 'box)
