;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs lisp load paths
;;

(add-to-list 'load-path (expand-file-name "~/emacs/org-mode/lisp"))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/w3m")
(add-to-list 'load-path (expand-file-name "~/emacs/"))
(add-to-list 'load-path (expand-file-name "~/emacs/auto-install"))
(add-to-list 'load-path (expand-file-name "~/emacs/tuareg-mode"))
(add-to-list 'load-path "~/emacs/haskell-mode")
(add-to-list 'load-path (expand-file-name "~/emacs/yasnippet"))
(add-to-list 'load-path "~/emacs/clojure-mode")
(add-to-list 'load-path "~/emacs/swank-clojure")
(add-to-list 'load-path "~/emacs/slime")
(add-to-list 'load-path (expand-file-name "~/emacs/color-themes"))
(add-to-list 'load-path (expand-file-name "~/emacs/color-themes/solarized"))
(add-to-list 'load-path (expand-file-name "~/emacs/anything"))
(add-to-list 'load-path (expand-file-name "~/emacs/magit"))
(add-to-list 'load-path (expand-file-name "~/emacs/minor-modes"))
(add-to-list 'load-path (expand-file-name "~/emacs/misc"))
(add-to-list 'load-path (expand-file-name "~/emacs/nxhtml"))
(add-to-list 'load-path (expand-file-name "~/emacs/mmm-mode"))
(add-to-list 'load-path (expand-file-name "~/emacs/nav"))

(defconfig nav
  (require 'nav))

(defconfig sentence-highlight-mode
  (require 'sentence-highlight))

;;;;;; Haskell mode
(defconfig haskell-mode
  (require 'haskell-mode)
  (require 'inf-haskell)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  
  (setf haskell-program-name "/Library/Frameworks/GHC.framework/Versions/Current/usr/bin/ghci")
  (setq auto-mode-alist (cons '("\\.hs$" . haskell-mode) auto-mode-alist)))

;;;; Salesforce .cls files
(defconfig salesforce-config
  ;;  (add-hook 'java-mode-hook 'my-java-mode-hook)
  (setq auto-mode-alist (cons '("\\.translation$" . xml-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.label$" . xml-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.object$" . xml-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.trigger$" . java-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.cls$" . java-mode) auto-mode-alist)))

;;;; w3m config - requires the w3m browser
(defconfig w3m-config
  (require 'w3m-load))


;;;; nxhtml mode - used for jsp mode
(defconfig nxhtml-mode-config
  (load (expand-file-name "~/emacs/nxhtml/autostart.el")))

;;;; mmm mode - used to get a custom jsp "mode"
(defconfig mmm-mode-config
  (require 'mmm-mode)
  (mmm-add-group
   'fancy-html
   '(
     (html-php-tagged
      :submode php-mode
      :face mmm-code-submode-face
      :front "<[?]php"
      :back "[?]>")
     (html-css-attribute
      :submode css-mode
      :face mmm-declaration-submode-face
      :front "styleNO=\""
      :back "\"")
     (jsp-code
      :submode java
      :match-face (("<%!" . mmm-declaration-submode-face)
		   ("<%=" . mmm-output-submode-face)
		   ("<%"  . mmm-code-submode-face))
      :front "<%[!=]?"
      :back "%>"
      :insert ((?% jsp-code nil @ "<%" @ " " _ " " @ "%>" @)
	       (?! jsp-declaration nil @ "<%!" @ " " _ " " @ "%>" @)
	       (?= jsp-expression nil @ "<%=" @ " " _ " " @ "%>" @))
      )
     (jsp-directive
      :submode text-mode
      :face mmm-special-submode-face
      :front "<%@"
      :back "%>"
      :insert ((?@ jsp-directive nil @ "<%@" @ " " _ " " @ "%>" @))
      )
     )))

;;;;; Tramp config
(defconfig tramp
  (setq tramp-default-method "ssh")
  (setq tramp-default-user "rcornel")
  (require 'tramp))

;;;;;; yassnippet 
(defconfig yasnippet
  (require 'yasnippet)
  (yas/initialize)
  (yas/load-directory "~/emacs/yasnippet/snippets"))


;;;;;;; common lisp slime config
(defconfig cl-config
  (defvar package-activated-list nil)
  (require 'slime)

  (slime-setup '(slime-repl) )
					;  (slime-setup '(slime-fancy slime-asdf))
  (setq scheme-program-name "/opt/mit-scheme/bin/scheme")
  (setq inferior-lisp-program "/usr/local/bin/sbcl"))

;;;;;;; Linux path config
(defconfig linux-clojure-config
  (setq swank-clojure-binary "/home/rcornel/bin/clojure")
  (setq swank-clojure-jar-path "/home/rcornel/src/clojure/clojure.jar")
  (require 'clojure-mode)
  (require 'swank-clojure-autoload)
  (swank-clojure-config
   (setq swank-clojure-jar-path "/home/rcornel/clojure/clojure.jar")
   (setq swank-clojure-extra-classpaths 
	 (list "/home/rcornel/src/clojure-contrib/target/clojure-contrib-1.2.0-SNAPSHOT.jar")))
  (require 'swank-clojure)
  (add-to-list 'slime-lisp-implementations '(sbcl ("/usr/bin/sbcl"))))


;;;;;;;; clojure slime config
(defconfig clojure-config
  (setq swank-clojure-binary "~/bin/clojure")
  (setq swank-clojure-jar-path "/Users/reuben/javaBin/clojure/clojure/clojure.jar")
  (require 'clojure-mode)
  (require 'swank-clojure-autoload)
  (swank-clojure-config
   (setq swank-clojure-jar-path "/Users/reuben/javaBin/clojure/clojure/clojure.jar")
   (setq swank-clojure-extra-classpaths 
	 (list "/Users/reuben/javaBin/clojure/clojure-contrib/clojure-contrib.jar")))
  (require 'swank-clojure)
  (add-to-list 'slime-lisp-implementations '(sbcl ("/usr/bin/local/sbcl"))))


(defconfig org-mode-config
  (require 'org-install)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-hide-leading-stars 't)
  (setq org-log-done 'time)

  (setq org-todo-keywords '((sequence "TODO" "STARTED" "WAITING" "|" "DONE")
                            (sequence "NOTPICKEDUP" "|" "PICKEDUP")))

  (setq org-modules '(org-bbdb 
                      org-contacts
                      org-gnus
                      org-info
                      org-jsinfo
                      org-habit
                      org-irc
                      org-mouse
                      org-annotate-file
                      org-eval
                      org-expiry
                      org-interactive-query
                      org-man
		      org-latex
                      org-panel
                      org-screen
                      org-toc))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
       
  (setq org-directory (expand-file-name "~/Dropbox"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-default-journal-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
	   "* TODO %? \n %i\n %a")
	  ("r" "Lookup Entry in region" entry (file+headline org-default-notes-file "Lookup")
	   "* %i :LOOKUP:\n")
	  ("l" "Lookup Entry" entry (file+headline org-default-notes-file "Lookup")
	   "* %?  :LOOKUP:\n %i \n")
	  ("j" "Journal" entry (file+datetree org-default-journal-file)
	   "* %? \nEntered on %U\n %i\n  %a"))) )
;; (setq remember-annotation-functions '(org-remember-annotation))
;; (setq remember-handler-functions '(org-remember-handler))
;; (eval-after-load 'remember
;;   '(add-hook 'remember-mode-hook 'org-remember-apply-template))

(defconfig linux-org-mode-config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-hide-leading-stars 't)
  (setq org-log-done 'time)

  (setq org-todo-keywords '((sequence "TODO" "STARTED" "WAITING" "|" "DONE")
                            (sequence "NOTPICKEDUP" "|" "PICKEDUP")))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  (setq org-directory (expand-file-name "~/Dropbox"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-default-journal-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
	   "* TODO %? \n %i\n %a")
	  ("r" "Lookup Entry in region" entry (file+headline org-default-notes-file "Lookup")
	   "* %i :LOOKUP:\n")
	  ("l" "Lookup Entry" entry (file+headline org-default-notes-file "Lookup")
	   "* %?  :LOOKUP:\n %i \n")
	  ("j" "Journal" entry (file+datetree org-default-journal-file)
	   "* %? \nEntered on %U\n %i\n  %a"))) )


(defconfig auto-install
  (require 'auto-install)
  (setq auto-install-directory "~/emacs/auto-install/")
  (setq auto-install-save-confirm nil))


(defconfig twit
  (require 'twit))

(defconfig linkd
  (require 'linkd))


(defconfig color-theme
  (require 'color-theme)
  (color-theme-initialize)
  (require 'color-theme-solarized))

(defconfig itunes-config
  (require 'osx-itunes))

(defconfig synonym-support
  (require 'synonyms)
  (setq synonyms-file "~/emacs/synonyms/mthesaur.txt")
  (setq synonyms-cache-file "~/emacs/synonyms/thesaurus.cache.txt"))

(defconfig ocaml-support
  (require 'tuareg)
  (setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
  (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
  (autoload 'camldebug "camldebug" "Run the Caml debugger" t))

(defconfig c-mode-config
  (setq basic-c-offset 8)
  (add-hook 'c-mode-hook 'my-c-mode-hook))


(defconfig ruby-mode
  (autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
  (setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))
  (setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
  (require 'inf-ruby)
  (setq ruby-program-name "/usr/local/bin/irb --inf-ruby-mode"))


(defconfig ido-mode
  (ido-mode 1)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t))

(defconfig htmlize
  (require 'htmlize))

(defconfig org-publish-config
  (require 'org-publish)

  (setq org-publish-project-alist
	'(
	  ("org-notes"
	   :base-directory "~/org/"
	   :base-extension "org"
	   :publishing-directory "/Users/reuben/webpage/org-notes/" ;"/rfcornel@tty.freeshell.org:~/html/";/Users/reuben/Everything/EverythingEmacs/orgTutorial/public_html/"
	   :recursive t
	   :style     "<link rel=\"stylesheet\" type=\"text/css\" href=\"orgstyle.css\" />"	 
	   :publishing-function org-publish-org-to-html
	   :auto-index t
	   :headline-levels 4             ; Just the default for this project.
	   :auto-preamble t
	   :index-filename "sitemap.org"  ; ... call it sitemap.org ...
	   :index-title "Sitemap"         ; ... with title 'Sitemap'.
	   )
	  
	  ("org-css"
	   :base-directory "~/org/css/"
	   :base-extension "css"
	   :publishing-directory "/Users/reuben/webpage/org-notes/" ;"/rfcornel@tty.freeshell.org:~/html/";/Users/reuben/Everything/EverythingEmacs/orgTutorial/public_html/"
					;	 :publishing-directory "/Users/reuben/Everything/EverythingEmacs/orgTutorial/public_html/"
	   :recursive t
	   :publishing-function org-publish-attachment
	   )

	  ("org-js"
	   :base-directory "~/org/js/"
	   :base-extension "js"
	   :publishing-directory "/Users/reuben/webpage/org-notes/" ;/"/rfcornel@tty.freeshell.org:~/html/";/Users/reuben/Everything/EverythingEmacs/orgTutorial/public_html/"
					;	 :publishing-directory "/Users/reuben/Everything/EverythingEmacs/orgTutorial/public_html/"
	   :recursive t
	   :publishing-function org-publish-attachment
	   )

	  ("org-source"
	   :base-directory "~/org/src/"
	   :base-extension "java"
	   :publishing-directory "/Users/reuben/webpage/org-notes/src" ;"/rfcornel@tty.freeshell.org:~/html/source/";/Users/reuben/Everything/EverythingEmacs/orgTutorial/public_html/"
	   :recursive t
	   :publishing-function org-publish-attachment)

	  
	  ("org" :components ("org-notes" "org-css" "org-js" ))))) ;"org-source"

;; Article file support
(defconfig article-file-support
  (setq auto-mode-alist (cons '("\\.article$" . html-mode) auto-mode-alist)))

;; Emacs 23 font configuration.
(defconfig emacs-graphical-font
  (set-frame-font "Inconsolata-11"))

(defconfig emacs-graphical-font-windows
  (set-frame-font "Inconsolata-13"))

(defconfig emacs-graphical-font-linux
  (set-frame-font "Inconsolata-11"))


(defconfig aquamacs-config
  (tabbar-mode -1)
  (scroll-bar-mode -1))

(defconfig thin-cursor-config
  (blink-cursor-mode 1)
  (require 'bar-cursor)
  (bar-cursor-mode 1))

(defconfig carbon-emacs-22-font-config
  (set-default-font "-apple-inconsolata-medium-r-normal--16-0-72-72-m-0-iso10646-1"))

(defconfig anything-mode
  (load "anything")
  (load "anything-config.el")
  
  (defun my-anything ()
    (interactive)
    (anything-other-buffer
     '(anything-c-source-buffers
       anything-c-source-file-name-history
       anything-c-source-info-pages
       anything-c-source-info-elisp
       anything-c-source-man-pages
       anything-c-source-locate
       anything-c-source-imenu
       anything-c-source-emacs-commands)
     " *my-anything*")))

(defconfig magit
  (require 'magit))

(defconfig select-enable-clipboard
  (setq x-select-enable-clipboard t))

(defconfig auto-save-config
  (require 'real-auto-save)
  (add-hook 'org-mode-hook 'turn-on-real-auto-save)
  (add-hook 'text-mode-hook 'turn-on-real-auto-save))

(defconfig markdown-mode
  (autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)
  (setq auto-mode-alist
        (cons '("\\.md" . markdown-mode) auto-mode-alist)))

(defconfig text-mode-config
  (fringe-mode -1)
  (add-hook 'text-mode-hook (lambda() (setq line-spacing 10))))

