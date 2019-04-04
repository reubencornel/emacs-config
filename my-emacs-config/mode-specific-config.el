;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs lisp load paths
;;


(defconfig c-mode-config
  (setq basic-c-offset 8)
  (add-hook 'c-mode-hook 'my-c-mode-hook))

;; Article file support
(defconfig article-file-support
  (setq auto-mode-alist (cons '("\\.article$" . html-mode) auto-mode-alist)))


(defconfig select-enable-clipboard
  (setq x-select-enable-clipboard t))

(defconfig text-mode-config
  (fringe-mode -1))

;; (defconfig magit
;;   (check-and-install-if-absent 'magit)
;;   (require 'magit))

;; (defconfig markdown-mode
;;   (check-and-install-if-absent 'markdown-mode)
;;   (require 'markdown-mode)
;;   (setq auto-mode-alist
;;         (cons '("\\.md" . markdown-mode) auto-mode-alist)))

(defconfig firacode-config
  ;; ;; Fira code specific settings.
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
		 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
		 (36 . ".\\(?:>\\)")
		 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
		 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
		 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
		 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
		 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
		 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
		 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
		 (48 . ".\\(?:x[a-zA-Z]\\)")
		 (58 . ".\\(?:::\\|[:=]\\)")
		 (59 . ".\\(?:;;\\|;\\)")
		 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
		 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
		 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
		 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
		 (91 . ".\\(?:]\\)")
		 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
		 (94 . ".\\(?:=\\)")
		 (119 . ".\\(?:ww\\)")
		 (123 . ".\\(?:-\\)")
		 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
		 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
		 )
	       ))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
			    `([,(cdr char-regexp) 0 font-shape-gstring])))))

(defconfig diary-config
  (setq diary-file "~/Dropbox/emacsdiary.txt")
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files))

(defconfig utf8-config
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  ;; set the default encoding system
  (prefer-coding-system 'utf-8)
  (setq default-file-name-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  ;; backwards compatibility as default-buffer-file-coding-system
  ;; is deprecated in 23.2.
  (if (boundp buffer-file-coding-system)
      (setq buffer-file-coding-system 'utf-8)
    (setq default-buffer-file-coding-system 'utf-8)))

(defconfig nov-mode-config
  (check-and-install-if-absent 'nov)
  (require 'nov)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
			     :height 1.0))
  (add-hook 'nov-mode-hook 'my-nov-font-setup))

;; --------------- Spacemacs themeing ---------------
(defconfig spacemacs-theme
  (check-and-install-if-absent 'spacemacs-theme)

  (if (check-and-install-if-absent 'all-the-icons)
      (all-the-icons-install-fonts))
  
  (require 'spaceline)
  (require 'spaceline-config)
  (require 'spaceline-all-the-icons)
  
  (setq spacemacs-theme-org-height t)
  (setq spacemacs-theme-org-agenda-height t)
  
  (spaceline-all-the-icons-theme)
  (spaceline-spacemacs-theme)

  
  (defun load-spacemacs-dark-theme()
    (interactive)
    (disable-all-themes)
    (load-theme 'spacemacs-dark))
    
  (defun load-spacemacs-light-theme()
    (interactive)
    (disable-all-themes)
    (load-theme 'spacemacs-light)))



