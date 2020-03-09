;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Function key settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; My Key bindings

(global-unset-key [(f1)])
(global-set-key [(f1)] 'next-multiframe-window)

(global-unset-key [(f2)])
(global-set-key [(f2)] 'previous-multiframe-window)

(global-unset-key [(f3)])
(global-set-key [(f3)] 'enlarge-window-horizontally)

(global-unset-key [(f4)])
(global-set-key [(f4)] 'shrink-window-horizontally)

(global-unset-key [(f5)])
(global-set-key [(f5)] 'enlarge-window)

(global-unset-key [(f6)])
(global-set-key [(f6)] 'helm-locate)

(global-unset-key [(f7)])
(global-set-key [(f7)] 'open-slip-box-and-add-note)

(global-unset-key [(f10)])
(global-set-key [(f10)] 'kmacro-start-macro)

(global-unset-key [(f11)])
(global-set-key [(f11)] 'kmacro-end-macro)

(global-unset-key [(f12)])
(global-set-key [(f12)] 'apply-macro-to-region-lines)

;; (global-unset-key [(control f2)])
;; (global-set-key [(control f2)] 'twit-post)


(global-unset-key "\M-g")
(global-set-key "\M-g" 'goto-line)
;;;;;;;;;;;;;;;
;; org-mode key  bindings
;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c a") 'org-agenda)           ;; (5)
(global-set-key (kbd "C-c r") 'org-capture)
(global-set-key "\C-cl" 'org-store-link)

(global-set-key "%" 'match-paren)

(global-unset-key [(control x) (control o)])
;;* key binding
(global-set-key [(control x) (control o)] 'grep-ffap)
;;* end

(if (package-installed-p 'helm)
    (let ()
      (global-set-key (kbd "M-x") 'helm-M-x)
      (global-set-key (kbd "C-x C-f") 'helm-find-files)
      (global-set-key (kbd "C-x b") 'helm-mini)
      (global-unset-key (kbd "C-x c"))   
      t)
  nil)
      
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key "\C-x!" 'insert-uuid)
