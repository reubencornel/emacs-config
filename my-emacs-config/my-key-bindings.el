;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Function key settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; My Key bindings


(global-unset-key [(f1)])
(global-set-key [(f1)] 'ace-window)


(defun capture-log()
  (interactive)
  (org-capture nil "g"))
(global-unset-key [(f7)])
(global-set-key [(f7)] 'capture-log)

(global-unset-key [(f10)])
(global-set-key [(f10)] 'kmacro-start-macro)

(global-unset-key [(f11)])
(global-set-key [(f11)] 'kmacro-end-macro)

(global-unset-key [(f12)])
(global-set-key [(f12)] 'apply-macro-to-region-lines)


(global-set-key (kbd "s-=")
                (lambda ()
                  (interactive)
                  (set-face-attribute 'default nil :height (+ (face-attribute 'default :height) 10))
		  (set-face-attribute 'variable-pitch nil :height (+ (face-attribute 'variable-pitch :height) 10))
		  (set-face-attribute 'fixed-pitch nil :height (+ (face-attribute 'fixed-pitch :height) 10))))
		    

(global-set-key (kbd "s--")
                (lambda ()
                  (interactive)
                  (set-face-attribute 'default nil :height (- (face-attribute 'default :height) 10))
		  (set-face-attribute 'variable-pitch nil :height (- (face-attribute 'variable-pitch :height) 10))
		  (set-face-attribute 'fixed-pitch nil :height (- (face-attribute 'fixed-pitch :height) 10))))

(global-unset-key "\M-g")
(global-set-key "\M-g" 'goto-line)
;;;;;;;;;;;;;;;
;; org-mode key  bindings
;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c a") 'org-agenda)           ;; (5)
(global-set-key (kbd "C-c r") 'org-capture)
(global-set-key "\C-cl" 'org-store-link)

(global-set-key "%" 'match-paren)
    
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key "\C-x!" 'insert-uuid)
