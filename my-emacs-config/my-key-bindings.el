;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Function key settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; My Key bindings


(global-unset-key (kbd "M-o"))
(global-set-key (kbd "M-o") 'ace-window)

(defun capture-log()
  (interactive)
  (org-capture nil "j"))

(global-unset-key [(f2)])
(global-set-key [(f2)] 'capture-log)

(global-unset-key [(f10)])
(global-set-key [(f10)] 'kmacro-start-macro)

(global-unset-key [(f11)])
(global-set-key [(f11)] 'kmacro-end-macro)

(global-unset-key [(f12)])
(global-set-key [(f12)] 'apply-macro-to-region-lines)

(defun increase-font-size ()
  (interactive)
  (set-face-attribute 'default nil :height (+ (face-attribute 'default :height) 10))
  (set-face-attribute 'variable-pitch nil :height (+ (face-attribute 'variable-pitch :height) 10))
  (set-face-attribute 'fixed-pitch nil :height (+ (face-attribute 'fixed-pitch :height) 10)))

(global-set-key (kbd "s-=") 'increase-font-size)
(defun decrease-font-size ()
  (interactive)
  (set-face-attribute 'default nil :height (- (face-attribute 'default :height) 10))
  (set-face-attribute 'variable-pitch nil :height (- (face-attribute 'variable-pitch :height) 10))
  (set-face-attribute 'fixed-pitch nil :height (- (face-attribute 'fixed-pitch :height) 10)))

(global-set-key (kbd "s--") 'decrease-font-size)

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

(global-unset-key "\M-@")
(global-set-key "\M-@"   'reuben/mark-word)
