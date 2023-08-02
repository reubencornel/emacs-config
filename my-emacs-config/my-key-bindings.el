;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Function key settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; My Key bindings


(global-unset-key [(f1)])
(global-set-key [(f1)] 'next-multiframe-window)

(global-unset-key [(f2)])
(defun open-daily-tasks()
  (interactive)
  (org-agenda "" "d"))
(global-set-key [(f2)] 'open-daily-tasks)

(global-unset-key [(f3)])
(defun open-inbox-today-entries()
  (interactive)
  (org-agenda "" "t"))
(global-set-key [(f3)] 'open-inbox-today-entries)

(global-unset-key [(f4)])
(defun open-inbox()
  (interactive)
  (org-agenda "" "i"))
(global-set-key [(f4)] 'open-inbox)

(global-unset-key [(f5)])
(defun open-review()
  (interactive)
  (org-agenda "" "r"))
(global-set-key [(f5)] 'open-review)

(global-unset-key [(f6)])
(defun open-working-memory()
  (interactive)
  (find-file "~/Dropbox/wm.md")
  (delete-blank-lines)
  (goto-char (point-max))
  (insert "\n")
  (insert-date-time)
  (insert "\n"))
(global-set-key [(f6)] 'open-working-memory)

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
                  (let ((old-face-attribute (face-attribute 'default :height)))
                    (set-face-attribute 'default nil :height (+ old-face-attribute 10)))))

(global-set-key (kbd "s--")
                (lambda ()
                  (interactive)
                  (let ((old-face-attribute (face-attribute 'default :height)))
                    (set-face-attribute 'default nil :height (- old-face-attribute 10)))))


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

    
(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key "\C-x!" 'insert-uuid)
