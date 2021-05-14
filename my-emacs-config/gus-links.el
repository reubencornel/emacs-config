(require 'org)

(defcustom gus-work-number-protocol "gus"
  "Protocol for Gus Work Number links"
  :group 'org-gus :type 'string)

(defun org-gus-number-follow(gus-number)
  (if (not (null (string-match-p "^a07" gus-number)))
      (browse-url (concat "https://gus.my.salesforce.com/" gus-number))
    (browse-url (concat "https://gus.my.salesforce.com/apex/ADM_WorkLocator?bugorworknumber=" gus-number))))

(org-link-set-parameters gus-work-number-protocol
                         :follow #'org-gus-number-follow)

(provide 'gus-links)

