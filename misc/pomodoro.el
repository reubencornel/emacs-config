;;; -*- lexical-binding: t -*-
;; Author: Reuben Cornel
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:

;;; Acknowledgements
;; I learned a lot by looking at Victor Deryagin's pomodoro.el, you can find it here
;; https://github.com/vderyagin/pomodoro.el

;;; Code:

;;TODO
;; Use hooks instead of functions.
;; Have to be able to define a set of tasks
;; Have the ability to look up tasks from org-mode
;; Ability to log pomodoros to an org mode file.

(eval-when-compile
  (require 'cl))

(defvar pomodoro-buffer-name "*pomodoro*")
(defvar pomodoro-buffer nil)
(defvar pomodoro-state "" "Variable that keeps track of the state of the program") ;; LB:SB:TK
(defvar pomodoro-task nil "Name of the task that is currently being executed")
(defvar pomodoro-timer nil)
(defvar pomodoro-mode-line-string "")
(defvar pomodoro-max-number-of-small-breaks 3)

(defvar pomodoro-size-of-tick 60 "Number seconds after which the timer is set off in seconds")
(defvar pomodoro-max-size 25 "Max time for the pomodoro in minutes")
(defvar pomodoro-short-break-size 5)
(defvar pomodoro-long-break-size 15)
(defvar pomodoro-time-remaining 0)
(defvar pomodoro-number-of-small-breaks 0)

(defvar pomodoro-custom-on-start-functions '() "Functions that are executed on start of the pomodoro")
(defvar pomodoro-custom-on-complete-functions '() "Functions that are executed on complete of the timer")
(defvar pomodoro-custom-on-tick-functions '() "Functions that are executed on every tick of the timer")
(defvar pomodoro-custom-on-cancel-functions '() "Functions that are executed when the timer is cancelled")
(defvar pomodoro-use-notify nil)


;; If notify.el has been installed use it.
(when (require 'notify nil 'noerror)
  (setq pomodoro-use-notify t))

(defun map-functions(function-list)
  "Function to map across a list of functions."
  (mapcar #'(lambda(function)
              (funcall function))
          function-list))

(defmacro pomodoro-timer-template(max-size start-functions tick-functions complete-functions)
  "Macro for a pomodoro timer, checks if a timer is active, if not then sets a timer."
  `(if (null pomodoro-timer)
       (progn
         (map-functions ,start-functions)
         (setq pomodoro-timer
               (run-at-time 0
                            pomodoro-size-of-tick
                            (pomodoro-tick ,max-size
                                           ,tick-functions
                                           (cons #'generic-cleanup-function
                                                   ,complete-functions)))))
     (pomodoro-log-to-buffer "There is a timer already running")))

(defun pomodoro-tick(time tick-functions complete-functions)
  "Function that provides the ticking..."
  (lexical-let ((pomodoro-minute 0)
                (max-time time)
                (on-complete-functions complete-functions)
                (on-tick-functions tick-functions))
    #'(lambda ()
        (incf pomodoro-minute)
        (setq pomodoro-time-remaining (- max-time pomodoro-minute))
        (map-functions on-tick-functions)
        (when (> pomodoro-minute max-time)
          (map-functions on-complete-functions)))))

(defun pomodoro-message (msg)
  "Function to write to the pomodoro buffer, just a wrapper, so that I don't have to write lambdas everywhere."
  (lexical-let ((message msg))
    #'(lambda()
        (pomodoro-log-to-buffer message))))

(defun generic-cleanup-function()
  "Called when a timer is up"
  (setq pomodoro-task "")
  (cancel-timer pomodoro-timer)
  (setq pomodoro-timer nil))

(defun pomodoro-start(task)
  (interactive "MTask Name:")
  (setq pomodoro-task task)
  (pomodoro))

(defun pomodoro()
  "Function that starts a pomodoro"
  (setq pomodoro-state "TK")
  (pomodoro-timer-template pomodoro-max-size
                           (append (list (update-mode-line)
                                         (pomodoro-message (concat "Starting pomodoro for: " pomodoro-task)))
                                 pomodoro-custom-on-start-functions)
                           (cons (update-mode-line)
                                 pomodoro-custom-on-tick-functions)
                           (append (list (update-mode-line)
                                         (pomodoro-message (concat "Completed Task:" pomodoro-task)))
                                   pomodoro-custom-on-complete-functions)))

(defun pomodoro-break()
  "Function that decides how you break"
  (interactive)
  (if (< pomodoro-number-of-small-breaks pomodoro-max-number-of-small-breaks)
      (progn 
        (incf pomodoro-number-of-small-breaks)
        (pomodoro-short-break))
    (progn
      (setq pomodoro-number-of-small-breaks 0)
      (pomodoro-long-break))))

(defun pomodoro-short-break()
  "Function that controls a short break"
  (interactive)
  (setq pomodoro-task "Short Break")
  (setq pomodoro-state "SB")
  (pomodoro-timer-template pomodoro-short-break-size
                           (append (list (update-mode-line)
                                         (pomodoro-message "Starting short break"))
                                   pomodoro-custom-on-start-functions)
                           (cons (update-mode-line)
                                 pomodoro-custom-on-tick-functions)
                           (append (list #'(lambda()(pomodoro-log-to-buffer "Completed Short Break"))
                                         (update-mode-line))
                                   pomodoro-custom-on-complete-functions)))

(defun pomodoro-long-break()
  "Function that controls a long break"
  (interactive)
  (setq pomodoro-state "LB")
  (setq pomodoro-task "Long Break")
  (pomodoro-timer-template pomodoro-long-break-size
                           (append (list (update-mode-line)
                                         (pomodoro-message "Starting long break"))
                                   pomodoro-custom-on-start-functions)
                           (cons (update-mode-line)
                                 pomodoro-custom-on-tick-functions)
                           (append (list (update-mode-line)
                                         #'(lambda()(pomodoro-log-to-buffer "Completed Long Break")))
                                   pomodoro-custom-on-complete-functions)))

(defun update-mode-line()
  "Returns a function that updates the value of the mode line"
  (lambda()
    (if (null pomodoro-timer)
        (progn
          (delq 'pomodoro-mode-line-string global-mode-string)
          (setq pomodoro-mode-line-string ""))
      (progn
        (add-to-list 'global-mode-string 'pomodoro-mode-line-string 'append)
        (setq pomodoro-mode-line-string
              (format " %s %02d:%02d"
                      pomodoro-state
                      00
                      pomodoro-time-remaining))))
      (force-mode-line-update)))

(defun pomodoro-cancel()
  "Cancels an existing pomodoro"
  (interactive)
  (map-functions (append pomodoro-custom-on-cancel-functions
                         (list #'generic-cleanup-function
                               (pomodoro-message (concat "Pomodoro cancelled for " pomodoro-task))))))

(defun pomodoro-log-interruption(interruption)
  "This function logs an interruption to the pomodoro buffer"
  (interactive "MDescribe Interruption:")
  (pomodoro-log-to-buffer "Interruption:" interruption))

(defun pomodoro-log-to-buffer(&rest log-message)
  "Logging to a pomodoro buffer, in case we need to audit this stuff later"
  (save-excursion
    (save-current-buffer
      (pomodoro-create-log-buffer)
      (set-buffer pomodoro-buffer)
      (goto-char (point-max))             ;
      (insert "[" (current-time-string) "]: "  (apply 'concat log-message) "\n")
      (if pomodoro-use-notify
        (notify "Pomodoro Timer" log-message)))))

(defun pomodoro-create-log-buffer()
  (setq pomodoro-buffer (get-buffer-create pomodoro-buffer-name)))

(provide 'pomodoro)
