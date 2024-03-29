;;; setup-gtd --- gtd configuration

;;; Commentary:

;;; Code:

;; (setq org-todo-keywords
;;       '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED")))
(setq org-agenda-files '("~/gtd/inbox.org"
			 "~/gtd/projects.org"
			 "~/gtd/tickler.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
			       (file+headline "~/gtd/inbox.org" "Inbox")
			       "* TODO %i%?")
			      ;; ("p" "Project [projects]" entry
			      ;;  (file+headline "~/gtd/projects.org" "Projects")
			      ;;  "* TODO %i%?")
			      ("T" "Tickler" entry
			       (file+headline "~/gtd/tickler.org" "Tickler")
			       "* %i%? \n %U")))

(setq org-refile-targets '(("~/gtd/projects.org" :maxlevel . 3)
			   ("~/gtd/agendas.org" :level . 1)
			   ("~/gtd/inbox.org" :maxlevel . 2)
			   ("~/gtd/references.org" :level . 1)
			   ("~/gtd/someday.org" :level . 1)
			   ("~/gtd/tickler.org" :maxlevel . 2)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(add-hook 'org-mode-hook #'(lambda ()
			     (local-set-key (kbd "C-c l") 'org-store-link)))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(provide 'setup-gtd)

;;; setup-gtd.el ends here
