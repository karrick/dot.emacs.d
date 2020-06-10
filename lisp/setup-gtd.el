;;; setup-gtd --- gtd configuration

;;; Commentary:

;;; Code:

(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/main.org"
                         "~/gtd/tickler.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ;; ("p" "Project [projects]" entry
                              ;;  (file+headline "~/gtd/main.org" "Projects")
                              ;;  "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-refile-targets '(("~/gtd/main.org" :maxlevel . 3)
                           ("~/gtd/agendas.org" :level . 1)
                           ("~/gtd/someday.org" :level . 1)
                           ("~/gtd/reference.org" :level . 1)
                           ("~/gtd/tickler.org" :maxlevel . 2)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(provide 'setup-gtd)

;;; setup-gtd.el ends here
