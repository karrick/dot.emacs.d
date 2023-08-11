;;; setup-org-mode --- customizations for org mode

;;; Commentary:

;;; Code:

(require 'org)
(require 'org-mode-begin-src)

(define-key org-mode-map (kbd "C-c s") #'org-mode-begin-src)

(require 'ol)
(define-key org-mode-map (kbd "C-c l") #'org-store-link)

(setq
 org-clock-mode-line-today 'today
 org-indent-mode t
 org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "PR(p)" "|" "MERGED(m)" "DONE(d)" "CANCELLED(c)" "DELEGATED(g)")))

(provide 'setup-org-mode)

;;; setup-org-mode.el ends here
