;;; setup-org-mode --- customizations for org mode

;;; Commentary:

;;; Code:

(require 'empty-string)
(require 'org)

(defun org-mode-begin-src (language)
  "Insert an 'org-mode' source block using LANGUAGE."
  (interactive "sLanguage: ")
  (if (empty-string-p language)
	  (insert (concat "#+BEGIN_SRC\n\n#+END_SRC\n"))
	(insert (concat "#+BEGIN_SRC " language "\n\n#+END_SRC\n")))
  (previous-line 2))

(define-key org-mode-map (kbd "C-c s") #'org-mode-begin-src)

(require 'ol)
(define-key org-mode-map (kbd "C-c l") #'org-store-link)

(setq
 org-clock-mode-line-today 'today
 org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "PR(p)" "|" "MERGED(m)" "DONE(d)" "CANCELLED(c)" "DELEGATED(g)")))

(provide 'setup-org-mode)

;;; setup-org-mode.el ends here
