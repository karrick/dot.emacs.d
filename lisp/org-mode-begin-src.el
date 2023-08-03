;;; org-mode-begin-src -- Insert an org-mode source block.

;;; Commentary:

;;; Code:

(require 'empty-string)

(defun org-mode-begin-src (language)
  "Insert an 'org-mode' source block using LANGUAGE."
  (interactive "sLanguage: ")
  (if (empty-string-p language)
	  (insert (concat "#+BEGIN_SRC\n\n#+END_SRC\n"))
	(insert (concat "#+BEGIN_SRC " language "\n\n#+END_SRC\n")))
  (previous-line 2))

(provide 'org-mode-begin-src)

;;; org-mode-begin-src.el ends here
