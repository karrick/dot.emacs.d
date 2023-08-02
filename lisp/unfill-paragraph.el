;;; unfill-paragraph -- The opposite of fill-paragraph by Stefan Monnier <foo at acm.org>.

;;; Commentary:

;;; Code:

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (or (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
		;; This would override `fill-column' if it's an integer.
		(emacs-lisp-docstring-fill-column t))
	(fill-paragraph nil region)))

(provide 'unfill-paragraph)

;;; unfill-paragraph.el ends here
