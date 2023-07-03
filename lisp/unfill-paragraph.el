;;; unfill-paragraph -- The opposite of fill-paragraph by Stefan Monnier <foo at acm.org>.

;;; Commentary:

;;; Code:

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
		;; This would override `fill-column' if it's an integer.
		(emacs-lisp-docstring-fill-column t))
	(fill-paragraph nil region)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

(provide 'unfill-paragraph)

;;; unfill-paragraph.el ends here
