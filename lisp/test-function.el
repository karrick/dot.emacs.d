;;; test-function -- test function over a list of inputs and expected outputs

;;; Commentary:

;;; Code:

(defun test-function (function cases)
  "Test FUNCTION over the provided CASES."
  (dolist (case cases)
	(let* ((input (car case))
		   (got (funcall function input))
		   (want (cdr case)))
	  (unless (equal got want)
		(message "TEST FAIL: (%s %s) (GOT: %s; WANT: %s)" (symbol-name function) input got want)))))

(provide 'test-function)

;;; test-function.el ends here
