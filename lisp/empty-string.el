;;; empty-string -- helpers for working with strings that may or not be empty

;;; Commentary:

;;; Code:

(require 'test-function)

(defun empty-string-p (argument)
  "Return non-nil when ARGUMENT is either not a string or an empty string."
  (or (not (stringp argument))
	  (zerop (length argument))))

(test-function #'empty-string-p
			   (list
				'(nil . t)
				'(t . t)
				'("" . t)
				'("a" . nil)))

(defun string-or-nil (argument)
  "Return either the string when ARGUMENT is string; otherwise nil."
  (when (stringp argument) argument))

(test-function #'string-or-nil
			   (list
				'(nil . nil)
				'(t . nil)
				'("" . "")
				'("a" . "a")))

(defun non-empty-string-or-nil (argument)
  "Return the string if ARGUMENT is not empty string; or nil."
  (when (and (stringp argument)
			 (not (zerop (length argument))))
	argument))

(test-function #'non-empty-string-or-nil
			   (list
				'(nil . nil)
				'(t . nil)
				'("" . nil)
				'("a" . "a")))

(provide 'empty-string)

;;; empty-string.el ends here
