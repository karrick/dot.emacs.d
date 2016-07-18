;;; find-first --- returns first element of list that satisfies a predicate

;;; Commentary:

;;; Code:

(defun find-first (list predicate)
  "Return first item of LIST that satisfies PREDICATE.
Returns nil if predicate is nil for all items in list."
  (catch 'break
    (dolist (item list)
      (let ((result (funcall predicate item)))
        (if result
            (throw 'break result))))))

(provide 'find-first)

;;; find-first.el ends here
