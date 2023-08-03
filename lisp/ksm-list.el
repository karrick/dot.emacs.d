;;; ksm-list --- provides a few list functions

;;; Commentary:

;;; Code:

(defun ksm/drop (n list)
  "Drop the first N items from LIST and return the rest."
  (if (and (> n 0) list)
	  (ksm/drop (1- n) (cdr list))
	list))

(defun ksm/take (n list)
  "Return empty list when N is 0; otherwise return the first N items from LIST."
  (if (and (> n 0) list)
	  (cons (car list) (ksm/take (1- n) (cdr list)))))

(defun ksm/list-insert-at-index (element list i)
  "Insert ELEMENT into LIST at (zero-based) index I."
  (let ((left (min (length list) (max 0 i))))
	(append (ksm/take left list)
			(cons element (ksm/drop left list)))))

(defun ksm/list-remove-at-index (i list)
  "Remove the element at (zero-based) index I from LIST."
  (append
   (ksm/take i list)
   (ksm/drop (1+ i) list)))

(provide 'ksm-list)

;;; ksm-list.el ends here
