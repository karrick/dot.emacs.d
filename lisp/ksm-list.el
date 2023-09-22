;;; ksm-list --- provides a few list functions

;;; Commentary:

;;; Code:

(defmacro ksm/list-append-modify (item place)
  "Return modified list after appending ITEM to tail of PLACE."
  `(setq ,place (append ,place (list ,item))))

(defun ksm/list-append (item place)
  "Return new list with ITEM appended to tail of PLACE."
  (append place (list item)))

(defun ksm/list-drop (n place)
  "Return new list resulting from dropping the first N items from PLACE."
  (if (and (> n 0) place)
	  (ksm/list-drop (1- n) (cdr place))
	place))

(defun ksm/list-insert-at-index (item place i)
  "Return new list with ITEM inserted into PLACE at (zero-based) index I."
  (let ((left (min (length place) (max 0 i))))
	(append (ksm/list-take left place)
			(cons item (ksm/list-drop left place)))))

(defun ksm/list-remove-at-index (i place)
  "Return new list with item at (zero-based) index I removed from PLACE."
  (append
   (ksm/list-take i place)
   (ksm/list-drop (1+ i) place)))

(defmacro ksm/list-prepend-modify (item place)
  "Return modified list after prepending ITEM to head of PLACE."
  `(setq ,place (cons ,item ,place)))

(defun ksm/list-prepend (item place)
  "Return new list after prepending ITEM to head of PLACE."
  (cons item place))

(defun ksm/list-take (n place)
  "Return new list consisting of at most the first N items from PLACE."
  (if (and (> n 0) place)
	  (cons (car place) (ksm/list-take (1- n) (cdr place)))))

(provide 'ksm-list)

;;; ksm-list.el ends here
