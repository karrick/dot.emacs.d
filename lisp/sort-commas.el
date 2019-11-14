;;; sort-commas -- split region text on commas, sort them, then rejoin text

;;; Commentary:

;;; Code:

(defun sort-commas (start end)
  (interactive "*r")
  (if (use-region-p)
      (let ((selection (buffer-substring-no-properties start end)))
        (delete-region start end)
        (insert (string-join (sort (mapcar #'string-trim (split-string selection ",")) #'string<) ",")))
    (message "cannot sort-commas without region selected")))

(provide 'sort-commas)

;;; sort-commas.el ends here
