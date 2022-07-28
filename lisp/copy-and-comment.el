;;; copy-and-comment -- copies the selected region and pastes a commented-out version of it above point.

;;; Commentary:

;;; Code:

(defun copy-and-comment (beg end)
  (interactive "*r")
  (if mark-active
      (save-excursion
	(copy-region-as-kill beg end)
	(yank)
	(comment-region beg end))
    (message "cannot copy-and-comment without region selected")))

(provide 'copy-and-comment)

;;; copy-and-comment.el ends here
