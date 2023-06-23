;;; make-shebang-executable -- Make file corresponding to buffer executable when first two characters are `#!' and not already executable.

;;; Commentary:

;;; Code:

(defun make-shebang-executable ()
  "Make file executable when begin with `#!' and not already executable."
  (when
	  (and
	   (save-excursion
		 (save-restriction
		   (widen)
		   (goto-char (point-min))
		   (save-match-data
			 (looking-at "^#!"))))
	   (not (file-executable-p buffer-file-name)))
	(set-file-modes buffer-file-name
					(logior (file-modes buffer-file-name) #o100))
	(message "Made file executable: %s" buffer-file-name)))

(add-hook 'after-save-hook #'make-shebang-executable)

(provide 'make-shebang-executable)

;;; make-shebang-executable.el ends here
