;;; path -- modifies exec-path and environment PATH variable for child processes

;;; Commentary:

;;; Code:

(require 'ksm-list)

(defun path-prepend (elem)
  "Prepend ELEM to variable `exec-path' and PATH environment variable."
  (interactive "DPrepend what directory to PATH: ")
  (let ((path (expand-file-name elem)))
	(when (string-match "/\\'" path)
	  (setq path (concat (replace-match "" nil nil path))))
	(when (and (not (member path exec-path))
			   (file-accessible-directory-p path))
	  (ksm/list-prepend-modify path exec-path)
	  (setenv "PATH" (concat path ":" (getenv "PATH")))
	  (message "Prepending %s to PATH" path))))

(defun path-append (elem)
  "Append ELEM to variable `exec-path' and PATH environment variable."
  (interactive "DAppend what directory to PATH: ")
  (let ((path (expand-file-name elem)))
	(when (string-match "/\\'" path)
	  (setq path (concat (replace-match "" nil nil path))))
	(when (and (not (member path exec-path))
			   (file-accessible-directory-p path))
	  (ksm/list-append-modify path exec-path)
	  (setenv "PATH" (concat (getenv "PATH") ":" path))
	  (message "Appending %s to PATH" path))))

(defun path-concat (a b)
  "Return OS pathname by concatenation of A and B."

  ;; remove slash from end of a
  (when (string-match "/\\'" a)
	(setq a (concat (replace-match "" nil nil a))))

  ;; remove slash from start of b
  (when (string-match "/\\`" b)
	(setq b (concat (replace-match "" nil nil b))))

  (convert-standard-filename (expand-file-name (concat a "/" b))))

(provide 'path)

;;; path.el ends here
