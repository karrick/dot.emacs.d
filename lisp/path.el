;;; path -- modifies exec-path and environment PATH variable for child prcesses

;;; Commentary:

;;; Code:

(defun path-prepend (elem)
  "Prepend ELEM to the 'exec-path' and PATH environment variable."
  (interactive "DPrepend what directory to PATH: ")
  (let ((path (expand-file-name elem)))
    (when (string-match "/\\'" path)
      (setq path (concat (replace-match "" nil nil path))))
    (when (file-accessible-directory-p path)
      (add-to-list 'exec-path path)
      (setenv "PATH" (concat path ":" (getenv "PATH")))
      (message "Prepending %s to PATH" path))))

(defun path-append (elem)
  "Append ELEM to the 'exec-path' and PATH environment variable."
  (interactive "DAppend what directory to PATH: ")
  (let ((path (expand-file-name elem)))
    (when (string-match "/\\'" path)
      (setq path (concat (replace-match "" nil nil path))))
    (when (file-accessible-directory-p path)
      (add-to-list 'exec-path path)
      (setenv "PATH" (concat (getenv "PATH") ":" path))
      (message "Appending %s to PATH" path))))

(provide 'path)

;;; path.el ends here
