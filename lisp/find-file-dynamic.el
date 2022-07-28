;;; find-file-dynamic --- find-file-dynamic configuration

;;; Commentary:

;;; Code:

(ido-mode 1)

(defun find-file-dynamic (&optional arg)
  "C-x C-f invokes #'ido-file-file; with ARG, invokes #'find-file-in-repository."
  (interactive "P")
  (if (equal current-prefix-arg nil)
      (ido-find-file)
    (find-file-in-repository)))

(global-set-key (kbd "C-x C-f") #'find-file-dynamic)

(provide 'find-file-dynamic)

;;; find-file-dynamic.el ends here
