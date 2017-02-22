;;; find-file-dynamic --- find-file-dynamic configuration

;;; Commentary:

;;; Code:

(require 'ido)
(ido-mode 1)

(require 'require-package)

(require-package/with-requirements '(find-file-in-repository)
  (defun find-file-dynamic (&optional arg)
    "C-x C-f invokes #'ido-file-file; with C-u prefix, invokes #'find-file-in-repository."
    (interactive "P")
    (if (equal current-prefix-arg nil)
        (ido-find-file)
      (find-file-in-repository)))

  (global-set-key (kbd "C-x C-f") #'find-file-dynamic))

(provide 'find-file-dynamic)

;;; find-file-dynamic.el ends here
