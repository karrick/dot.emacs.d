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

;; From: https://www.emacswiki.org/emacs/TrampMode
(defun find-alternative-file-with-sudo ()
  "Toggle between opening the file with sudo or as unprivileged user."
  (interactive)
  (let ((fname (or buffer-file-name
				   dired-directory)))
	(when fname
	  (if (string-match "^/sudo:root@localhost:" fname)
		  (setq fname (replace-regexp-in-string
					   "^/sudo:root@localhost:" ""
					   fname))
		(setq fname (concat "/sudo:root@localhost:" fname)))
	  (find-alternate-file fname))))

(provide 'find-file-dynamic)

;;; find-file-dynamic.el ends here
