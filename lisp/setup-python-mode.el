;;; setup-python-mode -- customizations for Python programming language-info-alist

;;; Code:

(add-hook 'python-mode-hook
	  #'(lambda ()
	      (setq indent-tabs-mode nil
		    tab-width 4)))

(provide 'setup-python-mode)

;;; setup-python-mode.el ends here
