;;; setup-javascript-mode -- customizations for JavaScript programming language

;;; Commentary:

;;; Code:

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; (defvaralias 'js-indent-level 'tab-width)
(set-default 'js2-basic-offset 4)
(set-default 'js2-mirror-mode nil)
(set-default 'js2-mode-escape-quotes nil)

(add-hook 'javascript-mode-hook #'(lambda ()
				    (add-hook 'before-save-hook #'clean-and-indent nil t)
				    (setq indent-tabs-mode t
					  ;; js-indent-level 4
					  )))

(provide 'setup-javascript-mode)

;;; setup-javascript-mode.el ends here
