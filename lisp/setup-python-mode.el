;;; setup-python-mode -- customizations for Python programming language-info-alist

;;; Code:

(add-hook 'python-mode-hook
          #'(lambda ()
              (flyspell-prog-mode)
              (setq fill-column 100
                    indent-tabs-mode nil
                    tab-width 4)))

(provide 'setup-python-mode)

;;; setup-python-mode.el ends here
