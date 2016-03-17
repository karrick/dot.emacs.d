;;; setup-js2-mode -- customizations for JavaScript programming language

;;; Code:

(defvaralias 'js-indent-level 'tab-width)
(set-default 'js2-basic-offset 4)
(set-default 'js2-mirror-mode nil)
(set-default 'js2-mode-escape-quotes nil)
(add-hook 'javascript-mode-hook
          #'(lambda ()
              (flyspell-prog-mode)
              (setq fill-column 100
                    indent-tabs-mode t
                    js-indent-level 4)))

(provide 'setup-js2-mode)

;;; setup-js2-mode.el ends here
