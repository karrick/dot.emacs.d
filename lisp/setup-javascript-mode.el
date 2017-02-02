;;; setup-javascript-mode -- customizations for JavaScript programming language

;;; Code:

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(autoload 'js2-mode "js2-mode" "Major mode for editing JavaScript code." t)
(eval-after-load "js2-mode"
  #'(lambda ()
      ;; (defvaralias 'js-indent-level 'tab-width)
      (set-default 'js2-basic-offset 4)
      (set-default 'js2-mirror-mode nil)
      (set-default 'js2-mode-escape-quotes nil)
      (add-hook 'javascript-mode-hook #'(lambda ()
                                          (add-hook 'before-save-hook #'clean-and-indent nil t)
                                          (flyspell-prog-mode)
                                          (setq fill-column 100
                                                indent-tabs-mode t
                                                ;; js-indent-level 4
                                                )))))

(provide 'setup-javascript-mode)

;;; setup-javascript-mode.el ends here
