;;; setup-autocomplete --- autocomplete configuration

;;; Commentary:

;;; Code:

(require 'require-package)

(require-package/with-requirements '((auto-complete :archive "melpa-stable"))
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories
               (concat user-emacs-directory (convert-standard-filename ".ac-dict")))
  (add-to-list 'ac-modes 'html-mode)
  (add-to-list 'ac-modes 'nxml-mode)
  (ac-config-default) ;; ac-common-setup is called by ac-config-default
  (defun ac-common-setup () (add-to-list 'ac-sources 'ac-source-yasnippet))
  (ac-config-default)
  (defun enable-auto-complete-mode () (auto-complete-mode 1))
  (defun disable-auto-complete-mode () (auto-complete-mode 0))
  (ac-flyspell-workaround)

  (require-package/with-requirements '((ac-emoji :archive "melpa-stable"))
    (cond ((eq system-type 'darwin)
           (set-fontset-font
            t 'symbol
            (font-spec :family "Apple Color Emoji") nil 'prepend))
          ((eq system-type 'gnu/linux)
           (set-fontset-font
            t 'symbol
            (font-spec :family "Symbola") nil 'prepend)))
    (add-hook 'markdown-mode-hook #'ac-emoji-setup)
    (add-hook 'git-commit-mode-hook #'ac-emoji-setup)))

(provide 'setup-autocomplete)

;;; setup-autocomplete.el ends here
