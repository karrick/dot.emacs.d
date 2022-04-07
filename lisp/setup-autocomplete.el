;;; setup-autocomplete --- autocomplete configuration

;;; Commentary:

;;; Code:

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
			 (locate-user-emacs-file ".ac-dict"))
(add-to-list 'ac-modes 'html-mode)
(add-to-list 'ac-modes 'nxml-mode)
(ac-config-default) ;; ac-common-setup is called by ac-config-default
(defun ac-common-setup () (add-to-list 'ac-sources 'ac-source-yasnippet))
(ac-config-default)
(defun enable-auto-complete-mode () (auto-complete-mode 1))
(defun disable-auto-complete-mode () (auto-complete-mode 0))
(ac-flyspell-workaround)

(provide 'setup-autocomplete)

;;; setup-autocomplete.el ends here
