;;; setup-ruby-mode -- customizations for Ruby programming language

;;; Commentary:

;;; Code:

(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))

(eval-after-load "ruby-mode"
  #'(lambda ()
      (setq ruby-deep-indent-paren nil) ; avoid ridiculous ruby indentation
      (defun ruby--jump-to-test ()
	(find-file
	 (replace-regexp-in-string
	  "/lib/" "/test/"
	  (replace-regexp-in-string
	   "/\\([^/]+\\).rb$" "/test_\\1.rb"
	   (buffer-file-name)))))
      (defun ruby--jump-to-lib ()
	(find-file
	 (replace-regexp-in-string
	  "/test/" "/lib/"
	  (replace-regexp-in-string
	   "/test_\\([^/]+\\).rb$" "/\\1.rb"
	   (buffer-file-name)))))
      (defun ruby-jump-to-other ()
	(interactive)
	(if (string-match-p "/test/" (buffer-file-name))
	    (ruby--jump-to-lib)
	  (ruby--jump-to-test)))
      ;; (add-hook 'ruby-mode-hook
      ;;           #'(lambda ()
      ;;               (local-set-key (kbd "C-c C-j") 'ruby-jump-other)
      ))

(provide 'setup-ruby-mode)

;;; setup-ruby-mode.el ends here
