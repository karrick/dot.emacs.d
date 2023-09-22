;;; setup-aspell -- configure and start `aspell(1)` helper program

;;; Commentary:
;;;
;;; Based on suggestions found at
;;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html

;;; Code:

(require 'empty-string)

(let ((cmd (executable-find "aspell")))
  (if (empty-string-p cmd)
	  (message "Cannot find spelling program: consider installing `aspell' and `en-aspell' packages.")
	(add-hook 'prog-mode-hook #'flyspell-prog-mode)
	(setq ispell-program-name cmd
		  ;; NOTE: ispell-extra-args contains actual parameters that will be
		  ;; passed to aspell.
		  ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

(provide 'setup-aspell)

;;; setup-aspell.el ends here
