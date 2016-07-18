;;; setup-elisp-mode -- customizations for Emacs Lisp programming language

;;; Code:

(defun auto-recompile-el-buffer ()
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)
    (message "Byte compiled %s" buffer-file-name)))
(add-hook 'after-save-hook 'auto-recompile-el-buffer)

(provide 'setup-elisp-mode)

;;; setup-elisp-mode.el ends here
