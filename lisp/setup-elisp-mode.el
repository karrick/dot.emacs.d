;;; setup-elisp-mode -- customizations for Emacs Lisp programming language

;;; Commentary:

;;; Code:

(require 'clean-and-indent)

(defun auto-recompile-el-buffer ()
  "Recompiles Emacs Lisp file after if byte compiled file already exists."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)
    (message "Byte compiled %s" buffer-file-name)))

(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (add-hook 'after-save-hook #'auto-recompile-el-buffer)
              (add-hook 'before-save-hook #'clean-and-indent nil t)
              (hl-line-mode 1)
              (eldoc-mode)
              (flyspell-prog-mode)
              (setq fill-column 80)))

(provide 'setup-elisp-mode)

;;; setup-elisp-mode.el ends here
