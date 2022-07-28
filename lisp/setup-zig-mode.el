;;; setup-zig-mode --- customizations for the Zig programming language

;;; Commentary:

;;; Code:

;; Configure emacs and environment for the Go programming language.

(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))

(when (executable-find "zls")
  (add-hook 'zig-mode-hook #'lsp-deferred))

;; This block sets up buffer scoped configuration and is invoked
;; every time a new zig-mode buffer is created.
(add-hook 'zig-mode-hook #'(lambda ()
			     (set (make-local-variable 'compile-command) (concat "zig test " (buffer-file-name)))))

(provide 'setup-zig-mode)

;;; setup-zig-mode.el ends here
