;;; setup-zig-mode --- customizations for the Zig programming language

;;; Commentary:

;;; Code:

;; configure emacs and environment for the Go programming language

(require 'require-package)

(require-package/with-requirements '(zig-mode)
  (let ((cmd (executable-find "zls")))
    (when cmd
      (require 'lsp)
      (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
      (lsp-register-client
       (make-lsp-client
	:new-connection (lsp-stdio-connection (executable-find "zls"))
	:major-modes '(zig-mode)
	:server-id 'zls))))

  ;; ;; This block sets up buffer scoped configuration and is invoked every time a new zig-mode buffer is created.
  (add-hook 'zig-mode-hook
	    #'(lambda ()
		(set (make-local-variable 'compile-command) (concat "zig test " (buffer-file-name)))))
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode)))

(provide 'setup-zig-mode)

;;; setup-zig-mode.el ends here
