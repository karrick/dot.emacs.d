;;; setup-zig-mode --- customizations for the Zig programming language

;;; Commentary:

;;; Code:

;; Configure emacs and environment for the Go programming language.

(require 'require-package)

(require-package/with-requirements '(zig-mode)
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))

  (when (executable-find "zls")
    (require-package/with-requirements '(lsp-mode)
      (add-hook 'zig-mode-hook #'lsp-deferred)))

  (when nil
    (let ((cmd (executable-find "zls")))
      (if (equal cmd nil)
          (message "Cannot find zls: https://github.com/zigtools/zls/")
        (lsp-register-client
         (make-lsp-client
          :new-connection (lsp-stdio-connection cmd)
          :major-modes '(zig-mode)
          :server-id 'zls)))))

  ;; This block sets up buffer scoped configuration and is invoked
  ;; every time a new zig-mode buffer is created.
  (add-hook 'zig-mode-hook #'(lambda ()
                               (set (make-local-variable 'compile-command) (concat "zig test " (buffer-file-name))))))

(provide 'setup-zig-mode)

;;; setup-zig-mode.el ends here
