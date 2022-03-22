;;; setup-golang-mode --- customizations for the Go programming language

;;; Commentary:

;;; Code:

;; Configure emacs and environment for the Go programming language.

(require 'require-package)

(require-package/with-requirements '(go-mode)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

  (require 'path)
  (let ((gopath (expand-file-name "~/go")))
    (path-prepend (concat gopath "/bin"))
    ;; (setenv "GOBIN" (expand-file-name "~/bin"))
    (setenv "GOPATH" gopath))

  ;; Use gogetdoc as it provides better documentation.
  (when (executable-find "gogetdoc")
    (setq godoc-at-point-function #'godoc-gogetdoc))

  (let ((cmd (executable-find "gopls")))
    (if (stringp cmd)
        (progn
          ;; When gopls executable is found, install hooks for go-mode
          ;; to use it.
          (require 'lsp-mode)
          (setq lsp-go-gopls-server-path cmd)
          (add-hook 'go-mode-hook #'lsp-deferred)
          ;; Set up before-save hooks to format buffer and add/delete
          ;; imports. Make sure there is not another gofmt or
          ;; goimports hook enabled.
          (defun lsp-go-install-save-hooks ()
            (add-hook 'before-save-hook #'lsp-format-buffer t t)
            (add-hook 'before-save-hook #'lsp-organize-imports t t))
          (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))
      (progn
        ;; When cannot find gopls language server, configure a different
        ;; go-mode-hook for graceful feature degredation.

        ;; Prefer goimports, but when not found, use gofmt.
        (setq gofmt-command (or (executable-find "goimports")
                                (executable-find "gofmt")))
        (add-hook 'go-mode-hook #'(lambda ()
                                    (add-hook 'before-save-hook #'gofmt-before-save nil t)))))))

(provide 'setup-golang-mode)

;;; setup-golang-mode.el ends here
