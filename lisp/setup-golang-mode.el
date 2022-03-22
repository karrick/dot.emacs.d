;;; setup-golang-mode --- customizations for the Go programming language

;;; Commentary:

;;; Code:

;; Configure emacs and environment for the Go programming language.

(require 'path)

(let ((gopath (expand-file-name "~/go")))
  (path-prepend (concat gopath "/bin"))
  ;; (setenv "GOBIN" (expand-file-name "~/bin"))
  (setenv "GOPATH" gopath))

(require 'require-package)

(require-package/with-requirements '(go-mode)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

  ;; Prefer goimports, but when not found, use gofmt.
  (setq gofmt-command (or (executable-find "goimports")
                          (executable-find "gofmt")))

  ;; Use gogetdoc as it provides better documentation.
  (when (executable-find "gogetdoc")
    (setq godoc-at-point-function #'godoc-gogetdoc))

  (when (executable-find "gopls")
    (require-package/with-requirements '(lsp-mode)
      (add-hook 'go-mode-hook #'lsp-deferred)))

  ;; This block sets up buffer scoped configuration and is invoked
  ;; every time a new go-mode buffer is created.
  (add-hook 'go-mode-hook #'(lambda ()
                              ;; (set (make-local-variable 'compile-command) "go test")
                              (add-hook 'before-save-hook #'gofmt-before-save nil t))))

(provide 'setup-golang-mode)

;;; setup-golang-mode.el ends here
