;;; setup-golang-mode --- customizations for the Go programming language

;;; Commentary:

;;; Code:

;; Configure emacs and environment for the Go programming language.

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(let ((gopath (expand-file-name "go" "~")))
  (setenv "GOPATH" gopath))

(when (null (getenv "GOCACHE"))
  (setenv "GOCACHE" (concat (file-name-as-directory (or (getenv "XDG_CACHE_HOME")
                                                        (expand-file-name ".cache" "~")))
                            "go-build")))

;; Use gogetdoc as it provides better documentation.
(when (executable-find "gogetdoc")
  (setq godoc-at-point-function #'godoc-gogetdoc))

(when nil
  ;; Fix parsing of error and warning lines in compiler output.
  (setq compilation-error-regexp-alist-alist ; first remove the standard conf; it's not good.
        (remove 'go-panic
                (remove 'go-test compilation-error-regexp-alist-alist)))
  ;; Make another one that works better and strips more space at the beginning.
  (add-to-list 'compilation-error-regexp-alist-alist
               '(go-test . ("^[[:space:]]*\\([_a-zA-Z./][_a-zA-Z0-9./]*\\):\\([0-9]+\\):.*$" 1 2)))
  (add-to-list 'compilation-error-regexp-alist-alist
               '(go-panic . ("^[[:space:]]*\\([_a-zA-Z./][_a-zA-Z0-9./]*\\):\\([0-9]+\\)[[:space:]].*$" 1 2)))
  ;; override.
  (add-to-list 'compilation-error-regexp-alist 'go-test t)
  (add-to-list 'compilation-error-regexp-alist 'go-panic t)
  )

(let ((cmd (executable-find "gopls")))
  (if cmd
      (progn
        ;; When gopls language server is found, install hooks for
        ;; go-mode to use it.
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
      ;; When cannot find gopls language server, configure a
      ;; different go-mode-hook for graceful feature degredation.

      ;; Prefer goimports, but when not found, use gofmt.
      (setq gofmt-command (or (executable-find "goimports")
                              (executable-find "gofmt")))
      (add-hook 'go-mode-hook #'(lambda ()
                                  (add-hook 'before-save-hook #'gofmt-before-save nil t))))))

(provide 'setup-golang-mode)

;;; setup-golang-mode.el ends here
