;;; setup-rust-mode --- customizations for the Rust programming language

;;; Commentary:

;;; Code:

;; configure emacs and environment for the Rust programming language

(require 'path)
(require 'rustic-rustfmt)

(path-prepend (expand-file-name "~/.cargo/bin"))

(let ((cmd (executable-find "rust-analyzer")))
  (if cmd
	  (progn
		;; When rust-analyzer language server is found, install hooks for
		;; rust-mode to use it.
		(require 'rustic)

		;; "M-j" lsp-ui-imenu
		;; "M-?" lsp-find-reference
		;; "C-c C-c l" flycheck-list-erors
		;; "C-c C-c a" lsp-execute-code-action
		;; "C-c C-c r" lsp-rename
		;; "C-c C-c q" lsp-workspace-restart
		;; "C-c C-c Q" lsp-workspace-shutdown
		;; "C-c C-c s" lsp-rust-analyzer-status

		(setq rustic-format-trigger 'on-save)
		(add-hook 'rustic-mode-hook #'rk/rustic-mode-hook))))

(defun rk/rustic-mode-hook ()
  ;; So that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not visiting a file. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this
  ;; should no longer be necessary.
  (when buffer-file-name
	(setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook #'lst-format-buffer nil t))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(provide 'setup-rust-mode)

;;; setup-rust-mode.el ends here
