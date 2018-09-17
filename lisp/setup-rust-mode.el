;;; setup-rust-mode --- customizations for the Rust programming language

;;; Commentary:

;;; Code:

;; configure emacs and environment for the Rust programming language

(require 'path)

(path-prepend (expand-file-name "~/.cargo/bin"))

(require-package/with-requirements '(rust-mode)
  ;; (autoload 'rust-mode "rust-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))


(provide 'setup-rust-mode)

;;; setup-rust-mode.el ends here
