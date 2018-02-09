;;; setup-rust-mode --- customizations for the Rust programming language

;;; Commentary:

;;; Code:

;; configure emacs and environment for the Rust programming language

(require 'path)

(path-prepend (expand-file-name "~/.cargo/bin"))

(provide 'setup-rust-mode)

;;; setup-rust-mode.el ends here
