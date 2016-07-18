;;; setup-go-mode --- customizations for Go programming language

;;; Commentary:

;;; Code:

(require 'path)
;; (require 'go-autocomplete)

(setenv "GO15VENDOREXPERIMENT" "1")     ; won't need this much longer
(let ((gopath (expand-file-name "~/go")))
  (prepend-path (concat gopath "/bin"))
  (setenv "GOPATH" gopath))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(with-eval-after-load 'go-mode
  (setq gofmt-command (or (executable-find "goimports")
                          (executable-find "gofmt")))
  (let ((dir (concat (getenv "GOPATH") "/src/golang.org/x/tools/cmd/guru")))
    (if (file-directory-p dir)
        (progn
          (add-to-list 'load-path dir)
          (require 'go-guru)
          (add-hook 'go-mode-hook #'(lambda () (go-guru-hl-identifier-mode t)))
          (defun go-set-scope-here ()
            (interactive)
            (setq go-guru-scope (file-name-directory (buffer-file-name))))))
    (message "Cannot find go-guru: %s" dir))
  (add-hook 'go-mode-hook #'(lambda ()
                              (add-hook 'before-save-hook #'gofmt-before-save nil t)
                              (flyspell-prog-mode)
                              (setq fill-column 100)
                              (auto-complete-mode 1)
                              ;; (local-set-key (kbd "C-c C-j") 'godef-jump-other-window)
                              (if (not (string-match "^go" compile-command))
                                  (set (make-local-variable 'compile-command) "go test && go build")))))

;; (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "lisp/yasnippet-go"))

(provide 'setup-go-mode)

;;; setup-go-mode.el ends here
