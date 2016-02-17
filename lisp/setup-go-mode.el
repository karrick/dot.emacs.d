;;; setup-go-mode --- customizations for Go programming language

;;; Code:

(require 'go-autocomplete)

(let ((gopath (expand-file-name "~/go")))
  (prepend-path (concat gopath "/bin"))
  (setenv "GOPATH" gopath)
  (setenv "GO15VENDOREXPERIMENT" "1")
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (load (concat gopath "/src/golang.org/x/tools/cmd/oracle/oracle"))
  (add-hook 'go-mode-hook
            #'(lambda ()
                (add-hook 'before-save-hook #'gofmt-before-save nil t)
                (flyspell-prog-mode)
                (setq fill-column 100)
                (local-set-key (kbd "C-c C-j") 'godef-jump-other-window)
                (if (not (string-match "^go" compile-command))
                    (set (make-local-variable 'compile-command)
                         "go test && go build"))))
  (eval-after-load "go-mode"
    #'(lambda ()
        (let ((gofmter (find-first #'(lambda (item)
                                       (executable-find item))
                                   '("goimports"
                                     "gofmt"))))
          (setq gofmt-command gofmter)))))

;; (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "lisp/yasnippet-go"))


(provide 'setup-go-mode)

;;; setup-go-mode.el ends here
