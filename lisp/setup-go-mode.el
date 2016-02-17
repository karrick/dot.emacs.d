;;; setup-go-mode --- customizations for Go programming language 

;;; Code:

(require 'go-autocomplete)

;; (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "lisp/yasnippet-go"))
(let ((gopath (expand-file-name "~/go")))
  (prepend-path (concat gopath "/bin"))
  (setenv "GOPATH" gopath)
  (setenv "GO15VENDOREXPERIMENT" "1")
  (eval-after-load "go-mode"
    #'(lambda ()
        (let ((gofmter (find-first #'(lambda (item)
                                       (executable-find item))
                                   '("goimports"
                                     "gofmt"))))
          (setq gofmt-command gofmter))
        (progn
          (add-to-list 'load-path (concat gopath "/src/golang.org/x/tools/cmd/oracle/oracle"))
          (require 'go-oracle)
          (add-hook 'go-mode-hook 'go-oracle-mode))))
  (add-hook 'go-mode-hook
            #'(lambda ()
                (add-hook 'before-save-hook #'gofmt-before-save nil t)
                (flyspell-prog-mode)
                (setq fill-column 100)
                (local-set-key (kbd "C-c C-j") 'godef-jump-other-window)
                (if (not (string-match "^go" compile-command))
                    (set (make-local-variable 'compile-command)
                         "go test && go build")))))

(provide 'setup-go-mode)

;;; setup-go-mode.el ends here
