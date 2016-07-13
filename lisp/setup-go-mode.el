;;; setup-go-mode --- customizations for Go programming language

;;; Code:

(require 'go-autocomplete)

(let ((gopath (expand-file-name "~/go")))
  (prepend-path (concat gopath "/bin"))
  (setenv "GOPATH" gopath)
  (setenv "GO15VENDOREXPERIMENT" "1")   ; won't need this much longer
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (eval-after-load "go-mode"
    #'(lambda ()
        (let ((gofmter (find-first #'(lambda (item)
                                       (executable-find item))
                                   '("goimports"
                                     "gofmt"))))
          (setq gofmt-command gofmter))
        (add-hook 'go-mode-hook
                  #'(lambda ()
                      (add-hook 'before-save-hook #'gofmt-before-save nil t)
                      (flyspell-prog-mode)
                      (setq fill-column 100)
                      ;; (local-set-key (kbd "C-c C-j") 'godef-jump-other-window)
                      (if (not (string-match "^go" compile-command))
                          (set (make-local-variable 'compile-command)
                               "go test && go build"))))
        (let* ((guru (concat (getenv "GOPATH") "/src/golang.org/x/tools/cmd/guru/go-guru"))
               (guru-el (concat guru ".el")))
          (if (file-exists-p guru-el)
              (progn
                (load guru)                 ; preferentially loads .elc, then .el
                (add-hook 'go-mode-hook #'(lambda () (go-guru-hl-identifier-mode t)))
                (defun go-set-scope-here ()
                  (interactive)
                  (let ((scope (file-name-directory (buffer-file-name))))
                    (setq go-guru-scope scope))))
            (message "No such file: %s" guru-el))))))

;; (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "lisp/yasnippet-go"))

(provide 'setup-go-mode)

;;; setup-go-mode.el ends here
