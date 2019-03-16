;;; setup-golang-mode --- customizations for the Go programming language

;;; Commentary:

;;; Code:

;; configure emacs and environment for the Go programming language

(require 'path)
(let ((gopath (expand-file-name "~/go")))
  (path-prepend (concat gopath "/bin"))
  (setenv "GOPATH" gopath))

(require 'require-package)

(require-package/with-requirements '(go-mode)

  ;; display messages for missing command line tools that don't have
  ;; associated required packages
  (dolist (tuple '(
                   ("godef" "github.com/rogpeppe/godef")
                   ("unconvert" "github.com/mdempsky/unconvert")
                   ))
    (unless (executable-find (car tuple))
      (message "Cannot find %s: `go get %s`" (car tuple) (cadr tuple))))

  ;; prefer goimports, but if not found, display message and use gofmt
  (setq gofmt-command (or (executable-find "goimports")
                          (progn
                            (message "Cannot find goimports: `go get golang.org/x/tools/cmd/goimports`")
                            (executable-find "gofmt"))))

  ;; prefer `go doc` over `godoc` because it provides more information
  (setq godoc-command "go doc")

  ;; gorename
  (let ((cmd (executable-find "gorename")))
    (if (not (string-equal cmd ""))
        (require-package/with-requirements '(go-rename)
          (setq go-rename-command cmd))
      (message "Cannot find gorename: `go get golang.org/x/tools/cmd/gorename`")))

  ;; go-eldoc -- eldoc for the Go programming language
  (require-package/with-requirements '(go-eldoc)
    (set-face-attribute 'eldoc-highlight-function-argument nil
                        :underline t :foreground "green"
                        :weight 'bold)
    (add-hook 'go-mode-hook #'go-eldoc-setup))

  ;; go-autocomplete
  (require-package/with-requirements '(go-autocomplete)
    ;; gocode -- autocompletion daemon for the Go programming language (requires go-autocomplete)
    (let ((dir (path-concat (getenv "GOPATH") "src/github.com/nsf/gocode/emacs")))
      (if (file-directory-p dir)
          (progn
            (add-to-list 'load-path dir)
            (ac-config-default)
            (add-hook 'go-mode-hook #'(lambda () (auto-complete-mode 1))))
        (message "Cannot find gocode: `go get github.com/nsf/gocode`"))))

  ;; golint
  (if (executable-find "golint")
      (require-package/ensure-require '(golint))
    (message "Cannot find golint: `go get github.com/golang/lint/golint`"))

  ;; This block sets up buffer scoped configuration and is invoked every time a new go-mode buffer is created.
  (add-hook 'go-mode-hook #'(lambda ()
                              (add-hook 'before-save-hook #'gofmt-before-save nil t)
                              ;; (local-set-key (kbd "C-c C-j") 'godef-jump-other-window)
                              (local-set-key (kbd "M-.") #'godef-jump)
                              (if (not (string-match "^go" compile-command))
                                  (set (make-local-variable 'compile-command) "go test -v && golint"))))

  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(provide 'setup-golang-mode)

;;; setup-golang-mode.el ends here
