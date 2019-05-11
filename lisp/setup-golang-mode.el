;;; setup-golang-mode --- customizations for the Go programming language

;;; Commentary:

;;; Code:

;; configure emacs and environment for the Go programming language

(require 'path)
(let ((gopath (expand-file-name "~/go")))
  (path-prepend (concat gopath "/bin"))
  ;; (setenv "GOBIN" (expand-file-name "~/bin"))
  (setenv "GOPATH" gopath))

(require 'require-package)

(require-package/with-requirements '(go-mode)

  ;; display messages for missing command line tools that don't have
  ;; associated required packages
  (dolist (tuple '(
                   ("godef" "github.com/rogpeppe/godef")
                   ("maligned" "github.com/mdempsky/maligned") ; ???
                   ("unconvert" "github.com/mdempsky/unconvert")
                   ))
    (unless (executable-find (car tuple))
      (message "Cannot find %s: `go get %s`" (car tuple) (cadr tuple))))

  ;; prefer goimports, but if not found, display message and use gofmt
  (setq gofmt-command (or (executable-find "goimports")
                          (progn
                            (message "Cannot find goimports: `go get golang.org/x/tools/cmd/goimports`")
                            (executable-find "gofmt"))))

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

  ;; gogetdoc: provides better documentation
  (let ((cmd (executable-find "gogetdoc")))
    (if (string-equal cmd "")
        (message "Cannot find gogetdoc: `go get github.com/zmb3/gogetdoc`")
      (setq godoc-at-point-function #'godoc-gogetdoc)))

  ;; gorename
  (let ((cmd (executable-find "gorename")))
    (if (string-equal cmd "")
        (message "Cannot find gorename: `go get golang.org/x/tools/cmd/gorename`")
      (require-package/with-requirements '(go-rename)
        (setq go-rename-command cmd))))

  ;; golint
  (if (executable-find "golint")
      (require-package/ensure-require '(golint))
    (message "Cannot find golint: `go get golang.org/x/lint/golint`"))

  ;; guru
  (let ((cmd (executable-find "guru")))
    (if (string-equal cmd "")
        (message "Cannot find guru: `go get golang.org/x/tools/cmd/guru`")
      (progn
        (require 'go-guru)
        (setq go-guru-command cmd)
        (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
        (set-face-attribute 'go-guru-hl-identifier-face nil
                            :background "chartreuse"
                            :foreground "gray0")
        (defun go-set-scope-here ()
          (interactive)
          (setq go-guru-scope (file-name-directory (buffer-file-name)))))))

  ;; This block sets up buffer scoped configuration and is invoked every time a new go-mode buffer is created.
  (add-hook 'go-mode-hook #'(lambda ()
                              (add-hook 'before-save-hook #'gofmt-before-save nil t)
                              (local-set-key (kbd "C-c C-d") #'godoc-at-point)
                              (local-set-key (kbd "M-.") #'godef-jump-other-window)
                              (set (make-local-variable 'compile-command) "go test")))

  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(provide 'setup-golang-mode)

;;; setup-golang-mode.el ends here
