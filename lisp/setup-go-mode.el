;;; setup-go-mode --- customizations for the Go programming language

;;; Commentary:

;;; Code:

;; configure emacs and environment for the Go programming language

(require 'require-package)

(require 'path)
(let ((gopath (expand-file-name "~/go")))
  (path-prepend (concat gopath "/bin"))
  (setenv "GOPATH" gopath))

(require-package/with-requirements '((go-mode :archive "melpa-stable"))
  ;; display messages for missing command line tools that don't have required packages
  (dolist (tuple '(
		   ("godef" "github.com/rogpeppe/godef")
		   ("unconvert" "github.com/mdempsky/unconvert")
		   ))
    (unless (executable-find (car tuple))
      (message "Cannot find %s: `go get -u %s`" (car tuple) (cadr tuple))))

  ;; prefer goimports, but if not found, display message and use gofmt
  (setq gofmt-command (or (executable-find "goimports")
			  (progn
			    (message "Cannot find goimports: `go get -u golang.org/x/tools/cmd/goimports`")
			    (executable-find "gofmt"))))

  ;; go-mode configuration
  (setq godoc-command (executable-find "godoc"))

  ;; gorename
  (if (executable-find "gorename")
      (require-package/ensure-installed '((go-rename :archive "melpa")))
    (message "Cannot find gorename: `go get -u golang.org/x/tools/cmd/gorename`"))

  ;; golint
  (if (executable-find "golint")
      (require-package/ensure-installed '((golint :archive "melpa")))
    (message "Cannot find golint: `go get -u github.com/golang/lint/golint`"))

  (require-package/with-requirements '((go-eldoc :archive "melpa-stable"))
    ;; go-eldoc -- eldoc for the Go programming language
    (set-face-attribute 'eldoc-highlight-function-argument nil
			:underline t :foreground "green"
			:weight 'bold)
    (add-hook 'go-mode-hook #'go-eldoc-setup))

  (require-package/with-requirements '((go-autocomplete :archive "melpa-stable"))
    ;; gocode -- an autocompletion daemon for the Go programming language
    (let ((dir (path-concat (getenv "GOPATH") "src/github.com/nsf/gocode/emacs")))
      (if (file-directory-p dir)
	  (progn
	    (add-to-list 'load-path dir)
	    (ac-config-default)
	    (add-hook 'go-mode-hook #'(lambda () (auto-complete-mode 1))))
	(message "Cannot find gocode: `go get -u github.com/nsf/gocode`"))))

  ;; This block sets up buffer scoped configuration and is invoked every time a new go-mode buffer is created.
  (add-hook 'go-mode-hook #'(lambda ()
			      (add-hook 'before-save-hook #'gofmt-before-save nil t)
			      (flyspell-prog-mode)
			      (setq fill-column 100)
			      ;; (local-set-key (kbd "C-c C-j") 'godef-jump-other-window)
			      (local-set-key (kbd "M-.") #'godef-jump)
			      (if (not (string-match "^go" compile-command))
				  (set (make-local-variable 'compile-command) "go vet && go test && golint"))))

  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(provide 'setup-go-mode)

;;; setup-go-mode.el ends here
