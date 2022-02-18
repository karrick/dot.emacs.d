;;; setup-golang-mode --- customizations for the Go programming language

;;; Commentary:

;;; Code:

;; configure emacs and environment for the Go programming language.

(require 'path)

(let ((gopath (expand-file-name "~/go")))
  (path-prepend (concat gopath "/bin"))
  ;; (setenv "GOBIN" (expand-file-name "~/bin"))
  (setenv "GOPATH" gopath))

(require 'require-package)

(require-package/with-requirements '(go-mode)
  ;; Display messages for missing command line tools that don't have
  ;; associated required packages.
  (dolist (tuple '(
                   ("godef" "github.com/rogpeppe/godef")
                   ("maligned" "github.com/mdempsky/maligned") ; ???
                   ("staticcheck" "honnef.co/go/tools/cmd/staticcheck")
                   ("unconvert" "github.com/mdempsky/unconvert")
                   ))
    (unless (executable-find (car tuple))
      (message "Cannot find %s: 'go install %s@latest'" (car tuple) (cadr tuple))))

  ;; Prefer goimports, but if not found, display message and use
  ;; gofmt.
  (setq gofmt-command (or (executable-find "goimports")
                          (progn
                            (message "Cannot find goimports: 'go install golang.org/x/tools/cmd/goimports@latest'")
                            (executable-find "gofmt"))))

  ;; go-eldoc -- eldoc for the Go programming language
  ;; TODO: looking at go-eldoc code, looks like it uses 'gocode' binary.
  (require-package/with-requirements '(go-eldoc)
    (set-face-attribute 'eldoc-highlight-function-argument nil
                        :underline t :foreground "green"
                        :weight 'bold)
    (add-hook 'go-mode-hook #'go-eldoc-setup))

  ;; gogetdoc: provides better documentation
  (let ((cmd (executable-find "gogetdoc")))
    (if (equal cmd nil)
        (message "Cannot find gogetdoc: 'go install github.com/zmb3/gogetdoc@latest'")
      (setq godoc-at-point-function #'godoc-gogetdoc)))

  ;; goflymake -- not sure whether this was causing problems in the
  ;; past
  (let ((cmd (executable-find "goflymake"))
        (dir (path-concat (getenv "GOPATH") "src/github.com/dougm/goflymake")))
    (if (or (equal cmd nil)
            (not (file-directory-p dir)))
        (message "Cannot find: 'mkdir -p ~/go/src/github.com/dougm && cd ~/go/src/github.com/dougm && git clone https://github.com/dougm/goflymake && cd goflymake && go install'")
      (add-to-list 'load-path dir)
      (require 'go-flycheck)))

  ;; ;; NOTE: I do not know whether gocode actually ever does anything...
  ;; ;; go-autocomplete
  ;; (require-package/with-requirements '(go-autocomplete)
  ;;   ;; gocode -- autocompletion daemon for the Go programming language (requires go-autocomplete)
  ;;   (let ((dir (path-concat (getenv "GOPATH") "src/github.com/nsf/gocode/emacs")))
  ;;     (if (file-directory-p dir)
  ;;         (progn
  ;;           (add-to-list 'load-path dir)
  ;;           (ac-config-default)
  ;;           (add-hook 'go-mode-hook #'(lambda () (auto-complete-mode 1))))
  ;;       (message "Cannot find gocode: 'go install github.com/nsf/gocode@latest'"))))

  ;; This block sets up buffer scoped configuration and is invoked
  ;; every time a new go-mode buffer is created.
  (add-hook 'go-mode-hook #'(lambda ()
                              (add-hook 'before-save-hook #'gofmt-before-save nil t)
                              ;; (set (make-local-variable 'compile-command) "go test")
                              (local-set-key (kbd "C-c C-d") #'godoc-at-point)
                              (local-set-key (kbd "M-.") #'godef-jump-other-window)))

  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(when nil
  (custom-set-variables
   '(go-add-tags-style 'lower-camel-case))
  (with-eval-after-load 'go-mode
    (define-key go-mode-map (kbd "C-c t") #'go-add-tags)))

(when nil
  (add-mode-hook 'go-mode'hook (lambda ()
                                 (setq tab-width 4))))

;; % go get golang.org/x/tools/cmd/gorename
;; % go build golang.org/x/tools/cmd/gorename
;; % mv gorename $HOME/bin/         # or elsewhere on $PATH

(provide 'setup-golang-mode)

;;; setup-golang-mode.el ends here
