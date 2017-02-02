;;; setup-go-mode --- customizations for the Go programming language

;;; Commentary:

;;; Code:

(require 'path)
(let ((gopath (expand-file-name "~/go")))
  (path-prepend (concat gopath "/bin"))
  (setenv "GOPATH" gopath))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;; This block sets up global Go configuration and is invoked a single time after go-mode is loaded.

(with-eval-after-load 'go-mode

  ;; go-eldoc -- eldoc for the Go programming language
  (with-eval-after-load 'go-autocomplete
    (set-face-attribute 'eldoc-highlight-function-argument nil
                        :underline t :foreground "green"
                        :weight 'bold)
    (add-hook 'go-mode-hook #'go-eldoc-setup))

  ;; gocode -- an autocompletion daemon for the Go programming language
  (let ((dir (concat (getenv "GOPATH") "/src/github.com/nsf/gocode/emacs")))
    (if (file-directory-p dir)
        (progn
          (add-to-list 'load-path dir)
          (require 'go-autocomplete)
          (require 'auto-complete-config)
          (ac-config-default))
      (message "Cannot find gocode: %s" dir)))

  ;; go-mode configuration
  (setq godoc-command "godoc")
  (setq gofmt-command (or (executable-find "goimports")
                          (executable-find "gofmt"))))

;; This block sets up buffer scoped configuration and is invoked every time a new go-mode buffer is created.

(add-hook 'go-mode-hook #'(lambda ()
                            (add-hook 'before-save-hook #'gofmt-before-save nil t)
                            (flyspell-prog-mode)
                            (setq fill-column 100)
                            (auto-complete-mode 1)
                            ;; (local-set-key (kbd "C-c C-j") 'godef-jump-other-window)
                            (local-set-key (kbd "M-.") #'godef-jump)
                            (if (not (string-match "^go" compile-command))
                                (set (make-local-variable 'compile-command) "go vet && golint && go test"))))

(provide 'setup-go-mode)

;;; setup-go-mode.el ends here
