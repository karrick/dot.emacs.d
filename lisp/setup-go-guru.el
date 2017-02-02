;;; setup-go-guru -- customizations for the Go programming language

;;; Commentary:

;; Sadly this customization is not working right now because go-guru.el removed from
;; golang.org/x/tools/cmd/guru, but not yet available in recent go-mode.el.

;;; Code:

(with-eval-after-load 'go-mode
  (let ((dir (concat (getenv "GOPATH") "/src/golang.org/x/tools/cmd/guru")))
    (if (file-directory-p dir)
        (progn
          (add-to-list 'load-path dir)
          (require 'go-guru)
          (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
          (set-face-attribute go-guru-hl-identifier-face nil
                              :background "chartreuse"
                              :foreground "gray0")
          (defun go-set-scope-here ()
            (interactive)
            (setq go-guru-scope (file-name-directory (buffer-file-name))))
          )
      (message "Cannot find go-guru: %s" dir))))

(provide 'setup-go-guru)

;;; setup-go-guru.el ends here
