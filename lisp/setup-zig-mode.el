;;; setup-zig-mode --- customizations for the Zig programming language

;;; Commentary:

;;; Code:

;; configure emacs and environment for the Go programming language

;; (require 'path)
;; (let ((gopath (expand-file-name "~/go")))
;;   (path-prepend (concat gopath "/bin"))
;;   ;; (setenv "GOBIN" (expand-file-name "~/bin"))
;;   (setenv "GOPATH" gopath))

(require 'require-package)

(require-package/with-requirements '(zig-mode)
  ;; (defun zigfmt ()
  ;;     (interactive)
  ;;     ;;
  ;;     (let ((start (point-min))
  ;;           (stop (point-max)))
  ;;       (copy-region-as-kill start stop)
  ;;       (shell-command-on-
  ;;     )

  ;; ;; This block sets up buffer scoped configuration and is invoked every time a new go-mode buffer is created.
  ;; (add-hook 'zig-mode-hook #'(lambda ()
  ;;                             (add-hook 'before-save-hook #'zigfmt-before-save nil t)
  ;;                             (local-set-key (kbd "C-c C-d") #'godoc-at-point)
  ;;                             (local-set-key (kbd "M-.") #'godef-jump-other-window)
  ;;                             (set (make-local-variable 'compile-command) "zig test")))

  ;; (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))
