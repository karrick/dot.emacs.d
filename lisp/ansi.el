(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))
