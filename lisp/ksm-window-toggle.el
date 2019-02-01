;;; ksm-window-toggle --- provides a handy window toggle function

;;; Commentary:

;;; Code:

(setq ksm/window-toggle-zoomed nil)

(defun ksm/window-toggle ()
  "Toggle current frame between single window and multiple windows."
  (interactive)
  (if ksm/window-toggle-zoomed
      (progn
        (jump-to-register ?0)
        (setq ksm/window-toggle-zoomed nil))
    (progn
      (window-configuration-to-register ?0)
      (delete-other-windows)
      (setq ksm/window-toggle-zoomed t))))

(provide 'ksm-window-toggle)

;;; ksm-window-toggle.el ends here
