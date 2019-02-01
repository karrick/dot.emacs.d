;;; ksm-window-toggle --- provides a handy window toggle function

;;; Commentary:

;;; This package uses register 0 to store the window layout when zooming in.

;;; Code:

(defvar ksm/window-toggle-zoomed nil "Internal variable that is non-nil when zoomed from multiple windows.")

(defun ksm/window-toggle ()
  "Toggle current frame between single window and multiple windows."
  (interactive)

  (if ksm/window-toggle-zoomed
      (progn
        (jump-to-register ?0)
        (setq ksm/window-toggle-zoomed nil)
        (message "restored to previously saved window layout"))
    (if (eq 1 (length (window-list)))
        (message "cannot zoom when already single window")
      (progn
        ;; When not zoomed in, and multiple windows, then save this layout.
        (window-configuration-to-register ?0)
        (delete-other-windows)
        (setq ksm/window-toggle-zoomed t)
        (message "zoomed in from multiple window layout")))))

(provide 'ksm-window-toggle)

;;; ksm-window-toggle.el ends here
