;;; ksm-window-toggle --- provides a handy window toggle function
;;;
;;; Commentary:
;;;
;;; I frequently find myself in a situation where I have half a dozen Emacs
;;; windows in a frame, and I want to focus on just one of those windows for a
;;; few moments, and then later return my window state to its present
;;; configuration.  Emacs already has facilities for accomplishing such tasks,
;;; but I wanted a quick toggle function like tmux' "C-b z" pane toggling
;;; feature.
;;;
;;; NOTE: This package uses register 0 to store the window layout when zooming
;;; in.
;;;
;;; Use:
;;;
;;; I put this file somewhere Emacs will find it, then added the below to my
;;; ~/.emacs/init.el file. Note that this binds "C-x z" to the toggle function,
;;; which both zooms to a single window and then restores the window layout to
;;; the previously saved configuration when run the next time.
;;;
;;;    (require 'ksm-window-toggle)
;;;    (global-set-key (kbd "C-x z") #'ksm/window-toggle)
;;;
;;; I also added the below to force myself to learn the new key-binding:
;;;
;;;    (global-set-key (kbd "C-x 1") #'(lambda() (interactive) (message "Use C-x z")))
;;;
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
