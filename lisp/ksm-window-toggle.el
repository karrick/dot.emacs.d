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

(defvar ksm/window-configurations nil "Associative array of saved window configurations.")

(defun ksm/window-config-drop (name)
  "Prompt user and drop window configuration identified by NAME."
  (interactive "sDrop Window Config: ")
  (let ((config (assoc name ksm/window-configurations)))
    (if config
        (progn
          (setq ksm/window-configurations
                (ksm/window--remove-from-alist config ksm/window-configurations #'(lambda (a b) (equal (car a) (car b)))))
          (message "dropped window configuration: %s" (car config)))
      (message "cannot drop unknown window configuration: %s" (car config)))))

(defun ksm/window-config-restore (name)
  "Prompt user and restore window configuration identified by NAME."
  (interactive "sRestore Window Config: ")
  (let ((config (cdr (assoc name ksm/window-configurations))))
    (if config
        (progn
          (set-window-configuration (car config))
          (goto-char (cdr config))
          (message "restored window configuration: %s" name))
      (message "cannot restore unknown window configuration: %s" name))))

(defun ksm/window-config-save (name)
  "Prompt user and save window configuration identified by NAME."
  (interactive "sSave Window Config As: ")
  (add-to-list 'ksm/window-configurations
               (cons name (cons (current-window-configuration) (point-marker)))
               nil
               #'(lambda (a b) (equal (car a) (car b)))))

(defun ksm/window--remove-from-alist (element list &optional testfn)
  "Return new list with all instances of ELEMENT removed from LIST, using optional TESTFN."
  (when list
    (unless testfn (setq testfn #'eql))
    (let* ((head (car list))
           (tail (ksm/window--remove-from-alist element (cdr list) testfn)))
      (if (funcall testfn element head)
          tail
        (if tail
            (list head tail)
          head)))))

;;   (ksm/window--remove-from-alist (list "baz")
;;                          (list (cons "foo" (cons 'c1 'm1))
;;                                (cons "bar" (cons 'c2 'm2))
;;                                (cons "baz" (cons 'c3 'm3)))
;;                          #'(lambda (a b) (equal (car a) (car b))))


;; (list (current-window-configuration) (point-marker))

;; (set-window-configuration (car val))
;; (goto-char (cadr val))

;; (defun ksm/window-toggle ()
;;   "Toggle current frame between single window and multiple windows."
;;   (interactive)

;;   (if ksm/window-toggle-zoomed
;;       (let* ((this (car ksm/window-toggle-zoomed))
;;              (ksm/window-toggle-zoomed (cdr ksm/window-toggle-zoomed)))
;;         (jump-to-register this)
;;         (message "restored to previously saved window layout %s" this))
;;     (


;;      (if (eq 1 (length (window-list)))

;;          (progn
;;            (if (eq 1 (length (window-list)))
;;                (message "cannot zoom when already single window")
;;              (progn
;;                ;; When not zoomed in, and multiple windows, then save this layout.
;;                (window-configuration-to-register ?0)
;;                (delete-other-windows)
;;                (setq ksm/window-toggle-zoomed t)
;;                (message "zoomed in from multiple window layout")))))


;; (defvar ksm/window-toggle-zoomed nil "Internal variable that is non-nil when zoomed from multiple windows.")
;; (defvar ksm/window-config-list nil "Internal variable that is non-nil when zoomed from multiple windows.")
;; (defvar ksm/window-config-index 0 "index of currently selected window configuration.")

(provide 'ksm-window-toggle)

;;; ksm-window-toggle.el ends here
