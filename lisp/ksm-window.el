;;; ksm-window --- provides handy window management functions
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
;;; Use:
;;;
;;; I put this file somewhere Emacs will find it, then added the below to my
;;; ~/.emacs/init.el file.
;;;
;;;    (require 'ksm-window)
;;;    (global-set-key (kbd "C-x w") #'ksm/window-zoom-out)
;;;
;;; Some people might like to override the default key-binding for the
;;; 'delete-other-windows' function, as it has a similar purpose to this
;;; library's zoom-in function:
;;;
;;;    (global-set-key (kbd "C-x 1") #'ksm/window-zoom-in)
;;;
;;; However, to force myself to learn learn tmux like key-bindings, I bind the
;;; zoom-in function to "C-x z" and rebind "C-x 1" to print a friendly reminder
;;; message.
;;;
;;;    (global-set-key (kbd "C-x 1") #'(lambda() (interactive) (message "Use C-x z")))
;;;    (global-set-key (kbd "C-x z") #'ksm/window-zoom-in) ; similar key-binding to tmux
;;;
;;; Sometimes I want to name a particular window configuration and quickly
;;; return to it. Perhaps I'm working on Project A and a colleague asks me to
;;; clarify how something in Project B works.  I save my window configuration
;;; with a string name, such as "a", open up other files to answer the question,
;;; then can restore my window configuration as it was before I loaded other
;;; files.
;;;
;;;    (global-set-key (kbd "C-x p") #'ksm/window-config-save)
;;;    (global-set-key (kbd "C-x j") #'ksm/window-config-restore)
;;;
;;; Code:

(defun ksm/window-delete-above ()
  "Delete the window above the current window."
  (interactive)
  (delete-window (window-in-direction 'above)))

(defun ksm/window-delete-below ()
  "Delete the window below the current window."
  (interactive)
  (delete-window (window-in-direction 'below)))

(defun ksm/window-delete-left ()
  "Delete the window to the left of the current window."
  (interactive)
  (delete-window (window-in-direction 'left)))

(defun ksm/window-delete-right ()
  "Delete the window to the right of the current window."
  (interactive)
  (delete-window (window-in-direction 'right)))

(defun ksm/window-lock ()
  "Mark the current window as dedicated for its current buffer."
  (interactive)
  (set-window-dedicated-p (get-buffer-window) t))

(defun ksm/window-unlock ()
  "Unmark the current window as dedicated for its current buffer."
  (interactive)
  (set-window-dedicated-p (get-buffer-window) nil))

(defvar ksm/window-configurations-list nil "Stack of saved window configurations.")

(defun ksm/window-zoom-in ()
  "Expand current window to entire frame, pushing the window configuration on the stack beforehand."
  (interactive)
  (push (cons (current-window-configuration) (point-marker)) ksm/window-configurations-list)
  (delete-other-windows)
  (message "zoomed in to %s" (current-buffer)))

(defun ksm/window-zoom-out ()
  "Pop a window configuration off the stack and restore it."
  (interactive)
  (let ((config (pop ksm/window-configurations-list)))
    (cond
     (config (set-window-configuration (car config))
             (goto-char (cdr config))
             (message "zoomed out"))
     (t (message "no more window configurations on the stack")))))

(defvar ksm/window-configurations-hash (make-hash-table :test #'equal) "Hash table of saved window configurations.")

(defun ksm/window-config-save (name)
  "Prompt user and save window configuration identified by NAME."
  (interactive "sSave Window Config As: ")
  (puthash name
           (cons (current-window-configuration) (point-marker))
           ksm/window-configurations-hash)
  (delete-other-frames)
  (message "saved window configuration: %s" name))

(defun ksm/window-config-list ()
  "List saved window configurations."
  (interactive)
  (message "saved window configurations: %s" (ksm/window--configs)))

(defun ksm/window--configs ()
  "Return list of saved window configurations."
  (interactive)
  (let ((keys '()))
    (maphash (lambda (k _v) (push k keys)) ksm/window-configurations-hash)
    keys))

(defun ksm/window-config-restore (name)
  "Prompt user and restore window configuration identified by NAME."
  (interactive "sRestore Window Config: ")
  (let ((config (gethash name ksm/window-configurations-hash)))
    (cond
     (config (set-window-configuration (car config))
             (goto-char (cdr config))
             (message "restored window configuration: %s" name))
     (t (message "cannot restore unknown window configuration: %s" name)))))

(defun ksm/window-config-drop (name)
  "Prompt user and drop window configuration identified by NAME."
  (interactive "sDrop Window Config: ")
  (cond
   ((gethash name ksm/window-configurations-hash)
    (remhash name ksm/window-configurations-hash)
    (message "dropped window configuration: %s" name))
   (t (message "cannot drop unknown window configuration: %s" name))))

(provide 'ksm-window)

;;; ksm-window.el ends here
