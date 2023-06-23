;;; ksm-window --- provides handy window management functions
;;;
;;; Commentary:
;;;
;;; I frequently find myself in a situation where I have half a dozen
;;; Emacs windows in a frame, and I want to focus on just one of those
;;; windows for a few moments, and then later return my window state
;;; to its present configuration.  Emacs already has facilities for
;;; accomplishing such tasks, but I wanted a quick toggle function
;;; like tmux' "C-b z" pane toggling feature.
;;;
;;; Use:
;;;
;;; I put this file somewhere Emacs will find it, then added the below
;;; to my ~/.emacs/init.el file.
;;;
;;;    (require 'ksm-window)
;;;
;;;    ;; Deletes the current window. With universal prefix it kills a
;;;    ;; buffer and deletes the window.
;;;    (global-set-key (kbd "C-x 0") #'ksm/delete-window)
;;;
;;;    ;; Deletes other windows vertically. With universal prefix it
;;;    ;; deletes all other windows.
;;;    (global-set-key (kbd "C-x 1") #'ksm/delete-other-windows)
;;;
;;;    ;; Pops and restores window configuration from stack.
;;;    (global-set-key (kbd "C-x -") #'ksm/window-zoom-out)
;;;
;;;    ;; Pushes window configuration to stack and deletes other
;;;    ;; windows.
;;;    (global-set-key (kbd "C-x +") #'ksm/window-zoom-in)
;;;
;;;    ;; Balances all windows.
;;;    (global-set-key (kbd "C-x =") #'balance-windows)
;;;
;;; Sometimes I want to name a particular window configuration and
;;; quickly return to it. Perhaps I'm working on Project A and a
;;; colleague asks me to clarify how something in Project B works.  I
;;; save my window configuration with a string name, such as "a", open
;;; up other files to answer the question, then can restore my window
;;; configuration as it was before I loaded other files.
;;;
;;;    (global-set-key (kbd "C-x p") #'ksm/window-config-save)
;;;    (global-set-key (kbd "C-x j") #'ksm/window-config-restore)
;;;
;;; Code:

(defun ksm/delete-other-windows (&optional vertical)
  "Delete all other windows; with VERTICAL delete other windows vertically."
  (interactive "P")
  (if vertical
	  (delete-other-windows-vertically)
	(delete-other-windows)))

(defun ksm/delete-window (&optional bury)
  "Delete the current window.  With optional BURY, bury the current buffer."
  (interactive "P")
  (if (not bury)
	  (delete-window)
	(delete-window)
	(bury-buffer)))

(defun ksm/delete-window-above ()
  "Delete the window above the current window."
  (interactive)
  (delete-window (window-in-direction 'above)))

(defun ksm/delete-window-below ()
  "Delete the window below the current window."
  (interactive)
  (delete-window (window-in-direction 'below)))

(defun ksm/delete-window-left ()
  "Delete the window to the left of the current window."
  (interactive)
  (delete-window (window-in-direction 'left)))

(defun ksm/delete-window-right ()
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
  "Push window config on stack then expand current window to entire frame."
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

(defun other-window-backward (&optional n)
  "Select the Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(provide 'ksm-window)

;;; ksm-window.el ends here
