;;;; -*- mode: emacs-lisp -*-

(setq Man-notify-method 'aggressive) ; Man-mode makes manpage active buffer
(setq compilation-scroll-output 'first-error) ; Compilation mode scrolls to first error
(setq use-file-dialog nil) ; Shut off a really annoying new dialog box

;;;; auto-complete-mode
(add-to-list 'ac-dictionary-directories 
	     (expand-file-name "~/.emacs.d/ac-dict"))
(ac-config-default)

;;;; Buffer menu mode sort function
(defun buffer-list-sort (column)
  (interactive "SColumn to sort by (one of name,size,mode,file,time [default=time]): ")
  (case column
    (name (Buffer-menu-sort 2))
    (size (Buffer-menu-sort 3))
    (mode (Buffer-menu-sort 4))
    (file (Buffer-menu-sort 5))
    (t    (Buffer-menu-sort nil))))
(add-hook 'Buffer-menu-mode-hook
	  (lambda ()
	    (define-key Buffer-menu-mode-map (kbd "S") 'buffer-list-sort)))

;; edit-server
(if (locate-library "edit-server")
    (progn
      (require 'edit-server)
      (setq edit-server-new-frame nil)
      (edit-server-start)))

(defun find-first (predicate list)
  "Return result of first item in list which
satisfies predicate.  Returns nil if predicate
is nil for all items in list."
  (catch 'found-it
    (dolist (item list)
      (let ((result (funcall predicate item)))
	(if result
	    (throw 'found-it result))))))

;;;; Set browser to chrome or firefox, in pref order
(let ((browser (find-first #'(lambda (item)
			       (executable-find item))
			   '("iceweasel" "firefox"
			     "chromium-browser" "google-chrome"))))
  (when browser
    (setq browse-url-generic-program browser
	  browse-url-browser-function 'browse-url-generic)))

;;;; Js2-mode for javascript
(setq js2-basic-offset 2)
(setq js2-bounce-indent-p t)

;;;; Add tab completion to shell-command mode
(require 'shell-command)
(shell-command-completion-mode)

(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-h a") 'apropos)
(global-set-key (kbd "C-x ;") 'align-regexp)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x ^") 'join-line)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key [(control tab)] 'other-window) ; make control tab switch buffer windows
(global-set-key [(meta g)] 'goto-line) ; Make meta g to goto-line

;;;; emacs progess meter for mini-buffer example
(defun collect-mana-for-emacs ()
  (interactive)
  (let ((progress-reporter
	 (make-progress-reporter "Collecting mana for Emacs..."
				 0 500)))  
    (dotimes (k 500)
      (sit-for 0.01)
      (progress-reporter-update progress-reporter k))
    (progress-reporter-done progress-reporter)))

(if (boundp 'desktop-save-mode) (desktop-save-mode 1)) ; don't save desktop sessions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'stash)
