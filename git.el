;;;; Author: Karrick McDermott

(defun use-current-default-directory (buffer)
  (let ((cwd default-directory))
    (switch-to-buffer buffer)
    (setq default-directory cwd)))

(defun git-status ()
  (interactive)
  (let ((buffer "*git status*"))
    (use-current-default-directory buffer)
    (delete-region (point-min) (point-max))
    (start-process "git-status" buffer
		   "git" "status")))

(defun git-commit (commit-message)
  (interactive "MEnter a commit message: ")
  (let ((buffer "*git commit*"))
    (use-current-default-directory buffer)
    (delete-region (point-min) (point-max))
    (start-process "git-commit" buffer
		   "git" "commit" "-m" commit-message)))

(defun git-add (pathname)
  (interactive "fAdd which file: ")
  (let ((buffer "*git add*"))
    (use-current-default-directory buffer)
    (delete-region (point-min) (point-max))
    (start-process "git-add" buffer
		   "git" "add" (expand-file-name pathname))))

(defun git-push ()
  (interactive)
  (let ((buffer "*git push*"))
    (use-current-default-directory buffer)
    (delete-region (point-min) (point-max))
    (start-process "git-push" buffer
		   "git" "push" "origin" "master")))
