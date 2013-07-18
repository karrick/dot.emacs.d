;;;; Author: Karrick McDermott

;; TODO: git-push: add ability to push to other than origin master

;; TODO: shell-mode-hook to have ^M parsed

;; CODE:

(defun use-current-default-directory (buffer)
  (let ((cwd default-directory))
    (switch-to-buffer buffer)
    (setq default-directory cwd)))

(defun git-status ()
  "Show the result of running `git status' in a buffer"
  (interactive)
  (let ((buffer "*git status*"))
    (use-current-default-directory buffer)
    (delete-region (point-min) (point-max))
    (start-process "git-status" buffer
		   "git" "status")))

(defun git-commit (commit-message)
  "Run `git commit' after prompting for a commit message"
  (interactive "MEnter a commit message: ")
  (let ((buffer "*git commit*"))
    (use-current-default-directory buffer)
    (delete-region (point-min) (point-max))
    (start-process "git-commit" buffer
		   "git" "commit" "-m" commit-message)))

(defun git-add (pathname)
  "Run `git add' after prompting for a filename to add"
  (interactive "fAdd which file: ")
  (let ((buffer "*git add*"))
    (use-current-default-directory buffer)
    (delete-region (point-min) (point-max))
    (start-process "git-add" buffer
		   "git" "add" (expand-file-name pathname))))

(defun git-push ()
  "Run `git push'"
  (interactive)
  (let ((buffer "*git push*"))
    (use-current-default-directory buffer)
    (delete-region (point-min) (point-max))
    (start-process "git-push" buffer
		   "git" "push" "origin" "master")))

(provide 'git)
