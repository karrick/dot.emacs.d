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

(defun git-log ()
  "Show the result of running `git log' in a buffer"
  (interactive)
  (let ((buffer "*git log*"))
    (use-current-default-directory buffer)
    (delete-region (point-min) (point-max))
    (start-process "git-log" buffer
		   "git" "log")))

(defun git-diff ()
  "Run `git diff -w'"
  (interactive)
  (let ((buffer "*git diff*"))
    (use-current-default-directory buffer)
    (delete-region (point-min) (point-max))
    (start-process "git-diff" buffer
		   "git" "diff" "-w")))

(defun git-add ()
  "Run `git add', prompting for a filename to add if not editing a file"
  (interactive)
  (let ((pathname (buffer-file-name)))
    (when (null pathname)
      (setq pathname (read-file-name "Add which file: ")))
    (save-buffer)
    (let ((buffer "*git add*"))
      (use-current-default-directory buffer)
      (delete-region (point-min) (point-max))
      (start-process "git-add" buffer
		     "git" "add" (expand-file-name pathname)))
    (message "Adding: %s" pathname)))

(defun git-commit (commit-message)
  "Run `git commit' after prompting for a commit message"
  (interactive "MEnter a commit message: ")
  (let ((buffer "*git commit*"))
    (use-current-default-directory buffer)
    (delete-region (point-min) (point-max))
    (start-process "git-commit" buffer
		   "git" "commit" "-m" commit-message)
    (message "Committing: %s" commit-message)))

(defun git-pull ()
  "Run `git pull'"
  (interactive)
  (let ((buffer "*git pull*"))
    (use-current-default-directory buffer)
    (delete-region (point-min) (point-max))
    (start-process "git-pull" buffer
		   "git" "pull")
    (message "Pulling")))g

(defun git-push ()
  "Run `git push'"
  (interactive)
  (let ((buffer "*git push*"))
    (use-current-default-directory buffer)
    (delete-region (point-min) (point-max))
    (start-process "git-push" buffer
		   "git" "push")
    (message "Pushing")))

(provide 'git)
