;;; async-shell-command-wrapper --- async shell commands are invoked in buffer named after the command

(defun ksm/async-shell-command (command)
  "Run shell COMMAND asynchronously in buffer named after command."
  (interactive
   (list
    (replace-regexp-in-string           ; Trim leading and trailing space from command.
     "^[ \t]+\\|[ \t]+$" ""
     (read-shell-command "Async shell command: "))))
  (when (> (length command) 0)
    (let ((dir default-directory)       ; Grab default-directory from current buffer
          (output-buffer (concat "*Async: " command "*")))
      (switch-to-buffer output-buffer)
      (setq default-directory dir)      ; Make default-directory buffer-local in output-buffer
      (setq list-buffers-directory dir) ; Ensure directory shows up in buffer list
      (async-shell-command command output-buffer))))

(provide 'async-shell-command-wrapper)
