;;; paths -- set exec-path and PATH environment variable for child processes

;;; Commentary:
;;;
;;; After XDG_DATA_HOME is set, can set PATH environment variable to any of
;;; the directories I typically use, provided that they exist.

;;; Code:

(require 'empty-string)
(require 'path)

;; (env-set-when-null "GOBIN"
;;				   (file-name-concat (getenv "XDG_DATA_HOME") os "bin"))

;; (unless (getenv "GOBIN")
;;   (setenv "GOBIN" (file-name-concat (getenv "XDG_DATA_HOME")

;; (let ((gobin (getenv "GOBIN")))
;;   (when (non-empty-string-or-nil gobin)
;;	   "foo"))

(defun system-name ()
  "Return a string representing the `system-type'."
  (cond
   ((memq system-type '(darwin ms-dos windows-nt cygwin haiku))
	(symbol-name system-type))
   ((eq system-type 'gnu/kfreebsd)
	"freebsd")
   ((eq system-type 'gnu/linux)
	(or
	 ;; Some programs must be compiled for each
	 ;; major EL release version.
	 (and
	  (file-readable-p "/etc/os-release")
	  (with-temp-buffer
		(insert-file-contents "/etc/os-release")
		(goto-char (point-min))
		(when
			(re-search-forward "^PLATFORM_ID=\"platform:\\(.*\\)\"" nil 'NOERROR)
		  (match-string 1))))
	 (when (file-readable-p "/etc/debian-release")
	   "debian")
	 "linux"))
   (t "unknown")))

(let* ((xdg-data-home (getenv "XDG_DATA_HOME"))
	   (os (system-name))
	   (gobin (or
			   (getenv "GOBIN")
			   (let ((gopath (getenv "GOPATH")))
				 (when (non-empty-string-or-nil gopath)
				   (file-name-concat gopath "bin")))))
	   (candidates (list
					"~/.cargo/bin"
					gobin
					"~/bin"
					(file-name-concat xdg-data-home "bin")
					(file-name-concat xdg-data-home os "bin")
					)))
  (dolist (path candidates)
	(when (not (empty-string-p path))
	  (setq path (expand-file-name path))
	  (path-prepend path))))

(provide 'paths)

;;; paths.el ends here
