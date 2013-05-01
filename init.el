;;;; -*- mode: emacs-lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use package manager

(require 'package)
(add-to-list 'package-archives
	     '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(when (not package-archive-contents)
  (package-refresh-contents))

(package-initialize)

(defvar my-packages '(
		      auto-complete
		      bash-completion
		      edit-server
		      fic-mode
		      find-file-in-repository
		      go-autocomplete
		      go-mode
		      js2-mode
		      json-mode
		      magit
		      markdown-mode
		      maxframe
		      multiple-cursors
		      nxml-mode
		      shell-command
		      smart-tab
		      wgrep
		      wgrep-ack
		      yaml-mode
		      zenburn-theme
		      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Add ido-mode, for buffer-switching only
(require 'ido)
(ido-mode 'buffer)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

;;;; different way of uniquifying names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p nil)
(setq uniquify-ignore-buffers-re "^\\*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SVN mode
(require 'psvn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fset 'yes-or-no-p 'y-or-n-p)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq diff-switches "-u")		; default to unified diffs
(setq dired-listing-switches "-Bhl")
(setq dired-show-ls-switches t)
(setq ediff-diff-options "-w")
(setq make-backup-files nil) ; stop making backup files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell-command / async-shell-command

;;;; Make meta ! always do an async shell command
;;;; async-shell-command copied from new simple.el in emacs repository
;;;; guard with fboundp
(unless (fboundp 'async-shell-command)
  (defun async-shell-command (command &optional output-buffer error-buffer)
    "Execute string COMMAND asynchronously in background.

Like `shell-command' but if COMMAND doesn't end in ampersand, adds `&'
surrounded by whitespace and executes the command asynchronously.
The output appears in the buffer `*Async Shell Command*'."
    (interactive
     (list
      (read-shell-command "Async shell command: " nil nil
			  (and buffer-file-name
			       (file-relative-name buffer-file-name)))
      current-prefix-arg
      shell-command-default-error-buffer))
    (unless (string-match "&[ \t]*\\'" command)
      (setq command (concat command " &")))
    (shell-command command output-buffer error-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; look and feel

;; Disable bars
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

;;;; Highlight entire expression within parens
(show-paren-mode t)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face "#1f3f3f")

;;;; Set to a usable font
(add-to-list 'default-frame-alist '(font-backend . "xft"))
(add-to-list 'default-frame-alist '(font . "Liberation Mono-12"))
(setq font-use-system-font t)

;;;; Don't spawn a new frame for the ediff commands, keep it all in one frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;;; Have ediff buffers show in a side-by-side view
(setq ediff-split-window-function 'split-window-horizontally)

;;;; Add line and column numbers to the modeline
(line-number-mode 1)
(column-number-mode 1)
(setq scroll-conservatively 5)
(setq scroll-step 1)
(setq visible-bell 1)
(setq inhibit-startup-message t)
(ansi-color-for-comint-mode-on) ; Allow terminal colorization

;;;; key bindings

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-r") 'rgrep)
(global-set-key (kbd "C-x f") 'find-file-in-repository)
(global-set-key [(meta g)] 'goto-line)
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))

;; Remap shortcuts to use async-shell-command by default
(global-set-key [(meta !)] 'async-shell-command)
(global-set-key [(control meta !)] 'shell-command)

;; Zenburn
(load-theme 'zenburn t)

;; (add-hook 'window-setup-hook 'maximize-frame t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
