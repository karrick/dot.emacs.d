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

(let ((packages '(
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
		  psgml
		  puppet-mode
		  shell-command
		  smart-tab
		  wgrep
		  wgrep-ack
		  yaml-mode
		  zenburn-theme
		  )))
  (dolist (p packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git configuration
(setenv "GIT_PAGER" "")			; elide git paging capability

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setenv "EDITOR" "emacsclient")
(setenv "VISUAL" "emacsclient")
(setenv "PAGER" "cat")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ansi-color

(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unshift-path (elem)
  (interactive "DAdd what directory to PATH: ")
  (let ((path (expand-file-name elem)))
    (when (string-match "/$" path)
      (setq path (concat (replace-match "" nil nil path))))
    (setenv "PATH" (concat path ":" (getenv "PATH")))
    (message "adding %s to PATH" path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "GOPATH" (expand-file-name "~/src/go"))

;; ________________________________________
;; save new scripts as executable

(add-hook 'after-save-hook
	  #'(lambda ()
	      (when
		  (and
		   (save-excursion
		     (save-restriction
		       (widen)
		       (goto-char (point-min))
		       (save-match-data
			 (looking-at "^#!"))))
		   (not (file-executable-p buffer-file-name)))
		(set-file-modes buffer-file-name
				(logior (file-modes buffer-file-name) #o100))
		(message
		 "Wrote and made executable: %s" buffer-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Advise the shell commands to name the buffer after the command itself

(defadvice async-shell-command (before buffer-named-with-command
				       (command &optional output-buffer error-buffer)
				       activate compile)
  (setq output-buffer (or output-buffer (concat "*Async: " command "*")))
  (let ((dir default-directory))
    (switch-to-buffer output-buffer)
    (setq default-directory dir)))

(defadvice shell-command (before buffer-named-with-command
				 (command &optional output-buffer error-buffer)
				 activate compile)
  (setq output-buffer (or output-buffer (concat "*Shell: " command "*")))
  (let ((dir default-directory))
    (switch-to-buffer output-buffer)
    (setq default-directory dir)))

;;;; auto-complete-mode
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
	     (expand-file-name "~/.ac-dict"))

;;;; ac-common-setup is called by ac-config-default
(defun ac-common-setup ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))
(add-to-list 'ac-modes 'html-mode)
(add-to-list 'ac-modes 'nxml-mode)
(ac-config-default)
(defun enable-auto-complete-mode ()
  (auto-complete-mode 1))
(defun disable-auto-complete-mode ()
  (auto-complete-mode 0))

;;;; edit-server for chromium browsers
(when (and (daemonp) (locate-library "edit-server"))
  (require 'edit-server)
  (setq edit-server-new-frame nil)
  (edit-server-start))

;;;; js2 indent 2 spaces
(set-default 'js2-basic-offset 2)
(set-default 'js2-mirror-mode  nil)
(set-default 'js2-mode-escape-quotes  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown mode

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ruby mode

(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; puppet mode

(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xslt mode

(add-to-list 'auto-mode-alist '("\\.xslt\\'" . nxml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido-mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)	; enable fuzzy matching


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; different way of uniquifying names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p nil)
(setq uniquify-ignore-buffers-re "^\\*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SVN mode
(require 'psvn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)
(setq diff-switches "-u")		; default to unified diffs
(setq dired-listing-switches "-Bhl")
(setq dired-show-ls-switches t)
(setq ediff-diff-options "-w")
(setq make-backup-files nil)		; stop making backup files
(setq compilation-scroll-output 'first-error) ; compilation mode scrolls to first error

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; look and feel

;; Disable bars
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

(if (boundp 'desktop-save-mode)
    (desktop-save-mode 0))		; don't save desktop sessions

;;;; Highlight entire expression within parens
(show-paren-mode t)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face "#1f3f3f")

;;;; Set to a usable font
(add-to-list 'default-frame-alist '(font-backend . "xft"))
;; (add-to-list 'default-frame-alist '(font . "Liberation Mono-12"))
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
(put 'narrow-to-region 'disabled nil)

;; other-window-backwards
(defun other-window-backwards (&optional n)
  "Select Nth previous window"
  (interactive "p")
  (other-window (- (prefix-numeric-value n))))

;;;; key bindings

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-r") 'rgrep)
(global-set-key [(meta !)] 'async-shell-command)
(global-set-key [(meta g)] 'goto-line)
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))
(global-set-key "\C-x\C-n" 'other-window)
(global-set-key "\C-x\C-p" 'other-window-backwards)
(global-set-key "\C-xn" 'other-window)
(global-set-key "\C-xp" 'other-window-backwards)

;;;; set f8 to be recompile, shift-f8 to compile
(global-set-key [f8]   'recompile)
(global-set-key [S-f8] 'compile)

;; Zenburn
(load-theme 'zenburn t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-program-name "ispell"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
