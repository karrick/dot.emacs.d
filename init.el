;;; package --- Summary: Emacs Initialization -*- mode: emacs-lisp -*-

;;; Commentary:

;; To resolve what might be considered a chicken-and-egg situation in terms of
;; settings variables in `custom-set-variables', which include setting
;; `package-selected-packages' and themes, this registers a single callback to
;; be invoked by `after-init-hook' to run all of the Lisp customization needed
;; after the various variables have been configured.

;;; Code:

;;; When "cert" file in user-emacs-directory, presumably placed there as a
;;; symbolic link to a host-specific yet non-standard system cert file, then
;;; configure gnutls to trust it, before we attempt to contact package-archives
;;; from which packages would be downloaded.
(if (not (gnutls-available-p))
	(message "GNU TLS is not available.")
  (with-eval-after-load 'gnutls
	(let ((cert (expand-file-name "cert" user-emacs-directory)))
	  (when (file-readable-p cert)
		(add-to-list 'gnutls-trustfiles cert)))))

;;; Configure Emacs to run following initialization code after it completes
;;; early initialization.
(add-hook 'after-init-hook
		  #'(lambda ()
			  (package-install-selected-packages)

			  ;; Make Elisp files in ~/.emacs.d/lisp directory available
			  ;; before we reference anything in the lisp directory.
			  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

			  ;;
			  ;; PROCESS ENVIRONMENT
			  ;;

			  ;; When running in daemon mode, change process directory to user
			  ;; home directory.
			  (if (daemonp) (cd (expand-file-name "~")))

			  ;; To prioritize access latency over availability, ensure that
			  ;; highly ephemeral cache data is stored on local machine rather
			  ;; than a home directory that is potentially mounted over a
			  ;; network. However, do place all cache files in a directory
			  ;; that makes it trivial to identify the owner and optionally
			  ;; remove all cache data.
			  (setenv "TMPDIR"
					  (let* ((logname (getenv "LOGNAME"))
							 (tmpdir (getenv "TMPDIR")))
						(cond
						 ((or (null tmpdir) (string-equal tmpdir ""))
						  (file-name-concat "/" "var" "tmp" logname))
						 ((string-suffix-p logname tmpdir)
						  tmpdir)
						 (t
						  (file-name-concat tmpdir logname)))))

			  ;; https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
			  (require 'env-set-when-null)
			  (env-set-when-null-verbose "XDG_CACHE_HOME" (getenv "TMPDIR"))
			  (env-set-when-null-verbose "XDG_CONFIG_HOME" (expand-file-name "~/.config"))
			  (env-set-when-null-verbose "XDG_DATA_HOME" (expand-file-name "~/.local/share"))
			  (env-set-when-null-verbose "XDG_STATE_HOME" (expand-file-name "~/.local/state"))
			  (env-set-when-null-verbose "GOCACHE" (file-name-concat (getenv "XDG_CACHE_HOME") "go-build"))
			  (env-set-when-null-verbose "GOTMPDIR" (file-name-concat (getenv "XDG_CACHE_HOME") "go-tmp"))
			  ;; (env-set-when-null "GOBIN" (file-name-concat (getenv "XDG_DATA_HOME") os "bin"))

			  ;; After XDG_DATA_HOME is set, can set PATH environment variable
			  ;; to any of the directories I typically use, provided that they
			  ;; exist.
			  (require 'paths)

			  (let* ((state (getenv "XDG_STATE_HOME"))
					 (history (file-name-concat state "history"))
					 (emacs (file-name-concat history "emacs")))
				(when (and state (file-directory-p history))
				  (setenv "HISTFILE" emacs)))

			  (require 'hrg)

			  ;; On machines that do not have GNU version of `ls(1)` command,
			  ;; use substitute written in Elisp.
			  (unless (memq system-type '(gnu gnu/linux gnu/kfreebsd))
				(require 'ls-lisp)
				(setq ls-lisp-use-insert-directory-program nil))

			  ;; On Windows prefer using `plink` program for TRAMP
			  ;; connections.
			  (when (and (eq window-system 'w32) (executable-find "plink"))
				(require 'tramp)
				(setq tramp-default-method "plink"))

			  (setenv "GIT_PAGER" "")                  ; elide git paging capability.
			  (setenv "PAGER" (executable-find "cat")) ; in lieu of paging files, dump them to a buffer using `cat(1)`.

			  ;; Make certain any child process knows to use `emacsclient(1)`
			  ;; as editor and can route file editing requests to this
			  ;; process.
			  (let ((cmd (executable-find "emacsclient")))
				(when cmd
				  (setenv "EDITOR" cmd)
				  (setenv "VISUAL" cmd)))

			  ;; (desktop-save-mode 0)
			  ;; (fido-mode 1)
			  (ido-mode 1)
			  (fset 'yes-or-no-p 'y-or-n-p)
			  (prefer-coding-system 'utf-8)
			  (put 'narrow-to-region 'disabled nil)
			  (when (fboundp 'which-key-mode) (which-key-mode))

			  ;;
			  ;; xterm-color is superior to ansi-color
			  ;;

			  ;; compilation buffers
			  (defun my/advice-compilation-filter (f proc string)
				"Transform ANSI sequences in string to Emacs face."
				(funcall f proc (xterm-color-filter string)))
			  (advice-add 'compilation-filter :around #'my/advice-compilation-filter)

			  ;; shell mode
			  (setq comint-output-filter-functions
					(remove 'ansi-color-process-output comint-output-filter-functions))
			  (add-hook 'shell-mode-hook
						#'(lambda ()
							;; Disable font-locking in this buffer to improve
							;; performance
							(font-lock-mode 0)
							;; Prevent font-locking from being re-enabled in
							;; this buffer
							(make-local-variable 'font-lock-function)
							(setq font-lock-function (lambda (_) nil))
							;; Add xterm-color hook
							(add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

			  ;; eshell mode
			  (with-eval-after-load 'esh-mode
				(add-hook 'eshell-before-prompt-hook
						  #'(lambda ()
							  (setq xterm-color-preserve-properties t)))
				(add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
				(setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
				(setenv "TERM" "xterm-256color"))

			  ;;
			  ;; VCS
			  ;;
			  (with-eval-after-load 'vc-hooks
				(define-key vc-prefix-map "=" #'vc-ediff))

			  ;; fossil
			  (autoload 'vc-fossil-registered "vc-fossil")
			  (add-to-list 'vc-handled-backends 'Fossil)

			  ;; svn
			  (autoload 'svn-status "psvn"
				"Examine the status of Subversion working copy in directory DIR.
If ARG is -, allow editing of the parameters. One could add -N to
run svn status non recursively to make it faster.  For every
other non nil ARG pass the -u argument to `svn status', which
asks svn to connect to the repository and check to see if there
are updates there.

If there is no .svn directory, examine if there is CVS and run
`cvs-examine'. Otherwise ask if to run `dired'."
				t)

			  ;;
			  ;; Miscellaneous
			  ;;

			  (require 'unfill-paragraph)
			  (define-key global-map (kbd "M-Q") 'unfill-paragraph)

			  (require 'async-shell-command-wrapper)
			  (global-set-key (kbd "M-&") #'ksm/async-shell-command)
			  (global-set-key (kbd "ESC &") #'ksm/async-shell-command)

			  (require 'align)
			  (require 'browser-open)
			  (require 'clean-and-indent)
			  (require 'copy-and-comment)
			  (require 'find-file-dynamic)
			  (require 'make-shebang-executable)
			  (require 'setup-aspell)
			  (require 'setup-gtd)
			  (require 'sort-commas)

			  ;;
			  ;; PROGRAMMING
			  ;;

			  ;; Tabs and indentation.
			  ;; (defvaralias 'c-basic-offset 'tab-width)
			  (defvaralias 'cperl-indent-level 'tab-width)
			  (defvaralias 'perl-indent-level 'tab-width)
			  (defvaralias 'yaml-indent-level 'tab-width)

			  (add-hook 'prog-mode-hook #'(lambda () (hl-line-mode 1)))
			  (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)

			  ;; tree-sitter is not yet configured properly.
			  (when nil
				(require 'tree-sitter)
				(require 'tree-sitter-langs)
				(require 'tree-sitter-indent)
				(global-tree-sitter-mode)
				(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
				(add-hook 'tree-sitter-after-on-hook #'tree-sitter-indent-mode)
				(progn
				  (require 'tree-sitter-ispell)
				  (cond
				   (nil (global-set-key (kbd "C-x C-s") #'tree-sitter-ispell-run-at-point))
				   (nil (global-set-key (kbd "C-x C-s") #'tree-sitter-ispell-run-buffer)))))

			  ;; Empirically discovered that lsp-keymap-prefix must be set
			  ;; before loading lsp-mode.
			  (setq lsp-keymap-prefix "C-c l")

			  (with-eval-after-load 'lsp-mode
				;; (global-set-key (kbd "C-x 4 M-.") #'xref-find-definitions-other-window)
				(setq read-process-output-max (* 4 1024 1024) ;; 4 MiB to handle larger payloads from LISP.
					  gc-cons-threshold 1000000) ;; 1 million
				(lsp-enable-which-key-integration t))

			  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
			  (add-hook 'markdown-mode-hook #'visual-line-mode)

			  (require 'setup-elisp-mode)
			  (require 'setup-golang-mode)
			  (require 'setup-javascript-mode)
			  (require 'setup-org-mode)
			  (require 'setup-python-mode)
			  (require 'setup-ruby-mode)
			  (require 'setup-rust-mode)
			  (require 'setup-zig-mode)

			  (defun aj-toggle-fold ()
				"Toggle fold all lines larger than indentation on current line"
				(interactive)
				(let ((col 1))
				  (save-excursion
					(back-to-indentation)
					(setq col (+ 1 (current-column)))
					(set-selective-display
					 (if selective-display nil (or col 1))))))

			  (global-set-key (kbd "C-x $") #'aj-toggle-fold)

			  ;;
			  ;; KEY BINDINGS
			  ;;

			  (global-unset-key (kbd "C-z")) ; disable suspend-frame
			  (global-unset-key (kbd "s-p")) ; disable prompt to print a buffer
			  (global-unset-key (kbd "s-q")) ; disable abrupt Emacs exit
			  (global-unset-key (kbd "s-t")) ; disable ns-popup-font-panel
			  (global-unset-key (kbd "s-z")) ; disable minimize

			  (require 'compile)
			  (global-set-key (kbd "<f4>")  #'recompile)
			  (global-set-key (kbd "<f5>")  #'compile)

			  (global-set-key (kbd "<f6>")  #'delete-indentation)
			  (global-set-key (kbd "<f7>")  #'async-shell-command)
			  (global-set-key (kbd "<f8>")  #'copy-and-comment)
			  (global-set-key (kbd "<f9>")  #'clean-and-indent)
			  (global-set-key (kbd "<f10>") #'revert-buffer)

			  (require 'ksm-window-scrolling)
			  (global-set-key (kbd "M-N") #'ksm/forward-line-scroll-up)
			  (global-set-key (kbd "M-P") #'ksm/previous-line-scroll-down)
			  (global-set-key (kbd "M-p") #'scroll-down-line)
			  (global-set-key (kbd "M-n") #'scroll-up-line)

			  ;; By default bind "C-x C-r" to rgrep, but when ripgrep and
			  ;; deadgrep are available, rebind to the latter to use the
			  ;; former...
			  (let ((cmd (executable-find "rg")))
				(if (not cmd)
					(global-set-key (kbd "C-x C-r") #'rgrep)
				  (require 'deadgrep)
				  (setq deadgrep-executable cmd)
				  (global-set-key (kbd "C-x C-r") #'deadgrep)))

			  ;; (define-key grep-mode-map (kbd "C-x C-q") #'wgrep-change-to-wgrep-mode)

			  ;; WINDOW MANAGEMENT: Mimic tmux commands for sanity, but
			  ;; importantly, to keep ability to use emacs in a tmux frame,
			  ;; you need to use a different key prefix in emacs than tmux.
			  ;;
			  ;; REQUIREMENTS:
			  ;;
			  ;;   1. Fluidly change which window is current. Preferably hold
			  ;;   down one or more modifier keys and press cursor direction.
			  ;;
			  ;;   2. Fluidly swap current buffer with an adjacent buffer,
			  ;;   keeping the active buffer active. Preferably hold down one
			  ;;   or more modifier keys and press cursor direction.
			  ;;
			  ;;   3. Temporarily work on one buffer, then restore balanced
			  ;;   buffer configuration. (Bind #'maximize-window)

			  (default-text-scale-mode)
			  (global-set-key (kbd "C-x C-b") #'ibuffer)

			  (require 'ksm-window)
			  (global-set-key (kbd "C-x j") #'ksm/window-config-restore) ; jump to window configuration from hash
			  (global-set-key (kbd "C-x p") #'ksm/window-config-save) ; save window configuration to hash

			  (global-set-key (kbd "C-x 0")  #'ksm/delete-window) ; extension to existing behavior
			  (global-set-key (kbd "C-x 1")  #'ksm/delete-other-windows) ; extension to existing behavior
			  ;; (global-set-key (kbd "C-x 2")  #'split-window-below) ; this is the default key binding
			  ;; (global-set-key (kbd "C-x 3")  #'split-window-right) ; this is the default key binding
			  (global-set-key (kbd "C-x -")  #'ksm/window-zoom-out) ; pop and restore window configuration from stack
			  (global-set-key (kbd "C-x +")  #'ksm/window-zoom-in) ; push window configuration to stack and delete other windows
			  (global-set-key (kbd "C-x =")  #'balance-windows)

			  (global-set-key (kbd "M-I")    #'windmove-up) ; move point to buffer above it
			  (global-set-key (kbd "M-K")  #'windmove-down) ; move point to buffer below it
			  (global-set-key (kbd "M-L") #'windmove-right) ; move point to buffer on its right
			  (global-set-key (kbd "M-J")  #'windmove-left) ; move point to buffer on its left

			  (global-set-key (kbd "C-x 4 i")     #'buf-move-up) ; swap buffer that has point with buffer above it
			  (global-set-key (kbd "C-x 4 k")   #'buf-move-down) ; swap buffer that has point with buffer below it
			  (global-set-key (kbd "C-x 4 j")   #'buf-move-left) ; swap buffer that has point with buffer on its left
			  (global-set-key (kbd "C-x 4 l")  #'buf-move-right) ; swap buffer that has point with buffer on its right

			  (global-set-key "\C-x\C-n" 'other-window)
			  (global-set-key "\C-x\C-p" 'other-window-backward)

			  ;; (global-set-key (kbd "M-n") 'scroll-n-lines-forward)
			  (global-set-key "\M-n" 'scroll-n-lines-forward)
			  (global-set-key "\M-p" 'scroll-n-lines-backward)

			  ;; (global-set-key (kbd "C-x 1") 'switch-window-then-maximize) ; like tmux C-z 1, but without the ability to toggle
			  ;; (global-set-key (kbd "C-x \"") 'switch-window-then-split-below) ; like tmux C-z "
			  ;; (global-set-key (kbd "C-x %") 'switch-window-then-split-right) ; like tmux C-z %
			  ;; (global-set-key (kbd "C-x 0") 'switch-window-then-delete)

			  ;; (global-set-key (kbd "C-x 4 0") 'switch-window-then-kill-buffer)
			  ;; (global-set-key (kbd "C-x 4 d") 'switch-window-then-dired)
			  ;; (global-set-key (kbd "C-x 4 f") 'switch-window-then-find-file)
			  ;; (global-set-key (kbd "C-x 4 m") 'switch-window-then-compose-mail)
			  ;; (global-set-key (kbd "C-x 4 r") 'switch-window-then-find-file-read-only)
			  ;; (global-set-key (kbd "C-x 4 s") 'switch-window-then-swap-buffer)

			  ;; (global-set-key (kbd "C-x 4 C-f") 'switch-window-then-find-file)
			  ;; (global-set-key (kbd "C-x 4 C-o") 'switch-window-then-display-buffer)
			  (global-set-key (kbd "C-x q") 'switch-window) ; like tmux C-z q, but only shows numbers to select when more than two windows

			  ;;
			  ;; TO ORGANIZE
			  ;;

			  (setq frame-title-format '("%b@" (:eval (or (file-remote-p default-directory 'host) system-name)) " - Emacs"))

			  (when nil
				(require 'expand-region)
				(global-set-key (kbd "M-=") #'er/expand-region)
				(global-set-key (kbd "ESC =") #'er/expand-region)
				(global-set-key (kbd "M--") #'er/contract-region)
				(global-set-key (kbd "ESC -") #'er/contract-region))

			  (when nil
				(require 'multiple-cursors)
				(global-set-key (kbd "C-S-c C-S-c") #'mc/edit-lines)
				(global-set-key (kbd "C-c C-S-c") #'mc/edit-lines)
				(global-set-key (kbd "C->") #'mc/mark-next-like-this)
				(global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
				(global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this)
				(global-set-key (kbd "C-c C->") #'mc/mark-more-like-this-extended))

			  (if (and (fboundp 'native-comp-available-p)
					   (native-comp-available-p))
				  (message "Native compilation is available")
				(message "Native complation is *not* available"))

			  (if (and (functionp 'json-serialize) (json-serialize nil))
				  (message "Native JSON is available")
				(message "Native JSON is *not* available"))

			  ;;
			  ;; LOCALHOST CONFIGURATION
			  ;;

			  (if (locate-library "localhost")
				  (require 'localhost)
				(message "no localhost file found"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(artist-text-renderer-function #'(lambda (someText) someText))
 '(column-number-mode t)
 '(compilation-environment '("TERM=xterm-256color"))
 '(compilation-scroll-output 'first-error)
 '(confirm-kill-emacs 'yes-or-no-p)
 '(custom-enabled-themes '(zenburn))
 '(custom-safe-themes
   '("5d966953e653a8583b3ad630d15f8935e9077f7fbfac456cb638eedad62f4480" "2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" "a3e99dbdaa138996bb0c9c806bc3c3c6b4fd61d6973b946d750b555af8b7555b" "fc48cc3bb3c90f7761adf65858921ba3aedba1b223755b5924398c666e78af8b" "70cfdd2e7beaf492d84dfd5f1955ca358afb0a279df6bd03240c2ce74a578e9e" "9040edb21d65cef8a4a4763944304c1a6655e85aabb6e164db6d5ba4fc494a04" "b77a00d5be78f21e46c80ce450e5821bdc4368abf4ffe2b77c5a66de1b648f10" "78e9a3e1c519656654044aeb25acb8bec02579508c145b6db158d2cfad87c44e" default))
 '(default-text-scale-amount 20)
 '(diff-switches "-u")
 '(dired-auto-revert-buffer t)
 '(dired-listing-switches "-AbFhl")
 '(ediff-diff-options "-w")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(fancy-splash-image "")
 '(fill-column 78)
 '(global-flycheck-mode t)
 '(highlight-indent-guides-method 'character)
 '(inhibit-startup-screen t)
 '(line-number-mode t)
 '(lsp-enable-snippet nil)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(minibuffer-prompt-properties
   '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
 '(native-comp-async-report-warnings-errors 'silent)
 '(ns-function-modifier 'hyper)
 '(ns-use-srgb-colorspace t)
 '(org-startup-folded 'show2levels)
 '(package-archive-priorities '(("melpa-stable" . 2) ("melpa" . 1) ("gnu" . 0)))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
	 ("melpa-stable" . "https://stable.melpa.org/packages/")
	 ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(buffer-move company deadgrep default-text-scale fic-mode find-file-in-repository flycheck gnu-elpa-keyring-update go-mode highlight-indent-guides jenkinsfile-mode js2-mode json-mode just-mode lsp-mode lsp-pyright lsp-ui markdown-mode nginx-mode nov puppet-mode rust-mode rustic switch-window tree-sitter tree-sitter-indent tree-sitter-ispell tree-sitter-langs vc-fossil vterm which-key xterm-color yaml-mode zenburn-theme zig-mode))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 5)
 '(show-paren-mode t)
 '(show-paren-style 'expression)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(tsc-dyn-get-from '(:compilation))
 '(uniquify-buffer-name-style 'post-forward nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(visible-bell t)
 '(vterm-buffer-name-string "*vterm*|%s")
 '(vterm-copy-exclude-prompt nil)
 '(wgrep-auto-save-buffer t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit fixed-pitch :background "DarkOliveGreen" :foreground "gray75")))))
