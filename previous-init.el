;;;; -*- Emacs-Lisp -*-

;;________________________________________________________________
;; helper functions
(defun add-valid-dir-to-list (list dir)
  (if (file-accessible-directory-p dir)
      (add-to-list list dir)))

(defun add-valid-dirs-to-list (list dirs)
  (dolist (dir dirs)
    (add-valid-dir-to-list list dir)))

(defun add-exec-paths (dirs)
  (add-valid-dirs-to-list 'exec-path dirs))

(defun add-load-paths (dirs)
  (add-valid-dirs-to-list 'load-path dirs))

;;______________________________________________________________________
(add-exec-paths '("/opt/CollabNet_Subversion/bin"
		  "~/bin" "~/sbin" "~/scripts"
		  "/opt/local/bin" "/opt/local/sbin"
		  "/usr/local/bin" "/usr/local/sbin"
		  "/blah"))

;;______________________________________________________________________
(add-load-paths '("~/.emacs.d/site-lisp"
	          "~/.emacs.d/site-lisp/ksm"))

;;______________________________________________________________________
;; handy shortcuts

(defun dot-emacs ()
  (interactive)
  (find-file "~/conf/dot.emacs"))

(defun ssh-config ()
  (interactive)
  (find-file "~/.ssh/config")
  (goto-char (point-max)))

(defun gitosis-conf ()
  (interactive)
  (find-file "~/gitosis-admin/gitosis.conf"))

(defun gtd ()
  (interactive)
  (when nil (add-hook ’mail-mode-hook ’turn-on-orgstruct))
  ;; consider adding a git-pull in the directory first
  (find-file "/sshx:karrick.org:/home/karrick/Documents/gtd/gtd.txt"))

(defun scripts ()
  (interactive)
  (find-file "~/scripts"))

(defun sudo-su (command)
  (interactive "sCommand: ")
  (let ((final-command (concat "sudo su -l root -c " (shell-quote-argument command))))
    (async-shell-command-revised final-command)))

;; ________________________________________
;; make meta-@ do an async shell command

(defun async-shell-command-revised (command)
  (interactive "sAsync shell command: ")
  (async-shell-command command
		       (switch-to-buffer-other-window
			(concat "*Async: " command "*"))))

(global-set-key "\M-@" 'async-shell-command-revised)
;; (global-set-key "\M-!" 'shell-command)

;;______________________________________________________________________
;; GLOBALS

(column-number-mode 1)
(if (boundp 'desktop-save-mode)
    (desktop-save-mode 0))		; don't save desktop sessions
(fset 'yes-or-no-p 'y-or-n-p)
(global-font-lock-mode t)
(setq compilation-scroll-output 'first-error)
(setq diff-switches "-u")		; default to unified diffs
(setq dired-listing-switches "-Bhl")
(setq dired-show-ls-switches t)
(setq ediff-diff-options "-w")
(setq inhibit-startup-message t)
(setq scroll-conservatively 5)
(setq scroll-step 1)
(setq visible-bell 1)

;;________________________________________

(when t					; man page needs this
  (add-hook 'man-mode-hook '(lambda ()
			      (setq truncate-lines t))))

;;________________________________________
(add-hook 'rcirc-mode-hook
	  (lambda ()
	    (rcirc-track-minor-mode 1)))

;;________________________________________________________________
;; Highlight entire expression within parens
(show-paren-mode t)
(setq show-paren-style 'expression)

;;________________________________________________________________
;; shell mode hooks

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Discard all control-M characters from the current group of shell output.
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

(add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)

(add-hook 'shell-mode-hook '(lambda () (toggle-truncate-lines 1)))
(setq comint-prompt-read-only t)

(require 'term)
(defun visit-ansi-term ()
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one"
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/bash")
        (anon-term (get-buffer "*ansi-term*")))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (call-interactively 'rename-buffer)
              (if anon-term
                  (switch-to-buffer "*ansi-term*")
                (ansi-term term-cmd)))
          (kill-buffer (buffer-name))
          (ansi-term term-cmd))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (switch-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd)))))

;;________________________________________________________________
(defalias 'qrr 'query-replace-regexp)
(defalias 'sl 'sort-lines)
(defalias 'vfo 'view-file-other-window)

(global-set-key (kbd "C-c a") 'list-matching-lines)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key [(f1)] (lambda () (interactive) (manual-entry (current-word))))
(global-set-key (kbd "<f2>") 'visit-ansi-term)
(global-set-key [(f3)] 'view-mode)
(global-set-key [(f4)] 'view-file-other-window)
(global-set-key [(f5)] 'balance-windows)
(global-set-key [(f6)] 'query-replace-regexp)
(global-set-key [(f9)] 'call-last-kbd-macro)
(global-set-key [M-up] 'transpose-lines)
					; (global-set-key [delete] 'delete-char)

(cond (nil (defalias 'lml 'list-matching-lines))
      (t ;; hack to allow user to click on entry in *Occur* buffer to center line
       (defun lml (arg)
	 (interactive "sList lines regexp: ")
	 (list-matching-lines arg)
	 (recenter-top-bottom))))

;;________________________________________________________________
;; auto-tab (indent or complete based on context of POINT)

(when nil
  (require 'auto-tab)
  (global-set-key "\t" 'auto-tab))

;;________________________________________________________________

;;(require 'redo)
(require 'ssh-remove-host)

;;________________________________________________________________
;; Add tab completion to shell-command mode
(when nil
  (require 'shell-command)
  (shell-command-completion-mode))

;;________________________________________________________________
;; ibuffer -- normally have activated
(when t
  (autoload 'ibuffer "ibuffer" "List buffers." t)
  (cond (nil (global-set-key "\C-x\C-b" 'buffer-menu))
	(t (global-set-key (kbd "C-x C-b") 'ibuffer))))

;;________________________________________
;; ido -- normally have activated
(unwind-protect
    (progn
      (require 'ido)
      (ido-mode t)
      (setq ido-enable-flex-matching t)) ; enable fuzzy matching
  (message "Cannot find ido library."))

;;________________________________________
(when t
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward)
  (setq uniquify-after-kill-buffer-p nil)
  (setq uniquify-ignore-buffers-re "^\\*"))

;; ________________________________________
;; some nice additions

(when nil
  (require 'gem-ksm)
  (require 'git-ksm)
  (setq git-repos-source "gitosis@karrick.org")
  (setq git-ignore-file "~/conf/dot.gitignore")
  (require 'psvn))

;;______________________________________________________________________
;; org-mode settings
(when t
  ;; (require 'remember-autoloads)
  (setq org-remember-templates
	'(("Tasks" ?t "* TODO %?\n  %i\n  %a" "~/Documents/gtd/gtd.txt")
	  ("Appointments" ?a "* Appointment: %?\n%^T\n%i\n  %a" "~/Documents/gtd/gtd.txt")))
  (setq remember-annotation-functions '(org-remember-annotation))
  (setq remember-handler-functions '(org-remember-handler))
  (eval-after-load 'remember
    '(add-hook 'remember-mode-hook 'org-remember-apply-template))
  (global-set-key (kbd "C-c r") 'remember)
  ;; (require 'org-install)
  ;; (require 'org)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  (global-set-key "\C-cl" 'org-store-link)
  (setq org-agenda-include-all-todo t)
  (setq org-agenda-include-diary t)
  (setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE")))

;;______________________________________________________________________
;; Eliminate trailing whitespace
(when t
  (setq nuke-trailing-whitespace-p t)	; on each line
  (setq require-final-newline 'ask)	; for buffer
  (autoload 'nuke-trailing-whitespace "whitespace" nil t)
  (add-hook 'mail-send-hook 'nuke-trailing-whitespace)
  (add-hook 'write-file-hooks 'nuke-trailing-whitespace))

;;______________________________________________________________________
;; paredit
(when nil
  (autoload 'paredit-mode "paredit"
    "Minor mode for pseudo-structurally editing Lisp code." t)
  (add-hook 'lisp-mode-hook (lambda () (paredit-mode +1))))

;;______________________________________________________________________
;; tramp defaults
;; (newer openssh supports multi-channel connections)

(defalias 'clean-tramp 'tramp-cleanup-all-connections)
(setq tramp-default-method "sshx")	; sshx to use simple shell

(when nil
  (defadvice tramp-handle-write-region (after tramp-write-beep-advice activate)
    "Make tramp beep after writing a file."
    (interactive)
    (beep))

  (defadvice tramp-handle-do-copy-or-rename-file (after tramp-copy-beep-advice activate)
    "Make tramp beep after copying a file."
    (interactive)
    (beep))

  (defadvice tramp-handle-insert-file-contents (after tramp-insert-beep-advice activate)
    "Make tramp beep after inserting a file."
    (interactive)
    (beep))
  )

(defun setup-stack-tramp-proxies (stack)
  (interactive "sHostname of stack, no FQDN (example: clay): ")
  (add-to-list 'tramp-default-proxies-alist
	       (list "\\.example\\.com"
		     nil
		     (concat "/ssh:" stack ".skarven.net:"))))

;;______________________________________________________________________
;;;; For work, punch an e3 user proxy hole through a stack's fw
(defun setup-stack-tramp-proxies (stack)
  (interactive "sHostname of stack, NO FQDN (example: clay): ")
  (setq tramp-default-proxies-alist nil)
  (add-to-list 'tramp-default-proxies-alist
	       (list "\\.example\\.com"
		     nil
		     (concat "/ssh:" stack ".skarven.net:"))))

(when nil
  (add-to-list 'tramp-default-proxies-alist
	       '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist
	       '((regexp-quote (system-name)) nil nil))
  (setq tramp-debug-buffer t)
  (setq tramp-verbose 10)
  (add-to-list 'tramp-default-proxies-alist
	       '("\\.skarven\\.net\\'" nil "/sshx:kmcdermott@pri.skarven.net:")))
;;________________________________________

(defun find-alternative-file-with-sudo ()
  (interactive)
  (let ((fname (or buffer-file-name
		   dired-directory)))
    (when fname
      (if (string-match-p "^/sudo:root@localhost:" fname)
	  (setq fname (replace-regexp-in-string
		       "^/sudo:root@localhost:" ""
		       fname))
	(setq fname (concat "/sudo:root@localhost:" fname)))
      (find-alternate-file fname))))
(global-set-key (kbd "C-x C-r") 'find-alternative-file-with-sudo)

;;________________________________________
;; mode-specific configurations

(setq auto-mode-alist (cons '("\\Makefile$" . makefile-mode) auto-mode-alist))

;;________________________________________________________________
;; text mode

(add-hook 'text-mode-hook
          '(lambda ()
	     (turn-on-auto-fill)
	     (auto-fill-mode 1)))

;;________________________________________
;;;;    Programming - Lisp
(when nil
  (add-hook 'emacs-lisp-mode-hook
	    '(lambda ()
	       (modify-syntax-entry ?- "w")))) ; change '-' so not considered a word-delimiter

;; specify modes for lisp file extensions
(setq auto-mode-alist
      (append '((".emacs$" . emacs-lisp-mode)
		("\\.cl$" . lisp-mode)
		("\\.lisp$" . lisp-mode)
		("\\.lsp$" . lisp-mode)
		("\\.sch$" . scheme-mode)
		("\\.scm$" . scheme-mode)
		("\\.ss$" . scheme-mode)
		("\\.system$" . lisp-mode))
	      auto-mode-alist))

;;__________________________________________________________________________
;;;;    Programming - Common Lisp and Slime

(let ((lisp "/usr/bin/sbcl"))
  (when (file-executable-p lisp)
    (setq inferior-lisp-program lisp)
    (let ((slime-helper (expand-file-name "~/quicklisp/slime-helper.el")))
      (if (file-readable-p slime-helper)
	  (load slime-helper)))))

;;______________________________________________________________________
;;;; Programming - Clojure

(when nil
  (catch 'missing-requirement
    (dolist (dir '("~/Downloads/clojure-mode"
		   "~/Downloads/swank-clojure"))
      (if (file-accessible-directory-p dir)
	  (add-load-paths (list dir))
	(throw 'missing-requirement t)))
    (setq swank-clojure-jar-path "~/.clojure")
    (require 'clojure-mode)
    (require 'swank-clojure-autoload)
    (setq swank-clojure-binary "clj")
    (setq auto-mode-alist (cons '("\\.clj$" . clojure-mode) auto-mode-alist))

    (add-to-list 'load-path "~/conf/dot.emacs.d/site-lisp/slime")
    (require 'slime)
    (eval-after-load "slime"
      '(progn (slime-setup '(slime-repl))))
    (slime-setup)
))



;;__________________________________________________________________________
;;;;    Programming - Haskell

(let ((file "/opt/local/share/emacs/site-lisp/haskell-mode-2.4/haskell-site-file"))
  (when (file-readable-p file)
    (load file)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
    (add-hook 'haskell-mode-hook 'font-lock-mode)
    (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)))

;;__________________________________________________________________________
;;;;    Programming - Javascript
(when t
  (add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
  (require 'js2))

;;__________________________________________________________________________
;;;;    Programming - Ruby
(when nil
  (autoload 'ruby-mode "ruby-mode" "Major mode for editing Ruby scripts." t)
  (setq auto-mode-alist (cons '("\\Rakefile$" . ruby-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.rake$" . ruby-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.rhtml$" . html-mode) auto-mode-alist))
  (setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))

  (autoload 'run-ruby "inf-ruby"
    "Run an inferior Ruby process" t)
  (autoload 'inf-ruby-keys "inf-ruby"
    "Set local key defs for inf-ruby in ruby-mode")

  (add-hook 'ruby-mode-hook
	    (lambda()
	      (inf-ruby-keys)
	      (add-hook 'local-write-file-hooks
			'(lambda()
			   (save-excursion
			     (untabify (point-min) (point-max))
			     (delete-trailing-whitespace))))
	      (set (make-local-variable 'indent-tabs-mode) 'nil)
	      (set (make-local-variable 'tab-width) 2)
	      (imenu-add-to-menubar "IMENU")
	      (define-key ruby-mode-map "\C-m" 'newline-and-indent))))

;;________________________________________
;; YAML
(when nil
  (require 'yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook '(lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;________________________________________

(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
	    (lambda ()
	      (if (file-exists-p (concat buffer-file-name "c"))
		  (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)

;;;; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;;;; TESTING AREA
;;;; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

;; Buffer menu mode sort function
;; -- works with buffer menu, not IBuffer
(when t
  (defun buffer-list-sort (column)
    (interactive "SColumn to sort by (one of name,size,mode,file,time [default]): ")
    (case column
      (name (Buffer-menu-sort 2))
      (size (Buffer-menu-sort 3))
      (mode (Buffer-menu-sort 4))
      (file (Buffer-menu-sort 5))
      (t    (Buffer-menu-sort nil))))
  (add-hook 'Buffer-menu-mode-hook
	    (lambda ()
	      (define-key Buffer-menu-mode-map (kbd "S") 'buffer-list-sort))))

;;________________________________________
;; Frame manipulation
;; (defun arrange-frame (w h &optional nosplit)
;;   "Rearrange the current frame to a custom width and height and split unless prefix."
;;   (delete-other-windows)
;;   (if (not nosplit)
;;       (split-window-horizontally))
;;   (let ((frame (selected-frame)))
;;     (when (memq frame '(mac w32 x))
;;       (set-frame-position frame 5 25)
;;       (set-frame-size frame w h))))

;; (defun my-set-mac-font (name size)
;;   "defined in my .emacs file"
;;   (interactive
;;    (list (completing-read "font-name: "
;;    			  (mapcar (lambda (p) (list (car p) (car p)))
;;    				  (cond ((equal 'mac (window-system))
;;    					 (x-font-family-list))
;;    					((equal 'x (window-system))
;;    					 (x-family-fonts)))) nil t)
;;    	 (read-number "size: " 12)))
;;   (set-face-attribute 'default nil
;; 		      :family name
;; 		      :slant  'normal
;; 		      :weight 'normal
;; 		      :width  'normal
;; 		      :height (* 10 size))
;;   (frame-parameter nil 'font))

;; (defun medium (&optional nosplit)
;;   "Create a large window suitable for coding on a macbook."
;;   (interactive "P")
;;   (my-set-mac-font "bitstream vera sans mono" 12)
;;   (arrange-frame 170 45 nosplit))

;; (defun presentation ()
;;   "Create a giant font window suitable for doing live demos."
;;   (interactive)
;;   (my-set-mac-font "bitstream vera sans mono" 24)
;;   (arrange-frame 85 25 t))

;;________________________________________
(when nil		    ; seems to be already working without this
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt))

(require 'enscript)

;;________________________________________
(defun print-to-pdf ()
  (interactive)
  (ps-spool-buffer)			;-with-faces)
  (switch-to-buffer "*PostScript*")
  (write-file "/tmp/tmp.ps")
  (kill-buffer "tmp.ps")
  (shell-command (concat "ps2pdf14 /tmp/tmp.ps '" (buffer-name) ".pdf'"))
  (shell-command "rm /tmp/tmp.ps")
  (message (concat "Saved to: '" (buffer-name) ".pdf'")))

;;________________________________________
;; Add tab completion to shell-command mode
(require 'shell-command)
(shell-command-completion-mode)

;;______________________________________________________________________
;; next window and previous window

(global-set-key "\C-x\C-n" 'other-window)

(defun other-window-backward (&optional n)
  "Select the Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(global-set-key "\C-x\C-p" 'other-window-backward)

;;________________________________________________________________
;; line at a time scrolling

(defalias 'scroll-ahead 'scroll-up)
(defalias 'scroll-behind 'scroll-down)

(defun scroll-n-lines-ahead (&optional n)
  "Scroll ahead N lines (1 by default)."
  (interactive "P")
  (scroll-ahead (prefix-numeric-value n)))

(defun scroll-n-lines-behind (&optional n)
  "Scroll behind N lines (1 by default)."
  (interactive "P")
  (scroll-behind (prefix-numeric-value n)))

;; (global-set-key "\C-q" 'scroll-n-lines-ahead)
;; (global-set-key "\C-z" 'scroll-n-lines-behind)

;;________________________________________________________________
;; other cursor and text motion commands

(defun point-to-top ()
  "Put point on top line of window."
  (interactive)
  (move-to-window-line 0))

(global-set-key "\M-," 'point-to-top)

(defun point-to-bottom ()
  "Put point on bottom line of window."
  (interactive)
  (move-to-window-line -1))

(global-set-key "\M-." 'point-to-bottom)

;;________________________________________________________________
;; advised buffer switching

(defadvice switch-to-buffer (before existing-buffer activate compile)
  "When interactive, switch to existing buffers only,
unless given a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: "
		      (other-buffer)
		      (null current-prefix-arg)))))

(defadvice switch-to-buffer-other-window (before existing-buffer activate compile)
  "When interactive, switch to existing buffers only,
unless given a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: "
		      (other-buffer)
		      (null current-prefix-arg)))))

(defadvice switch-to-buffer-other-frame (before existing-buffer activate compile)
  "When interactive, switch to existing buffers only,
unless given a prefix argument."
  (interactive
   (list (read-buffer "Switch to buffer: "
		      (other-buffer)
		      (null current-prefix-arg)))))

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

;; ________________________________________

(add-to-list 'vc-handled-backends 'Fossil)
(require 'vc-fossil)

;; ________________________________________
;; ui customizations

;; I put these last because when the emacs window has default colors,
;; it's a sure bet that something failed to load in this file.

(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(when (memq window-system '(mac ns w32 x)) ; ns is nextstep, a.k.a. Mac OS X
  (setq frame-title-format (concat "Emacs on \"" system-name "\"")))

(require 'color-theme)
(require 'color-theme-zenburn)
(color-theme-zenburn)
