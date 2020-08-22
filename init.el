;;;; -*- mode: emacs-lisp -*-

;;; Commentary:

;; Minimalistic.

;; Uses built-in emacs package manager to specify which optional packages to
;; install.

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(let ((default-directory (convert-standard-filename (expand-file-name (concat user-emacs-directory "/lisp")))))
  (normal-top-level-add-to-load-path '("."))
  ;; optionally benchmark init process
  (when (require 'benchmark-init-loaddefs nil 'no-error)
    (benchmark-init/activate)))

(require 'require-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCESS ENVIRONMENT

(require 'path)
(let ((directories (list
                    "/usr/local/bin"
                    "~/bin"
                    )))
  (dolist (dir directories)
    (path-prepend dir)))

(setenv "GIT_PAGER" "")                  ; elide git paging capability.
(setenv "PAGER" (executable-find "cat")) ; in lieu of paging files, dump them to a buffer using `cat`.

(when (and (fboundp #'daemonp) (daemonp)) ; when invoked as a daemon
  (cd (expand-file-name "~"))             ; change to home directory at startup
  (server-start)
  (when nil
    ;; edit-server for browsers (install "It's All Text!" on Firefox, or "Edit with Emacs" for Chrome)
    (require-package/with-requirements '(edit-server)
      (edit-server-start))))

;; While this process is running, make certain any sub process knows to use
;; emacsclient as editor and can route file editing requests to this process.
(let ((cmd (executable-find "emacsclient")))
  (when cmd
    (setenv "EDITOR" cmd)
    (setenv "VISUAL" cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONFIGURATION

(when (fboundp #'desktop-save-mode) (desktop-save-mode 0)) ; don't save desktop sessions
(when (fboundp #'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp #'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp #'tool-bar-mode) (tool-bar-mode -1))

(column-number-mode 1)
(line-number-mode 1)
(prefer-coding-system 'utf-8)
(put 'narrow-to-region 'disabled nil)   ; this is such a useful feature
(show-paren-mode t)                     ; parentheses matching

(require 'browser-open)
(require 'eshell)
(require 'find-file-dynamic)
(require 'make-shebang-executable)
(require 'setup-autocomplete)
;; (require 'setup-codesearch)
(require 'uniquify)                     ; uniquify buffer names

;; flycheck is the successor to flymake
(require-package/with-requirements '(flycheck)
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Writable grep buffers via toggling off read-only (similar to wdired mode for dired buffers)
(require-package/with-requirements '(wgrep wgrep-ack)
  (define-key grep-mode-map (kbd "C-x C-q") #'wgrep-change-to-wgrep-mode))

(setq ediff-diff-options "-w"
      ediff-window-setup-function 'ediff-setup-windows-plain ; don't spawn a new frame for the ediff commands, keep it all in one frame
      ediff-split-window-function 'split-window-horizontally) ; have ediff buffers show in a side-by-side view

(require 'clean-and-indent)
(global-set-key (kbd "<f2>") #'clean-and-indent)
(dolist (h '(sh-mode-hook css-mode-hook))
  (add-hook h #'(lambda () (add-hook 'before-save-hook #'clean-and-indent nil t))))

(require 'copy-and-comment)
(global-set-key (kbd "<f3>") #'copy-and-comment)

(require 'async-shell-command-wrapper)
(global-set-key (kbd "M-&") #'ksm/async-shell-command)
(global-set-key (kbd "ESC &") #'ksm/async-shell-command)

(when nil
  (require-package/with-requirements '(expand-region)
    (global-set-key (kbd "M-=") #'er/expand-region)
    (global-set-key (kbd "ESC =") #'er/expand-region)
    (global-set-key (kbd "M--") #'er/contract-region)
    (global-set-key (kbd "ESC -") #'er/contract-region)))

(when nil
  (require-package/with-requirements '(multiple-cursors)
    (global-set-key (kbd "C-S-c C-S-c") #'mc/edit-lines)
    (global-set-key (kbd "C-c C-S-c") #'mc/edit-lines)
    (global-set-key (kbd "C->") #'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this)
    (global-set-key (kbd "C-c C->") #'mc/mark-more-like-this-extended)))

(when t
  ;; (setq org-todo-keywords
  ;;       '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED")))
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture))

(require 'sort-commas)

(when t
  (require 'setup-gtd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VCS

(eval-after-load "vc-hooks" '(define-key vc-prefix-map "=" #'vc-ediff))

;; fossil vc mode
(autoload 'vc-fossil-registered "vc-fossil")
(add-to-list 'vc-handled-backends 'Fossil)

;; svn mode
(autoload 'svn-status "psvn"
  "Examine the status of Subversion working copy in directory DIR.
If ARG is -, allow editing of the parameters. One could add -N to
run svn status non recursively to make it faster.
For every other non nil ARG pass the -u argument to `svn status', which
asks svn to connect to the repository and check to see if there are updates
there.

If there is no .svn directory, examine if there is CVS and run
`cvs-examine'. Otherwise ask if to run `dired'."
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SPELL CHECK
;;;
;;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html

(cond
 ((executable-find "aspell")
  ;; you may also need `ispell-extra-args`
  (setq ispell-program-name "aspell"))

 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")

  ;; Please note that `ispell-local-dictionary` itself will be passed to
  ;; hunspell cli with "-d".  It is also used as the key to lookup
  ;; ispell-local-dictionary-alist if we use a different dictionary.
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))

 (t (setq ispell-program-name nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROGRAMMING LANGUAGE SPECIFIC

(add-hook 'prog-mode-hook #'(lambda ()
                              (setq fill-column 80)
                              (flyspell-prog-mode)
                              (hl-line-mode 1)))

;; tabs and indenting
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'perl-indent-level 'tab-width)

(require 'setup-elisp-mode)
(require 'setup-golang-mode)
(require 'setup-rust-mode)
(require 'setup-javascript-mode)
(require 'setup-python-mode)
(require 'setup-ruby-mode)
;; (require 'setup-zig-mode)

(require-package/with-requirements '(json-mode)
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

(require-package/ensure-require '(
                                  fic-mode
                                  keyword-search
                                  markdown-mode
                                  yaml-mode
                                  ))

;; (add-to-list 'auto-mode-alist '("\\.xslt\\'" . nxml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEY BINDINGS

(global-set-key (kbd "C-c H") #'hl-line-mode)

;; By default bind "C-x C-r" to rgrep, but when deadgrep is installed, rebind to that...
(global-set-key (kbd "C-x C-r") #'rgrep)
(require-package/with-requirements '(deadgrep)
  (if (executable-find "rg")
      (global-set-key (kbd "C-x C-r") #'deadgrep)))

(global-set-key (kbd "<f1>") #'(lambda () (interactive) (revert-buffer nil t nil)))
(global-set-key (kbd "<f4>") #'recompile)
(global-set-key (kbd "<f5>") #'compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WINDOW MANAGEMENT: Mimic tmux commands for sanity, but importantly,
;; to keep ability to use emacs in a tmux frame, you need to use a
;; different key prefix in emacs than tmux.

(global-set-key (kbd "C-x C-b") #'ibuffer)

;; (global-set-key (kbd "C-x &") #'kill-buffer-and-window) ; similar key-binding to tmux
;; (global-set-key (kbd "C-x c") #'shell)                         ; create shell
;; (global-set-key (kbd "C-z") #'delete-other-windows-vertically) ; does not work inside tmux

;; (global-unset-key (kbd "C-x C-c"))      ; disable save-buffers-kill-terminal
(setq confirm-kill-emacs 'yes-or-no-p)

;; (global-unset-key (kbd "C-z"))          ; disable suspend-frame
(global-unset-key (kbd "s-z"))          ; disable abrupt Emacs minimize

(when (eq system-type 'darwin)
  (global-unset-key (kbd "C-z"))   ; disable suspend-frame
  (global-unset-key (kbd "s-p"))   ; disable prompt to print a buffer
  (global-unset-key (kbd "s-q"))   ; disable abrupt Emacs exit
  (global-unset-key (kbd "s-t")))  ; disable ns-popup-font-panel

(require 'ksm-window-scrolling)
(global-set-key (kbd "M-N") #'ksm/forward-line-scroll-up)
(global-set-key (kbd "M-P") #'ksm/previous-line-scroll-down)
(global-set-key (kbd "M-p") #'ksm/see-more-up)
(global-set-key (kbd "M-n") #'ksm/see-more-down)

(require 'ksm-window)
(global-set-key (kbd "C-x j") #'ksm/window-config-restore) ; jump to window configuration from hash
(global-set-key (kbd "C-x p") #'ksm/window-config-save) ; save window configuration to hash
(global-set-key (kbd "C-x w") #'ksm/window-zoom-out) ; pop and restore window configuration from stack
(global-set-key (kbd "C-x z") #'ksm/window-zoom-in) ; push window configuration to stack and delete other windows; similar key-binding to tmux

;; (global-set-key (kbd "C-x o") #'(lambda() (interactive) (message "Use C-x <arrow>")))
(require-package/with-requirements '(switch-window)
  (global-set-key (kbd "C-x q") 'switch-window)) ; like tmux C-z q

(global-set-key (kbd "C-x <up>")    #'windmove-up)    ; move point to buffer above it
(global-set-key (kbd "C-x <down>")  #'windmove-down)  ; move point to buffer below it
(global-set-key (kbd "C-x <right>") #'windmove-right) ; move point to buffer on its right
(global-set-key (kbd "C-x <left>")  #'windmove-left)  ; move point to buffer on its left

(global-set-key (kbd "C-S-i") #'windmove-up)    ; move point to buffer above it
(global-set-key (kbd "C-S-j") #'windmove-left)  ; move point to buffer on its left
(global-set-key (kbd "C-S-k") #'windmove-down)  ; move point to buffer below it
(global-set-key (kbd "C-S-l") #'windmove-right) ; move point to buffer on its right

(global-set-key (kbd "C-S-<up>")    #'enlarge-window)
(global-set-key (kbd "C-S-<down>")  #'shrink-window)
(global-set-key (kbd "C-S-<left>")  #'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") #'enlarge-window-horizontally)

(require-package/with-requirements '(buffer-move)
  (global-set-key (kbd "<C-M-up>")     #'buf-move-up)     ; swap buffer that has point with buffer above it
  (global-set-key (kbd "<C-M-down>")   #'buf-move-down)   ; swap buffer that has point with buffer below it
  (global-set-key (kbd "<C-M-left>")   #'buf-move-left)   ; swap buffer that has point with buffer on its left
  (global-set-key (kbd "<C-M-right>")  #'buf-move-right)) ; swap buffer that has point with buffer on its right

(require-package/with-requirements '(default-text-scale)
  (default-text-scale-mode))

(when (and (display-color-p) (boundp 'eshell-preoutput-filter-functions))

  ;; compilation buffers should convert ANSI sequences to color formatting
  (require 'ansi-color)
  (defun endless/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook
            #'endless/colorize-compilation)

  (require-package/with-requirements '(zenburn-theme)
    (load-theme 'zenburn t))
  (require-package/with-requirements '(xterm-color)
    (progn (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter)
           (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions)))

    (require 'eshell)
    (add-hook 'eshell-mode-hook #'(lambda () (setq xterm-color-preserve-properties t)))
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

    (add-hook 'compilation-start-hook
              #'(lambda (proc)
                  ;; We need to differentiate between compilation-mode buffers
                  ;; and running as part of comint (which at this point we assume
                  ;; has been configured separately for xterm-color)
                  (when (eq (process-filter proc) 'compilation-filter)
                    ;; This is a process associated with a compilation-mode buffer.
                    ;; We may call `xterm-color-filter' before its own filter function.
                    (set-process-filter
                     proc
                     #'(lambda (proc string)
                         (funcall 'compilation-filter proc (xterm-color-filter string)))))))))

(when (display-mouse-p) ; previously used display-graph-p, so this might not work
  ;; iTerm2 mouse support
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

;; (require 'nice-font)                ; nice-font guards with display-multi-font-p

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOCALHOST CONFIGURATION

(if (locate-library "localhost")
    (require 'localhost)
  (message "no localhost file found"))

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(compilation-scroll-output 'first-error)
 '(custom-safe-themes
   '("84890723510d225c45aaff941a7e201606a48b973f0121cb9bcb0b9399be8cba" "cd4d1a0656fee24dc062b997f54d6f9b7da8f6dc8053ac858f15820f9a04a679" "bfdcbf0d33f3376a956707e746d10f3ef2d8d9caa1c214361c9c08f00a1c8409" default))
 '(diff-switches "-u")
 '(dired-listing-switches "-Bhlo")
 '(edit-server-new-frame nil)
 '(eshell-output-filter-functions
   '(eshell-handle-control-codes eshell-watch-for-password-prompt eshell-postoutput-scroll-to-bottom eshell-handle-control-codes eshell-watch-for-password-prompt))
 '(fci-rule-color "#383838")
 '(flycheck-emacs-lisp-load-path 'inherit)
 '(flycheck-global-modes '(not go-mode))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(minibuffer-prompt-properties
   '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(ns-function-modifier 'hyper)
 '(ns-use-srgb-colorspace t)
 '(package-selected-packages
   '(nix-mode buffer-move deadgrep default-text-scale edit-server eglot fic-mode find-file-in-repository flycheck flymake gnu-elpa-keyring-update go-autocomplete go-eldoc go-errcheck go-rename golint js2-mode json-mode keyword-search lsp-mode lsp-ui markdown-mode protobuf-mode rust-mode switch-window wgrep wgrep-ack xterm-color yaml-mode zenburn-theme zig-mode))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(scroll-conservatively 5)
 '(show-paren-style 'expression)
 '(tab-width 4)
 '(uniquify-buffer-name-style 'post-forward nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(visible-bell t)
 '(wgrep-auto-save-buffer t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#DCDCCC" :background "#3F3F3F"))))
 '(hl-line ((((type x ns) (class color) (background dark)) (:background "firebrick4"))))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "DarkOliveGreen" :foreground "gray75"))))
 '(show-paren-match ((((type x ns) (class color) (background light)) (:background "PaleTurquoise1")))))
