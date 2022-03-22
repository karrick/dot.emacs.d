;;;; -*- mode: emacs-lisp -*-

;;; Commentary:

;; Minimalistic.

;; Uses built-in Emacs package manager to specify which optional
;; packages to install.

;;; Code:

;; Added by Package.el. This must come before configurations of
;; installed packages. Don't delete this line. If you don't want it,
;; just comment it out by adding a semicolon to the start of the
;; line. You may delete these explanatory comments.

(add-to-list 'load-path (directory-file-name (convert-standard-filename (expand-file-name (concat user-emacs-directory "/lisp")))))
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

(let ((dir (file-name-as-directory (expand-file-name ".history" "~"))))
  (when (file-directory-p dir)
    (setenv "HISTFILE" (concat dir "emacs"))))

(setenv "GIT_PAGER" "")                  ; elide git paging capability.
(setenv "PAGER" (executable-find "cat")) ; in lieu of paging files, dump them to a buffer using `cat`.

(when (daemonp)
  ;; When Emacs is running as a daemon, make certain any sub process
  ;; knows to use emacsclient as editor and can route file editing
  ;; requests to this process.
  (cd (expand-file-name "~"))
  (let ((cmd (executable-find "emacsclient")))
    (when cmd
      (setenv "EDITOR" cmd)
      (setenv "VISUAL" cmd))))

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

(require-package/with-requirements '(which-key)
  (which-key-mode))

;; flycheck is the successor to flymake
(require-package/with-requirements '(flycheck)
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Writable grep buffers via toggling off read-only (similar to wdired mode for dired buffers)
(require-package/with-requirements '(wgrep wgrep-ack)
  (define-key grep-mode-map (kbd "C-x C-q") #'wgrep-change-to-wgrep-mode))

(setq ediff-window-setup-function 'ediff-setup-windows-plain ; don't spawn a new frame for the ediff commands; keep it all in one frame
      ediff-split-window-function 'split-window-horizontally ; have ediff buffers show in a side-by-side view
      ediff-diff-options "-w")

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

(require 'setup-gtd)
(require 'sort-commas)

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

(let ((cmd (executable-find "aspell")))
  (if (not (stringp cmd))
      (message "Cannot find spelling program: consider installing aspell and en-aspell packages.")
    (setq ispell-program-name cmd)
    ;; NOTE: ispell-extra-args contains actual parameters that will be
    ;; passed to aspell.
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROGRAMMING LANGUAGE SPECIFIC

;; tabs and indenting
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'perl-indent-level 'tab-width)

(add-hook 'prog-mode-hook
          #'(lambda ()
              (setq fill-column 70)
              (hl-line-mode 1)))

(setq lsp-keymap-prefix "C-c l")
(require-package/with-requirements '(lsp-mode)
  (lsp-enable-which-key-integration t))

(require-package/with-requirements '(json-mode)
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

(require-package/with-requirements '(markdown-mode)
  (add-hook 'markdown-mode-hook #'visual-line-mode))

(require-package/ensure-require '(
                                  fic-mode
                                  yaml-mode
                                  ))

(require 'setup-elisp-mode)
(require 'setup-golang-mode)
(require 'setup-javascript-mode)
(require 'setup-python-mode)
(require 'setup-ruby-mode)
(require 'setup-rust-mode)
(require 'setup-zig-mode)

;; (add-to-list 'auto-mode-alist '("\\.xslt\\'" . nxml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEY BINDINGS

(require-package/with-requirements '(which-key)
  (which-key-mode))

;; (global-unset-key (kbd "C-x C-c"))      ; disable save-buffers-kill-terminal
(setq confirm-kill-emacs 'yes-or-no-p)

(global-set-key (kbd "C-c H") #'hl-line-mode)

;; By default bind "C-x C-r" to rgrep, but when ripgrep and deadgrep
;; are available, rebind to that...
(if (executable-find "rg")
    (require-package/with-requirements '(deadgrep)
      (global-set-key (kbd "C-x C-r") #'deadgrep))
  (global-set-key (kbd "C-x C-r") #'rgrep))

(global-set-key (kbd "<f1>") #'(lambda () (interactive) (revert-buffer nil t nil)))
(global-set-key (kbd "<f4>") #'recompile)
(global-set-key (kbd "<f5>") #'compile)

(require 'ksm-window-scrolling)
(global-set-key (kbd "M-N") #'ksm/forward-line-scroll-up)
(global-set-key (kbd "M-P") #'ksm/previous-line-scroll-down)
(global-set-key (kbd "M-p") #'scroll-down-line)
(global-set-key (kbd "M-n") #'scroll-up-line)

(global-unset-key (kbd "C-z"))         ; disable suspend-frame
(global-unset-key (kbd "s-z"))         ; disable minimize

(when (eq system-type 'darwin) ; TODO: should also be anything without GNU...
  (progn ; using progn here to merely group the following two items as a chunk
    (setq ls-lisp-use-insert-directory-program nil)
    (require 'ls-lisp))
  (progn ; the following prefixes begin with Super modifier, which is the Command key on Apple devices.
    (global-unset-key (kbd "s-p"))  ; disable prompt to print a buffer
    (global-unset-key (kbd "s-q"))  ; disable abrupt Emacs exit
    (global-unset-key (kbd "s-t"))))    ; disable ns-popup-font-panel

(global-unset-key (kbd "C-z"))          ; disable suspend-frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WINDOW MANAGEMENT: Mimic tmux commands for sanity, but importantly,
;; to keep ability to use emacs in a tmux frame, you need to use a
;; different key prefix in emacs than tmux.

;; REQUIREMENTS:
;;
;;   1. Fluidly change which window is current. Preferably hold down
;;   one or more modifier keys and press cursor direction.
;;
;;   2. Fluidly swap current buffer with an adjacent buffer, keeping
;;   the active buffer active. Preferably hold down one or more
;;   modifier keys and press cursor direction.
;;
;;   3. Temporarily work on one buffer, then restore balanced buffer
;;   configuration. (Bind #'maximize-window)

(global-set-key (kbd "C-x C-b") #'ibuffer)

;; As an alternative to ksm/window-*, consider M-x winner-mode, then
;; "C-c left" to undo most recent window arrangement change; or, "C-c
;; right" to redo an undone window arrangement change.
(require 'ksm-window)
(global-set-key (kbd "C-x j") #'ksm/window-config-restore) ; jump to window configuration from hash
(global-set-key (kbd "C-x p") #'ksm/window-config-save) ; save window configuration to hash

(require 'windmove)
(global-set-key (kbd "C-<up>")    #'windmove-up) ; move point to buffer above it
(global-set-key (kbd "C-<down>")  #'windmove-down) ; move point to buffer below it
(global-set-key (kbd "C-<right>") #'windmove-right) ; move point to buffer on its right
(global-set-key (kbd "C-<left>")  #'windmove-left) ; move point to buffer on its left

(global-set-key (kbd "C-x 4 i")    #'windmove-up) ; move point to buffer above it
(global-set-key (kbd "C-x 4 k")  #'windmove-down) ; move point to buffer below it
(global-set-key (kbd "C-x 4 l") #'windmove-right) ; move point to buffer on its right
(global-set-key (kbd "C-x 4 j")  #'windmove-left) ; move point to buffer on its left

(require-package/with-requirements '(buffer-move)
  (global-set-key (kbd "C-x <up>")     #'buf-move-up) ; swap buffer that has point with buffer above it
  (global-set-key (kbd "C-x <down>")   #'buf-move-down) ; swap buffer that has point with buffer below it
  (global-set-key (kbd "C-x <left>")   #'buf-move-left) ; swap buffer that has point with buffer on its left
  (global-set-key (kbd "C-x <right>")  #'buf-move-right)) ; swap buffer that has point with buffer on its right

(global-set-key (kbd "C-x 0")  #'ksm/delete-window)
(global-set-key (kbd "C-x 1")  #'ksm/delete-other-windows)
;; (global-set-key (kbd "C-x 2")  #'split-window-below) ; default key binding
;; (global-set-key (kbd "C-x 3")  #'split-window-right) ; default key binding
(global-set-key (kbd "C-x -")  #'ksm/window-zoom-out) ; pop and restore window configuration from stack
(global-set-key (kbd "C-x +")  #'ksm/window-zoom-in) ; push window configuration to stack and delete other windows
(global-set-key (kbd "C-x =")  #'balance-windows)

(require-package/with-requirements '(switch-window)
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
  (global-set-key (kbd "C-x q") 'switch-window)) ; like tmux C-z q, but only shows numbers to select when more than two windows

(when (or (display-graphic-p) (daemonp))
  (require-package/with-requirements '(default-text-scale)
    (default-text-scale-mode)))

(when (or (display-color-p) (daemonp))
  (require-package/with-requirements '(zenburn-theme)
    (load-theme 'zenburn t))

  (require-package/with-requirements '(xterm-color) ;; xterm-color is superior to ansi-color
    ;; compilation buffers
    (progn
      (setq compilation-environment '("TERM=xterm-256color"))
      (defun my/advice-compilation-filter (f proc string)
        (funcall f proc (xterm-color-filter string)))
      (advice-add 'compilation-filter :around #'my/advice-compilation-filter))

    ;; shell mode
    (progn
      (setq comint-output-filter-functions
            (remove 'ansi-color-process-output comint-output-filter-functions))
      (add-hook 'shell-mode-hook
                #'(lambda ()
                    ;; Disable font-locking in this buffer to improve performance
                    (font-lock-mode -1)
                    ;; Prevent font-locking from being re-enabled in this buffer
                    (make-local-variable 'font-lock-function)
                    (setq font-lock-function (lambda (_) nil))
                    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))))

    ;; eshell mode
    (when nil
      (require 'eshell) ; or use with-eval-after-load
      (require 'esh-mode)

      (add-hook 'eshell-before-prompt-hook
                #'(lambda ()
                    (setq xterm-color-preserve-properties t)))
      ;; (add-hook 'eshell-mode-hook
      ;;           #'(lambda ()
      ;;               (setq xterm-color-preserve-properties t)))

      (add-to-list 'eshell-preoutput-filter-functions #'xterm-color-filter)
      (setq eshell-output-filter-functions (remove #'eshell-handle-ansi-color eshell-output-filter-functions)))
    (setenv "TERM" "xterm-256color")) ;; other color display stuff?
  )

(when (display-mouse-p)
  ;; iTerm2 mouse support
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

;; (require 'nice-font)                ; nice-font guards with display-multi-font-p

;; Use plain text rather than using figlet to render text in
;; artist-mode.
(setq artist-text-renderer-function #'(lambda (someText) someText))

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
   '(lsp-ui lsp-mode vterm vterm-toggle sql-indent which-key buffer-move vc-fossil go-mode auto-complete nix-mode deadgrep default-text-scale edit-server fic-mode find-file-in-repository flycheck gnu-elpa-keyring-update go-errcheck js2-mode json-mode markdown-mode protobuf-mode rust-mode switch-window wgrep wgrep-ack xterm-color yaml-mode zenburn-theme zig-mode))
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
