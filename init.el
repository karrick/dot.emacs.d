;;;; -*- mode: emacs-lisp -*-

;;; Commentary:

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
;;; process environment

(require 'path)
(let ((directories (list
                    "/usr/local/bin"
                    "~/bin"
                    )))
  (dolist (dir directories)
    (path-prepend dir)))

(setenv "GIT_PAGER" "")                  ; elide git paging capability.
(setenv "PAGER" (executable-find "cat")) ; in lieu of paging files, dump them to a buffer using `cat`.

(when (and (fboundp #'daemonp) (daemonp)) (cd (expand-file-name "~"))) ; change to home directory when invoked as daemon

(when window-system
  (let ((cmd (executable-find "emacsclient")))
    (when cmd
      (require 'server)
      (unless (server-running-p) (message "window-system and server is not yet running; starting server") (server-start))
      (setenv "EDITOR" cmd)
      (setenv "VISUAL" cmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; configuration

(require 'setup-autocomplete)

;; flycheck is the successor to flymake
(require-package/with-requirements '(flycheck)
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; parentheses matching
(show-paren-mode t)

;; clean-and-indent
(require 'clean-and-indent)
(dolist (h '(sh-mode-hook css-mode-hook))
  (add-hook h #'(lambda () (add-hook 'before-save-hook #'clean-and-indent nil t))))
(define-key global-map (kbd "<f2>") #'clean-and-indent)

;; copy-and-comment
(require 'copy-and-comment)
(define-key global-map (kbd "<f3>") #'copy-and-comment)

(require 'async-shell-command-wrapper)
(define-key global-map (kbd "M-&") #'ksm/async-shell-command)
(define-key global-map (kbd "ESC &") #'ksm/async-shell-command)

(prefer-coding-system 'utf-8)

(setq ediff-diff-options "-w"
      ediff-window-setup-function 'ediff-setup-windows-plain ; don't spawn a new frame for the ediff commands, keep it all in one frame
      ediff-split-window-function 'split-window-horizontally) ; have ediff buffers show in a side-by-side view

;; tabs and indenting
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'perl-indent-level 'tab-width)

(require 'uniquify)

;; vcs
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

;; key bindings

(define-key global-map (kbd "C-z") nil)     ; disable suspend-frame
(define-key global-map (kbd "C-x C-z") nil) ; disable suspend-frame
(define-key global-map (kbd "C-x C-c") nil) ; disable save-buffers-kill-terminal

(define-key global-map (kbd "C-x C-b") #'ibuffer)
(define-key global-map (kbd "C-x C-r") #'rgrep)
(define-key global-map (kbd "<f1>") #'(lambda () (interactive) (revert-buffer nil t nil)))
(define-key global-map (kbd "<f5>") #'compile)
(define-key global-map (kbd "<f4>") #'recompile)

(require-package/with-requirements '(expand-region)
  (define-key global-map (kbd "M-=") #'er/expand-region)
  (define-key global-map (kbd "ESC =") #'er/expand-region)
  (define-key global-map (kbd "M--") #'er/contract-region)
  (define-key global-map (kbd "ESC -") #'er/contract-region))

(require-package/with-requirements '(multiple-cursors)
  (define-key global-map (kbd "C-S-c C-S-c") #'mc/edit-lines)
  (define-key global-map (kbd "C-c C-S-c") #'mc/edit-lines)
  (define-key global-map (kbd "C->") #'mc/mark-next-like-this)
  (define-key global-map (kbd "C-<") #'mc/mark-previous-like-this)
  (define-key global-map (kbd "C-c C-<") #'mc/mark-all-like-this)
  (define-key global-map (kbd "C-c C->") #'mc/mark-more-like-this-extended))

;; edit-server for browsers (install "It's All Text!" on Firefox, or "Edit with Emacs" for Chrome)
(require-package/with-requirements '(edit-server)
  (when (and (fboundp #'daemonp) (daemonp))
    (edit-server-start)))

(require 'browser-open)
(require 'find-file-dynamic)
(require 'make-shebang-executable)
;; (require 'setup-codesearch)

;; writable grep buffers via toggling off read-only (similar to wdired mode for dired buffers)
(require-package/with-requirements '(wgrep wgrep-ack)
  (define-key grep-mode-map (kbd "C-x C-q") #'wgrep-change-to-wgrep-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; display

(defun ksm/forward-line-scroll-up (&optional n)
  "Scroll window down N lines, keeping point at same relative position."
  (interactive "^p")                    ; number, if no prefix argument, defaults to 1
  (or n (setq n 1))
  (forward-line n)
  (scroll-up n))
(define-key global-map (kbd "C-S-n") #'ksm/forward-line-scroll-up)

(defun ksm/previous-line-scroll-down (&optional n)
  "Scroll window up N lines, keeping point at same relative position."
  (declare (interactive-only
            "use `ksm/forward-line-scroll-up' with negative argument instead."))
  (interactive "^p")                    ; number, if no prefix argument, defaults to 1
  (or n (setq n 1))
  (ksm/forward-line-scroll-up (- n)))
(define-key global-map (kbd "C-S-p") #'ksm/previous-line-scroll-down)

(define-key global-map (kbd "C-c H") #'hl-line-mode)

(define-key global-map (kbd "M-<up>") #'enlarge-window)
(define-key global-map (kbd "ESC <up>") #'enlarge-window)
(define-key global-map (kbd "M-<down>") #'shrink-window)
(define-key global-map (kbd "ESC <down>") #'shrink-window)
(define-key global-map (kbd "M-<left>") #'shrink-window-horizontally)
(define-key global-map (kbd "ESC <left>") #'shrink-window-horizontally)
(define-key global-map (kbd "M-<right>") #'enlarge-window-horizontally)
(define-key global-map (kbd "ESC <right>") #'enlarge-window-horizontally)

(require-package/with-requirements '(switch-window)
  (define-key global-map (kbd "C-x o") 'switch-window))

(require 'raghu)

(when nil
  (require-package/with-requirements '(zenburn-theme)
    (load-theme 'zenburn t)))

(when window-system
  (require 'nice-font))

;; add line and column numbers to the modeline
(line-number-mode 1)
(column-number-mode 1)
;; (setq scroll-conservatively 5
;;       scroll-step 1)
(put 'narrow-to-region 'disabled nil)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(if (fboundp #'desktop-save-mode) (desktop-save-mode 0)) ; don't save desktop sessions

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
                       (funcall 'compilation-filter proc (xterm-color-filter string))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; language specific configuration

(require 'setup-elisp-mode)
(require 'setup-go-mode) ; golang
(require 'setup-javascript-mode)
(require 'setup-python-mode)
(require 'setup-ruby-mode)

(require-package/with-requirements '(json-mode)
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode)))

(require-package/ensure-require '(
                                  fic-mode
                                  keyword-search
                                  markdown-mode
                                  yaml-mode
                                  ))

;; (add-to-list 'auto-mode-alist '("\\.xslt\\'" . nxml-mode))

(if (locate-library "localhost")
    (require 'localhost)
  (message "no localhost file found"))

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-scroll-output (quote first-error))
 '(diff-switches "-u")
 '(dired-listing-switches "-Bhlo")
 '(edit-server-new-frame nil)
 '(eshell-output-filter-functions
   (quote
    (eshell-handle-control-codes eshell-watch-for-password-prompt eshell-postoutput-scroll-to-bottom eshell-handle-control-codes eshell-watch-for-password-prompt)))
 '(flycheck-emacs-lisp-load-path (quote inherit))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(minibuffer-prompt-properties
   (quote
    (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(ns-function-modifier (quote hyper))
 '(ns-use-srgb-colorspace t)
 '(package-selected-packages
   (quote
    (wgrep zenburn-theme yaml-mode xterm-color wgrep-ack switch-window multiple-cursors markdown-mode keyword-search json-mode golint go-rename go-eldoc go-autocomplete flycheck find-file-in-repository fic-mode expand-region edit-server ac-js2 ac-emoji)))
 '(show-paren-style (quote expression))
 '(tab-width 4)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(visible-bell t)
 '(wgrep-auto-save-buffer t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((((type x ns) (class color) (background dark)) (:background "firebrick4"))))
 '(show-paren-match ((((type x ns) (class color) (background light)) (:background "PaleTurquoise1")))))
