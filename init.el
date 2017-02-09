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

;; Some packages listed here would be installed automatically from
;; package dependency resolution system, but we want to pin the
;; package to a given archive to ensure we get a particular version.
;;
;; These are typically packages which are dependencies of other
;; packages we will explicitly pull
;; in. `require-package/ensure-installed` appends new package
;; requirements to `package-pinned-packages` before installing new
;; packages.
(dolist (package '(
                   (bash-completion . "melpa-stable")
                   (dash . "melpa-stable") ; flycheck
                   (epl . "melpa-stable") ; pkg-info
                   (ivy . "gnu") ; :signature-requirement t)
                   (pkg-info . "melpa-stable") ; flycheck
                   (popup . "melpa-stable")    ; auto-complete
                   (simple-httpd . "melpa-stable") ; ac-js2
                   (skewer-mode . "melpa-stable") ; ac-js2
                   ))
  (require-package/pin-package package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; process environment

(when (and (fboundp 'daemonp) (daemonp)) (cd (expand-file-name "~"))) ; change to home directory when invoked as daemon

(require 'path)
(let ((directories (list
                    "/usr/local/bin"
                    "~/bin"
                    )))
  (dolist (dir directories)
    (path-prepend dir)))

(setenv "GIT_PAGER" "")                  ; elide git paging capability.
(setenv "PAGER" (executable-find "cat")) ; in lieu of paging files, dump them to a buffer using `cat`.

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
(require-package/with-requirements '((flycheck :archive "melpa-stable"))
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-emacs-lisp-load-path 'inherit))

;; compilation
(setq compilation-scroll-output 'first-error)

;; parentheses matching
(show-paren-mode t)
(setq show-paren-style 'expression) ; highlight entire expression within parens
(set-face-background 'show-paren-match-face "#1f3f3f")

;; clean-and-indent
(require 'clean-and-indent)
(dolist (h '(sh-mode-hook css-mode-hook))
  (add-hook h #'(lambda () (add-hook 'before-save-hook #'clean-and-indent nil t))))
(global-set-key (kbd "<f2>") #'clean-and-indent)

;; copy-and-comment
(require 'copy-and-comment)
(global-set-key (kbd "<f3>") #'copy-and-comment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; advise the shell commands to name the buffer after the command itself
(eval-after-load 'shell-command
  (defadvice shell-command (before buffer-named-with-command
                                   (command &optional output-buffer error-buffer)
                                   activate compile)
    (setq output-buffer (or output-buffer (concat "*Shell: " command "*")))
    (let ((dir default-directory))
      (switch-to-buffer output-buffer)
      (setq default-directory dir))))

(when (fboundp 'async-shell-command)
  (defadvice async-shell-command (before buffer-named-with-command
                                         (command &optional output-buffer error-buffer)
                                         activate compile)
    (setq output-buffer (or output-buffer (concat "*Async: " command "*")))
    (let ((dir default-directory))
      (switch-to-buffer output-buffer)
      (setq default-directory dir)))
  (global-set-key [(meta !)] 'async-shell-command))

(prefer-coding-system 'utf-8)
(setq make-backup-files nil
      dired-listing-switches "-Bhl"
      diff-switches "-u"
      ediff-diff-options "-w"
      ediff-window-setup-function 'ediff-setup-windows-plain ; don't spawn a new frame for the ediff commands, keep it all in one frame
      ediff-split-window-function 'split-window-horizontally) ; have ediff buffers show in a side-by-side view

;; tabs and indenting
(setq-default indent-tabs-mode nil
              tab-width 8)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'perl-indent-level 'tab-width)

;; uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-after-kill-buffer-p nil
      uniquify-ignore-buffers-re "^\\*")

;; don't let the cursor go into minibuffer prompt (thank's, xah!)
(setq minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

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
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x C-r") #'rgrep)
(global-set-key (kbd "M-g") #'goto-line)
;; (global-set-key (kbd "s-r") #'(lambda () (interactive) (revert-buffer nil t nil)))
(global-set-key (kbd "<f1>") #'(lambda () (interactive) (revert-buffer nil t nil)))
(global-set-key (kbd "<S-f8>") #'compile)
(global-set-key (kbd "<f8>") #'recompile)

;; window movement

(defun other-window-backwards (&optional n)
  "Select Nth previous window."
  (interactive "p")
  (other-window (- (prefix-numeric-value n))))

(global-set-key (kbd "C-x C-n") #'other-window)
(global-set-key (kbd "C-x C-p") #'other-window-backwards)
(global-set-key (kbd "C-x n") #'other-window)
(global-set-key (kbd "C-x p") #'other-window-backwards)

(when (eq system-type 'darwin)
  ;; (global-set-key (kbd "s-<down>") #'windmove-down)
  ;; (global-set-key (kbd "s-<left>") #'windmove-left)
  ;; (global-set-key (kbd "s-<right>") #'windmove-right)
  ;; (global-set-key (kbd "s-<up>") #'windmove-up))
  (windmove-default-keybindings)
  ;; (setq windmove-wrap-around t)
  )

(require-package/with-requirements '((expand-region :archive "melpa-stable"))
  (global-set-key (kbd "H-=") #'er/expand-region)
  (global-set-key (kbd "H--") #'er/contract-region))

(require-package/with-requirements '((multiple-cursors :archive "melpa-stable"))
  (global-set-key (kbd "C-S-c C-S-c") #'mc/edit-lines)
  (global-set-key (kbd "C-c C-S-c") #'mc/edit-lines)
  (global-set-key (kbd "C->") #'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this)
  (global-set-key (kbd "C-c C->") #'mc/mark-more-like-this-extended))

;; edit-server for browsers (install "It's All Text!" on Firefox, or "Edit with Emacs" for Chrome)
(require-package/with-requirements '((edit-server :archive "melpa-stable"))
  (when (and (fboundp 'daemonp) (daemonp))
    (setq edit-server-new-frame nil)
    (edit-server-start)))

(require 'browser-open)
(require 'find-file-dynamic)
(require 'make-shebang-executable)
;; (require 'setup-codesearch)

;; writable grep buffers via toggling off read-only (similar to wdired mode for dired buffers)
(require-package/with-requirements '((wgrep :archive "melpa-stable") (wgrep-ack :archive "melpa-stable"))
  (define-key grep-mode-map (kbd "C-x C-q") #'wgrep-change-to-wgrep-mode)
  (setq wgrep-auto-save-buffer t))

;;;; Darwin fixes
(when (eq system-type 'darwin)
  ;; (setq ring-bell-function #'(lambda ()))
  (setq ns-function-modifier 'hyper
        ns-use-srgb-colorspace t)
  ;; darwin ls program
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; display

(require-package/with-requirements '((zenburn-theme :archive "melpa-stable"))
  (load-theme 'zenburn t))

(require-package/ensure-require '((switch-window :archive "melpa-stable")))

(when window-system
  (require 'nice-font))

(setq visible-bell (cond
                    ((eq system-type 'darwin) nil) ; darwin: do not use visibile-bell
                    (t t)))                        ; all others: flash frame instead of bell

;; add line and column numbers to the modeline
(line-number-mode 1)
(column-number-mode 1)
(setq scroll-conservatively 5
      scroll-step 1
      inhibit-startup-message t)
(put 'narrow-to-region 'disabled nil)

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(when (fboundp 'desktop-save-mode) (desktop-save-mode 0)) ; don't save desktop sessions

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

(require-package/ensure-require '(
                                  (fic-mode :archive "melpa")
                                  (keyword-search :archive "melpa")
                                  (markdown-mode :archive "melpa-stable")
                                  (yaml-mode :archive "melpa-stable")
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
 '(package-selected-packages
   (quote
    (go-autocomplete go-eldoc golint go-rename go-mode zenburn-theme yaml-mode wgrep-ack wgrep switch-window multiple-cursors markdown-mode keyword-search ivy find-file-in-repository fic-mode expand-region edit-server bash-completion auto-complete ac-emoji popup flycheck dash puppet-mode pkg-info epl))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
