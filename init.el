;;;; -*- mode: emacs-lisp -*-

;;; Commentary:

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)
(setq package-check-signature t)

(let ((default-directory (concat user-emacs-directory (convert-standard-filename "lisp/"))))
  (normal-top-level-add-to-load-path '("."))
  (ignore-errors
    (when (file-directory-p (concat default-directory "benchmark-init-el"))
      (require 'benchmark-init-loaddefs)
      (benchmark-init/activate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'path)

;;;; process environment

(let ((directories (list
                    "/usr/local/bin"
                    "~/bin"
                    )))
  (dolist (dir directories)
    (path-prepend dir)))

(let ((cmd (executable-find "emacsclient")))
  (setenv "EDITOR" cmd)
  (setenv "VISUAL" cmd))

(when (and (fboundp 'daemonp) (daemonp) (cd (expand-file-name "~"))))
(setenv "GIT_PAGER" "")			; elide git paging capability
(setenv "PAGER" "cat")

(prefer-coding-system 'utf-8)
(setq compilation-scroll-output 'first-error
      diff-switches "-u"
      dired-listing-switches "-Bhl"
      make-backup-files nil)

(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

;;;; ediff
(setq ediff-diff-options "-w"
      ediff-window-setup-function 'ediff-setup-windows-plain ;; don't spawn a new frame for the ediff commands, keep it all in one frame
      ediff-split-window-function 'split-window-horizontally) ;; have ediff buffers show in a side-by-side view

(dolist (item '(sh-mode-hook css-mode-hook))
  (add-hook item #'(lambda () (add-hook 'before-save-hook #'clean-and-indent nil t))))

;;;; advise the shell commands to name the buffer after the command itself
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

;;;; tabs and indenting

(setq-default indent-tabs-mode nil
              tab-width 8)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'perl-indent-level 'tab-width)

;;;; ido-mode

;; (require 'ido)
;; (ido-mode t)
;; (setq ido-enable-flex-matching t)	; enable fuzzy matching

;;;; uniquify buffer names

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-after-kill-buffer-p nil
      uniquify-ignore-buffers-re "^\\*")

;;;; svn mode

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

;;;; vcs

(eval-after-load "vc-hooks" '(define-key vc-prefix-map "=" 'vc-ediff))

;;;; fossil vc mode

(autoload 'vc-fossil-registered "vc-fossil")
(add-to-list 'vc-handled-backends 'Fossil)

;;;; ansi-color

(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(ansi-color-for-comint-mode-on) ; allow terminal colorization

;;;; highlight entire expression within parens
(show-paren-mode t)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face "#1f3f3f")

(when (fboundp 'desktop-save-mode) (desktop-save-mode 0)) ; don't save desktop sessions

;; ido-mode or ivy-mode

(add-hook 'after-init-hook
          #'(lambda ()
              (require 'ido)
              (if t
                  (ido-mode 1)
                (ivy-mode 1))))

;; codesearch
(require 'codesearch)

;; flycheck is the successor to flymake
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-emacs-lisp-load-path 'inherit)

;;;; key bindings

(global-set-key (kbd "C-x C-f") #'(lambda (&optional arg)
                                    "C-x C-f invokes #'ido-file-file; with C-u prefix, invokes #'find-file-in-repository."
                                    (interactive "P")
                                    (if (equal current-prefix-arg nil)
                                        (ido-find-file)
                                      (find-file-in-repository))))

(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x C-r") #'rgrep)
(global-set-key (kbd "M-g") #'goto-line)
;; (global-set-key (kbd "s-r") #'(lambda () (interactive) (revert-buffer nil t nil)))
(global-set-key (kbd "<f1>") #'(lambda () (interactive) (revert-buffer nil t nil)))
(global-set-key (kbd "<f2>") #'clean-and-indent)
(global-set-key (kbd "<f3>") #'copy-and-comment)
(global-set-key (kbd "<f8>") #'recompile)
(global-set-key (kbd "<S-f8>") #'compile)

;;;; window movement

(defun other-window-backwards (&optional n)
  "Select Nth previous window"
  (interactive "p")
  (other-window (- (prefix-numeric-value n))))

(global-set-key (kbd "C-x C-n") #'other-window)
(global-set-key (kbd "C-x C-p") #'other-window-backwards)
(global-set-key (kbd "C-x n") #'other-window)
(global-set-key (kbd "C-x p") #'other-window-backwards)

(when (eq system-type 'darwin)
  (global-set-key (kbd "s-<down>") #'windmove-down)
  (global-set-key (kbd "s-<left>") #'windmove-left)
  (global-set-key (kbd "s-<right>") #'windmove-right)
  (global-set-key (kbd "s-<up>") #'windmove-up))

;; expand-region
(global-set-key (kbd "H-=") #'er/expand-region)
(global-set-key (kbd "H--") #'er/contract-region)

;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") #'mc/edit-lines)
(global-set-key (kbd "C-c C-S-c") #'mc/edit-lines)
(global-set-key (kbd "C->") #'mc/mark-next-like-this)
(global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this)
(global-set-key (kbd "C-c C->") #'mc/mark-more-like-this-extended)

;;;; don't let the cursor go into minibuffer prompt (thank's, xah!)
(setq minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;;;; writable grep buffers via toggling off read-only (similar to wdired mode for dired buffers)
(add-hook 'after-init-hook
          #'(lambda ()
              (require 'wgrep)
              (define-key grep-mode-map (kbd "C-x C-q") #'wgrep-change-to-wgrep-mode)
              (setq wgrep-auto-save-buffer t)))

;;;; edit-server for browsers
;; install "It's All Text!" on Firefox, or "Edit with Emacs" for Chrome

(eval-after-load "edit-server"
  (when (and (fboundp 'daemonp) (daemonp) (locate-library "edit-server"))
    (require 'edit-server)
    (setq edit-server-new-frame nil)
    (edit-server-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook
          #'(lambda ()
              (require 'clean-and-indent)
              (require 'copy-and-comment)
              (require 'make-shebang-executable)
              (require 'setup-autocomplete)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Language specific setup files

(add-hook 'after-init-hook
          #'(lambda ()
              (require 'setup-elisp-mode)
              (require 'setup-go-mode) ; golang
              (require 'setup-javascript-mode)
              (require 'setup-python-mode)
              (require 'setup-ruby-mode)
              (add-to-list 'auto-mode-alist '("\\.xslt\\'" . nxml-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(condition-case err
    (require 'localhost)
  (file-error
   (message "no localhost file found: %s" err)))

(setq visible-bell (cond
                    ((eq system-type 'darwin) nil) ; darwin: do not use visibile-bell
                    (t t)))                        ; all others: flash frame instead of bell

;;;; Darwin fixes
(when (eq system-type 'darwin)
  ;; (setq ring-bell-function #'(lambda ()))
  (setq ns-function-modifier 'hyper
        ns-use-srgb-colorspace t)
  ;;darwin ls program
  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; add line and column numbers to the modeline
(line-number-mode 1)
(column-number-mode 1)
(setq scroll-conservatively 5
      scroll-step 1
      inhibit-startup-message t)
(put 'narrow-to-region 'disabled nil)

;;;; disable menu, scroll, and tool bars
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

(add-hook 'after-init-hook #'(lambda () (load-theme 'zenburn t)))

(when window-system
  (server-start)
  (require 'nice-font))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (auto-complete bash-completion edit-server expand-region fic-mode find-file-in-repository flycheck go-eldoc go-mode go-rename golint ivy js2-mode json-mode markdown-mode maxframe multiple-cursors puppet-mode shell-command smart-tab wgrep-ack yaml-mode zenburn-theme ))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
