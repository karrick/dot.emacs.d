;;;; -*- mode: emacs-lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; attempt to use package manager, but employ graceful degredation
;;;; where elpa is unavailable

;;; Code:
(condition-case err
    (progn
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
      (defmacro configure-package (packages &rest body)
        "Install specified package and run configuration body"
        `(progn
           (dolist (pkg ,packages)
             (if (not (package-installed-p pkg))
                 (package-install pkg)))
           (progn ,@body))))
  (file-error
   (progn
     (message "elpa not supported: %s" err)
     (defmacro configure-package (packages &rest body)
       "Skip installation and configuration of package because elpa is not installed"
       `(dolist (pkg ,packages)
          (message "skipping configure-package: %s" pkg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; I don't have any package specific configuration for these packages

(configure-package '(
                     bash-completion
                     fic-mode
                     find-file-in-repository
                     json-mode
                     markdown-mode
                     maxframe
                     psgml
                     puppet-mode
                     smart-tab
                     yaml-mode
                     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepend-path (elem)
  (interactive "DPrepend what directory to PATH: ")
  (let ((path (expand-file-name elem)))
    (when (string-match "/$" path)
      (setq path (concat (replace-match "" nil nil path))))
    (when (file-accessible-directory-p path)
      (add-to-list 'exec-path path)
      (setenv "PATH" (concat path ":" (getenv "PATH")))
      (message "prepending %s to PATH" path))))

(defun append-path (elem)
  (interactive "DAppend what directory to PATH: ")
  (let ((path (expand-file-name elem)))
    (when (string-match "/$" path)
      (setq path (concat (replace-match "" nil nil path))))
    (when (file-accessible-directory-p path)
      (add-to-list 'exec-path path)
      (setenv "PATH" (concat (getenv "PATH") ":" path))
      (message "appending %s to PATH" path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-first (predicate list)
  "Return result of first item in list which
satisfies predicate.  Returns nil if predicate
is nil for all items in list."
  (catch 'break
    (dolist (item list)
      (let ((result (funcall predicate item)))
        (if result
            (throw 'break result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; process environment

(let ((directories (list
                    "/usr/local/bin"
                    "~/bin"
                    )))
  (dolist (dir directories)
    (prepend-path dir)))

(if (boundp 'user-emacs-directory)
    (add-to-list 'load-path (concat user-emacs-directory "lisp"))
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp")))

(cd (expand-file-name "~"))
(setenv "GIT_PAGER" "")			; elide git paging capability
(setenv "PAGER" "cat")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; save new scripts as executable

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

(defun clean-and-indent ()
  (interactive "*")
  (apply #'indent-region (if (use-region-p)
                             (list (min (point) (mark)) (max (point) (mark)))
                           (list (point-min) (point-max))))
  (whitespace-cleanup))

(defun copy-and-comment (beg end)
  (interactive "*r")
  (if mark-active
      (save-excursion
        (copy-region-as-kill beg end)
        (yank)
        (comment-region beg end))
    (message "cannot copy-and-comment without region selected")))

(dolist (item '(sh-mode-hook css-mode-hook))
  (add-hook item #'(lambda ()
                     (add-hook 'before-save-hook #'clean-and-indent nil t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; advise the shell commands to name the buffer after the command
;;;; itself

(configure-package '(shell-command)
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

;;;; auto-complete-mode
(configure-package '(auto-complete)
                   (require 'auto-complete-config)
                   (add-to-list 'ac-dictionary-directories
                                (expand-file-name "~/.ac-dict"))
                   ;; ac-common-setup is called by ac-config-default
                   (defun ac-common-setup ()
                     (add-to-list 'ac-sources 'ac-source-yasnippet))
                   (add-to-list 'ac-modes 'html-mode)
                   (add-to-list 'ac-modes 'nxml-mode)
                   (ac-config-default)
                   (defun enable-auto-complete-mode ()
                     (auto-complete-mode 1))
                   (defun disable-auto-complete-mode ()
                     (auto-complete-mode 0)))

;;;; tabs and indenting

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'js-indent-level 'tab-width)
(defvaralias 'perl-indent-level 'tab-width)

(add-hook 'python-mode-hook
          #'(lambda ()
              (setq indent-tabs-mode nil)
              (setq tab-width 4)))

(add-hook 'javascript-mode-hook
          #'(lambda ()
              (setq indent-tabs-mode t)
              (setq js-indent-level 4)))

;; example '.dir-locals.el' file:
;;
;; ((nil . ((indent-tabs-mode . nil)
;;          (tab-width . 8)
;;          (fill-column . 80)))
;;  (js-mode . ((indent-tabs-mode . t)
;;              (js-indent-level . 8)
;;              (fill-column . 80)))
;;  (css-mode . ((indent-tabs-mode . t)
;;               (css-indent-offset . 8)
;;               (fill-column . 80)))
;;  (python-mode . ((indent-tabs-mode . nil)
;;                  (tab-width . 4)
;;                  (fill-column . 80)))
;;  (makefile-mode . ((indent-tabs-mode . t)
;;                    (tab-width . 8)
;;                    (fill-column . 80))))

;;;; ido-mode

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)	; enable fuzzy matching

;;;; different way of uniquifying names

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p nil)
(setq uniquify-ignore-buffers-re "^\\*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)
(setq diff-switches "-u")
(setq dired-listing-switches "-Bhl")
(setq dired-show-ls-switches t)
(setq make-backup-files nil)
(setq compilation-scroll-output 'first-error)

;;;; svn mode

(require 'psvn)

;;;; fossil vc mode

(autoload 'vc-fossil-registered "vc-fossil")
(add-to-list 'vc-handled-backends 'Fossil)

;;;; ansi-color
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(ansi-color-for-comint-mode-on) ; allow terminal colorization

;;;; disable bars
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

(if (boundp 'desktop-save-mode)
    (desktop-save-mode 0))		; don't save desktop sessions

;;;; highlight entire expression within parens
(show-paren-mode t)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face "#1f3f3f")

;;;; set to a usable font
(require 'nice-font)

;;;; ediff

(setq ediff-diff-options "-w")
;;;; don't spawn a new frame for the ediff commands, keep it all in one frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;;; have ediff buffers show in a side-by-side view
(setq ediff-split-window-function 'split-window-horizontally)

;;;; add line and column numbers to the modeline
(line-number-mode 1)
(column-number-mode 1)
(setq scroll-conservatively 5)
(setq scroll-step 1)
(setq visible-bell 1)
(setq inhibit-startup-message t)
(put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun other-window-backwards (&optional n)
  "Select Nth previous window"
  (interactive "p")
  (other-window (- (prefix-numeric-value n))))

;;;; key bindings

(global-set-key (kbd "C-x C-n") 'other-window)
(global-set-key (kbd "C-x C-p") 'other-window-backwards)
(global-set-key (kbd "C-x n") 'other-window)
(global-set-key (kbd "C-x p") 'other-window-backwards)

(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x C-r") #'rgrep)
(global-set-key (kbd "M-g") #'goto-line)

(global-set-key (kbd "<f1>") #'(lambda () (interactive) (revert-buffer nil t nil)))
(global-set-key (kbd "<f2>") #'clean-and-indent)
(global-set-key (kbd "<f3>") #'copy-and-comment)
(global-set-key (kbd "<f8>") #'recompile)
(global-set-key (kbd "<S-f8>") #'compile)

;;; move to different window

(global-set-key (kbd "<M-down>") 'windmove-down)
(global-set-key (kbd "<M-left>") 'windmove-left)
(global-set-key (kbd "<M-right>") 'windmove-right)
(global-set-key (kbd "<M-up>") 'windmove-up)

;;;; expand-region

(configure-package '(expand-region)
                   (global-set-key (kbd "H-=") 'er/expand-region)
                   (global-set-key (kbd "H--") 'er/contract-region))

;;;; multiple-cursor mode

(configure-package '(multiple-cursors)
                   (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
                   (global-set-key (kbd "C-c C-S-c") 'mc/edit-lines)
                   (global-set-key (kbd "C->") 'mc/mark-next-like-this)
                   (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
                   (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
                   (global-set-key (kbd "C-c C->") 'mc/mark-more-like-this-extended))

;;;; writable grep buffers via toggling off read-only (similar to
;;;; wdired mode for dired buffers)

(configure-package '(wgrep wgrep-ack)
                   (require 'wgrep)
                   (define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)
                   (setq wgrep-auto-save-buffer t))

;;;; don't let the cursor go into minibuffer prompt (thank's, xah!)

(setq minibuffer-prompt-properties
      (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

;;;; zenburn

(configure-package '(zenburn-theme)
                   (load-theme 'zenburn t))

;;;; Darwin fixes

;;;; TODO: want to run some of these localized to where frame is
;;;; created, and upon creation of new frame

(when (eq system-type 'darwin)
  ;; (when (eq nil window-system)
  ;;   (server-start))
  ;; (menu-bar-mode 1)
  ;; (setenv "LANG" "en_US.UTF-8")
  ;; (setq ns-use-srgb-colorspace t)
  (setq ns-function-modifier 'hyper)
  (setq dired-use-ls-dired nil))

;;;; edit-server for browsers
;; install "It's All Text!" on Firefox, or "Edit with Emacs" for Chrome

(let* ((client (find-first #'(lambda (item)
                               (executable-find item))
                           '(
                             "/usr/local/bin/emacsclient"
                             "/usr/bin/emacsclient")))
       (cmd (concat client " -a ''")))
  (setenv "EDITOR" cmd)
  (setenv "VISUAL" cmd)
  (configure-package '(edit-server)
                     (when (and (fboundp 'daemonp) (daemonp) (locate-library "edit-server"))
                       (require 'edit-server)
                       (setq edit-server-new-frame nil)
                       (edit-server-start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Language specific setup files
;;;; (eval-after-load 'clojure-mode '(require 'setup-clojure-mode))
;;;; (eval-after-load 'markdown-mode '(require 'setup-markdown-mode))

;;;; js2-mode offers nice javascript support
(configure-package '(js2-mode)
                   (autoload 'js2-mode "js2-mode" nil t)
                   (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
                   (set-default 'js2-basic-offset 4)
                   (set-default 'js2-mirror-mode nil)
                   (set-default 'js2-mode-escape-quotes nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ruby mode

(eval-after-load 'ruby-mode '(require 'setup-ruby-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; xslt mode

(configure-package '(nxml-mode)
                   (add-to-list 'auto-mode-alist '("\\.xslt$" . nxml-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; codesearch
(require 'codesearch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; flycheck is the successor to flymake

(configure-package '(flycheck)
                   (add-hook 'after-init-hook #'global-flycheck-mode)
                   (setq flycheck-check-syntax-automatically '(mode-enabled save)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; golang

(configure-package '(go-mode)
                   ;; (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "lisp/yasnippet-go"))

                   (let ((gopath (expand-file-name "~/go")))
                     (prepend-path (concat gopath "/bin"))
                     (setenv "GOPATH" gopath)
                     ;; go get -u golang.org/x/tools/cmd/oracle
                     (load-file (concat gopath "/src/golang.org/x/tools/cmd/oracle/oracle.el"))
                     (add-hook 'go-mode-hook 'go-oracle-mode)
                     )
                   (setenv "GO15VENDOREXPERIMENT" "1")

                   (configure-package '(golint)) ;; go get -u github.com/golang/lint/golint
                   (configure-package '(go-rename)) ;; go get -u golang.org/x/tools/cmd/gorename

                   (progn ;; go get -u golang.org/x/tools/cmd/goimports
                     (let ((gofmter (find-first #'(lambda (item)
                                                    (executable-find item))
                                                '("goimports"
                                                  "gofmt"))))
                       (setq gofmt-command gofmter)))

                   (add-hook 'go-mode-hook
                             #'(lambda ()
                                 (add-hook 'before-save-hook #'gofmt-before-save nil t)
                                 (flyspell-prog-mode)
                                 (setq fill-column 100)
                                 (local-set-key (kbd "C-c C-j") 'godef-jump-other-window) ;; go get -u github.com/rogpeppe/godef
                                 (if (not (string-match "^go" compile-command))
                                     (set (make-local-variable 'compile-command)
                                          "go test && go build")))))

(configure-package '(go-autocomplete)
                   ;; go get -u github.com/nsf/gocode
                   (require 'go-autocomplete)
                   (require 'auto-complete-config)
                   (ac-config-default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'irc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(condition-case err
    (require 'localhost)
  (file-error
   (message "no localhost file found: %s" err)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-program-name "ispell"))
