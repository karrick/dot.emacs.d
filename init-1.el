;;; package --- Summary: Emacs Initialization -*- mode: emacs-lisp -*-

;;; Commentary:

;;; Code:

(fido-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When "cert" file in user-emacs-directory, presumably placed there
;; as a symbolic link to a host-specific yet non-standard system cert
;; file, then configure gnutls to trust it, before we attempt to
;; contact package-archives from which packages would be downloaded.
(if (not (gnutls-available-p))
    (message "GNU TLS is not available.")
  (with-eval-after-load 'gnutls
    (let ((cert (file-truename (locate-user-emacs-file "cert"))))
      (when (file-readable-p cert)
        (add-to-list 'gnutls-trustfiles cert)))))

;; (require 'package)
;; ;; (setq package-archives (append package-archives (list '("melpa-stable" . "https://stable.melpa.org/packages/"))))
;; ;; (setq package-archives (append package-archives (list '("melpa" . "https://melpa.org/packages/"))))
;; (setq package-archives
;;       '(("melpa-stable" . "https://stable.melpa.org/packages/")
;;  ("melpa" . "https://melpa.org/packages/")))
;; (setq package-archive-priorities (list '("melpa-stable" . 2) '("melpa" . 1) '("gnu" . 0)))

;; (unless package-archive-contents
;;   (package-refresh-contents))

(fset 'yes-or-no-p 'y-or-n-p)

;; (fido-mode 1)
;; (fido-vertical-mode 1)

(cond ((memq system-type '(gnu gnu/linux gnu/kfreebsd))
       (setq dired-listing-switches "-ABhlo"))
      (t
       (require 'ls-lisp)
       (setq dired-listing-switches "-ABhl"
             ls-lisp-use-insert-directory-program nil)))

(let ((dir (file-name-as-directory (expand-file-name ".history.d" "~"))))
  (when (file-directory-p dir)
    (setenv "HISTFILE" (concat dir "emacs"))))

(defun other-window-backward (&optional n)
  "Select the Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(global-set-key "\C-x\C-n" 'other-window)
(global-set-key "\C-x\C-p" 'other-window-backward)

(defalias 'scroll-forward 'scroll-up)
(defalias 'scroll-backward 'scroll-down)

(defun scroll-n-lines-forward (&optional n)
  "Scroll forward N lines (1 by default)."
  (interactive "P")
  (scroll-forward (prefix-numeric-value n)))

(defun scroll-n-lines-backward (&optional n)
  "Scroll backward N lines (1 by default)."
  (interactive "P")
  (scroll-forward (- (prefix-numeric-value n))))

(global-set-key (kbd "M-n") 'scroll-n-lines-forward)
(global-set-key (kbd "M-p") 'scroll-n-lines-backward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'scroll-up 'unscrollable t)
(put 'scroll-right 'unscrollable t)
(put 'scroll-down 'unscrollable t)
(put 'scroll-left 'unscrollable t)

(defvar unscroll-point (make-marker)
  "Cursor position for next call to `unscroll'.")
(defvar unscroll-window-start (make-marker)
  "Window start for next call to `unscroll'.")
(defvar unscroll-hscroll nil
  "Horizontal position for next call to `unscroll'.")

(defun unscroll-maybe-remember ()
  (unless (get last-command 'unscrollable)
    (message "remembering position for unscroll...") ; TODO
    (set-marker unscroll-point (point))
    (set-marker unscroll-window-start (window-start))
    (setq unscroll-hscroll (window-hscroll))))

(defun unscroll ()
  "Revert to `unscroll-point' and `unscroll-window-start'."
  (interactive)
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))

(defadvice scroll-up (before remember-for-unscroll
                             activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-right (before remember-for-unscroll
                                activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-down (before remember-for-unscroll
                               activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-left (before remember-for-unscroll
                               activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tooltip-mode 0)
(setq redisplay-dont-pause t)


;; EXAMPLE:
;; (with-eval-after-load "font-lock"
;;   (setq lisp-font-lock-keywords lisp-font-lock-keywords-2))

(when nil
  ;; Make Elisp files in ~/.emacs.d/lisp directory available.
  (add-to-list 'load-path (locate-user-emacs-file "lisp")))

;; (unless (package-installed-p 'use-package)
;;   (message "Refreshing package contents...")
;;   (package-refresh-contents)
;;   (message "Installing 'use-package...")
;;   (package-install 'use-package))

;; (eval-when-compile
;;   (require 'use-package)
;;   (setq use-package-verbose t))

;; (setq init-file-debug t)
;; (if init-file-debug
;;     (setq use-package-verbose t
;;           use-package-expand-minimally nil
;;           use-package-compute-statistics t
;;           debug-on-error t)
;;   (setq use-package-verbose nil
;;         use-package-expand-minimally t))

;; (use-package xterm-color)

;; ;; By default bind "C-x C-r" to rgrep, but when ripgrep and deadgrep
;; ;; are available, rebind to that...
;; (let ((cmd (executable-find "rg")))
;;   (if (not (stringp cmd))
;;       (global-set-key (kbd "C-x C-r") #'rgrep)
;;     (require-package/with-requirements '(deadgrep)
;;       (setq deadgrep-executable cmd)
;;       (global-set-key (kbd "C-x C-r") #'deadgrep))))

;; (use-package company
;;   :ensure t
;;   :pin melpa-stable)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(zenburn))
 '(custom-safe-themes
   '("b77a00d5be78f21e46c80ce450e5821bdc4368abf4ffe2b77c5a66de1b648f10" "78e9a3e1c519656654044aeb25acb8bec02579508c145b6db158d2cfad87c44e" default))
 '(dired-auto-revert-buffer t)
 '(fancy-splash-image "")
 '(inhibit-default-init t)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(package-selected-packages '(nov xterm-color zenburn-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
