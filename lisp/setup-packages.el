;;; setup-packages --- Summary

;; package initialization functions

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'package)

;; (setq package-enable-at-startup nil)
(package-initialize)

(defcustom setup-packages/archive-tuples
  '(
    ("gnu" "https://elpa.gnu.org/packages/" t)                ; require signed packages
    ("melpa-stable" "https://stable.melpa.org/packages/" nil) ; do not require signed packages (causes package-level sign requirement to be ignored)
    ("melpa" "https://melpa.org/packages/" nil)               ; do not require signed packages (causes package-level sign requirement to be ignored)
    )
  "List of archive tuples, in the form of NAME, URL, and flag indicating whether we require signed packages from that archive.")

;; Some packages list here would be installed automatically from
;; package dependency resolution system, but we want to pin the
;; package to a given archive to ensure we get the more stable
;; version.
(defcustom setup-packages/package-tuples
  '(
    (ac-emoji "melpa-stable" t)
    (auto-complete "melpa-stable" t)
    (bash-completion "melpa-stable" t)
    (dash "melpa-stable" t) 		; dependency of flycheck
    (edit-server "melpa-stable" t)
    (epl "melpa-stable" t) ; dependency of pkg-info
    (expand-region "melpa-stable" t)
    (fic-mode "melpa" t)
    (find-file-in-repository "melpa-stable" t)
    (flycheck "melpa-stable" t)
    (ivy "gnu" t)
    (keyword-search "melpa" t)
    (markdown-mode "melpa-stable" t)
    ;; (maxframe "melpa-stable" t)
    (multiple-cursors "melpa-stable" t)
    (pkg-info "melpa-stable" t) ; dependency of puppet-mode, flycheck
    (popup "melpa-stable" t) ; dependency of auto-complete
    ;; (psgml "melpa-stable")
    (puppet-mode "melpa-stable" t)
    ;; (smart-tab "melpa-stable" t)
    (switch-window "melpa-stable" t)
    (wgrep "melpa-stable" t)
    (wgrep-ack "melpa-stable" t)
    (yaml-mode "melpa-stable" t)
    (zenburn-theme "melpa-stable" t)
    )
  "List of package tuples, in the form of NAME, package ARCHIVE pinning declaration, and a flag indicating whether we require signed package.")

(defun setup-packages/alist-from-tuples (tuples)
  "Convert TUPLES to alist of first two items in each tuple."
  (mapcar #'(lambda (tuple)
	      (cons (car tuple) (car (cdr tuple))))
          tuples))

(defmacro setup-packages/with-package-archives (archives &rest body)
  "Temporarily set package-archives to ARCHIVES and execute BODY."
  (declare (indent 1))
  `(let* ((saved package-archives)
          (package-archives ,archives))
     (unwind-protect
         (progn
           (package-refresh-contents)
           ,@body)
       (progn
         (setq package-archives saved)
         (package-refresh-contents)))))

(defmacro setup-packages/with-package-check-signature (signature &rest body)
  "Temporarily set package-check-signature set to SIGNATURE and execute BODY."
  ;; valid options: t nil 'allow-unsigned
  (declare (indent 1))
  `(let* ((saved package-check-signature)
          (packge-check-signature ,signature))
     (unwind-protect
         (progn ,@body)
       (setq package-check-signature saved))))

(defmacro setup-packages/with-package-pinned-packages (packages &rest body)
  "Temporarily set package-pinned-packages to PACKAGES and execute BODY."
  (declare (indent 1))
  `(let* ((saved package-pinned-packages)
          (package-pinned-packages ,packages))
     (message "package-pinned-packages: %s" package-pinned-packages)
     (unwind-protect
         (progn ,@body)
       (setq package-pinned-packages saved))))

(defmacro setup-packages/with-package-unsigned-archives (archives &rest body)
  "Temporarily set package-unsigned-archives to ARCHIVES and execute BODY."
  (declare (indent 1))
  `(let* ((saved package-unsigned-archives)
          (package-unsigned-archives ,archives))
     (message "package-unsigned-archives: %s" package-unsigned-archives)
     (unwind-protect
         (progn ,@body)
       (setq package-unsigned-archives saved))))

(defun setup-packages/install-missing-packages (packages)
  "Ensure list of PACKAGES are installed."
  (interactive)
  (let ((missing-packages (cl-remove-if #'package-installed-p packages)))
    (when missing-packages
      (message "Attempting to install %d missing package(s)" (length missing-packages))
      (mapc #'(lambda (pkg)
                (progn
                  (message "Attempting to install package: %s" pkg)
                  (condition-case err
                      (package-install pkg)
                    (error (message "Cannot install package: %s" err)))
		  ))
            missing-packages))))

(defun setup-packages/install (tuples)
  "Ensure list of package TUPLES are installed."
  (setup-packages/with-package-pinned-packages
      (setup-packages/alist-from-tuples tuples)
    ;; for each possible sign-flag, install missing packages
    (dolist (sign-flag '(t nil 'allow-unsigned))
      (setup-packages/with-package-check-signature sign-flag
        (setup-packages/install-missing-packages
         ;; convert list of filtered tuples to list of packages
         (mapcar #'(lambda (tuple) (car tuple))
                 ;; filter list of tuples to those with sign-flag
                 (cl-remove-if-not #'(lambda (tuple) (eq sign-flag (nth 2 tuple)))
                                   tuples)))))))

(defun setup-packages/merge-package-alists (a b)
  "Merge package alists A and B.  Elements from B will override any corresponding element from A."
  (let ((result nil))
    (dolist (item b)
      (unless (assoc (car item) result)
	(setq result (cons item result))))
    (dolist (item a)
      (unless (assoc (car item) result)
	(setq result (cons item result))))
    result))

(defun setup-packages/install-package-tuples ()
  "Install all packages defined by `setup-packages/package-tuples`."
  (interactive)
  (setup-packages/with-package-pinned-packages (setup-packages/alist-from-tuples setup-packages/package-tuples)
    (setup-packages/with-package-archives (setup-packages/alist-from-tuples setup-packages/archive-tuples)
      (setup-packages/with-package-unsigned-archives (setup-packages/alist-from-tuples
						      ;; filter tuples to list where third item is nil
						      (cl-remove-if #'(lambda (tuple) (nth 2 tuple))
								    setup-packages/archive-tuples))
	(let* ((saved debug-on-error)
	       (debug-on-error nil))
	  (unwind-protect
	      (condition-case err
		  (package-refresh-contents)
		(error (message "Cannot refresh package contents: %s" err)))
	    (setq debug-on-error saved)))
	(setup-packages/install setup-packages/package-tuples)))))

(setq package-pinned-packages (setup-packages/alist-from-tuples setup-packages/package-tuples))
(setq package-archives (setup-packages/alist-from-tuples setup-packages/archive-tuples))
(setq package-unsigned-archives (setup-packages/alist-from-tuples
                                 ;; filter tuples to list where third item is nil
                                 (cl-remove-if #'(lambda (tuple) (nth 2 tuple))
                                               setup-packages/archive-tuples)))

(provide 'setup-packages)

;;; setup-packages.el ends here
