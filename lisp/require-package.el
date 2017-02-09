;;; require-package --- Summary

;; package initialization functions

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defun require-package/value-from-key (key set)
  "Return element following KEY in the SET, or nil if not present."
  (cond ((eq set nil) nil)
	((eq key (car set)) (cadr set))
	(t (require-package/value-from-key key (cdr set)))))

;;; package tuple (pt-*) functions
;;
;; A package tuple begins with its FEATURE, as used by `require` function, followed by one or more
;; keyword arguments, all of which are optional.
;;
;;   :package package-name (as used by `package-install` function, if missing, defaults to FEATURE)
;;   :archive archive-name (equivalent to first element of an archive in package-archives list)
;;   :signature-requirement (as used by `package-check-signature` variable)
;;
;; (auto-complete)
;; (auto-complete :package auto-complete)
;; (auto-complete :archive "melpa-stable")
;; (auto-complete :archive "melpa-stable" :signature-requirement t)

(defun require-package/pt-feature (package-tuple)
  "Return feature slot of PACKAGE-TUPLE."
  (if (atom package-tuple)
      package-tuple
    (car package-tuple)))			; feature name is first element

(defun require-package/pt-package (package-tuple)
  "Return package name slot of PACKAGE-TUPLE."
  (if (atom package-tuple)
      package-tuple
    (or (require-package/value-from-key :package package-tuple)
	(car package-tuple))))		; package name defaults to FEATURE name

(defun require-package/pt-archive (package-tuple)
  "Return archive slot of PACKAGE-TUPLE."
  (if (atom package-tuple)
      nil
    (require-package/value-from-key :archive package-tuple)))

(defun require-package/pt-pinned-alist (package-tuple)
  "Return alist used by package-pinned-packages for PACKAGE-TUPLE."
  (if (atom package-tuple)
      (cons package-tuple nil)
    (cons (require-package/pt-package package-tuple)
	  (require-package/pt-archive package-tuple))))

(defcustom require-package/signature-requirement-default nil
  "Value used for package-check-signature when installing feature without specified :signature-requirement keyword.")

(defun require-package/pt-signature-requirement (package-tuple)
  "Return signature-requirement slot of PACKAGE-TUPLE."
  (if (atom package-tuple)
      require-package/signature-requirement-default
    (or (require-package/value-from-key :signature-requirement package-tuple)
	require-package/signature-requirement-default)))

(defun require-package/pinned-packages (package-tuples)
  "Return alist of PACKAGE-TUPLES which are pinned."
  (cl-remove-if #'(lambda (alist) (null (cdr alist)))
		(mapcar #'require-package/pt-pinned-alist package-tuples)))

(defun require-package/merge-pinned-packages (a b)
  "Return alist of A and B package-tuples which are pinned.

Elements from B will override any corresponding element from A."
  (let ((result nil)
	(pinned-a (require-package/pinned-packages a))
	(pinned-b (require-package/pinned-packages b)))
    (dolist (item pinned-b)
      (unless (assoc (car item) result)
	(setq result (cons item result))))
    (dolist (item pinned-a)
      (unless (assoc (car item) result)
	(setq result (cons item result))))
    result))

(require 'package)
;; (setq package-enable-at-startup nil)
(package-initialize)

(defcustom require-package/archive-tuples
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
(defcustom require-package/package-tuples
  '(
    (epl :archive "melpa-stable") ; pkg-info
    (pkg-info :archive "melpa-stable") ; puppet-mode, flycheck
    (puppet-mode :archive "melpa-stable")

    (dash :archive "melpa-stable")		; flycheck
    (flycheck :archive "melpa-stable")

    (popup :archive "melpa-stable") ; auto-complete
    (ac-emoji :archive "melpa-stable")
    (auto-complete :archive "melpa-stable")
    (bash-completion :archive "melpa-stable")
    (edit-server :archive "melpa-stable")
    (expand-region :archive "melpa-stable")
    (fic-mode :archive "melpa")
    (find-file-in-repository :archive "melpa-stable")

    (ivy :archive "gnu" :signature-requirement t)
    (keyword-search :archive "melpa")
    (markdown-mode :archive "melpa-stable")
    ;; (maxframe :archive "melpa-stable" t)
    (multiple-cursors :archive "melpa-stable")
    ;; (psgml :archive "melpa-stable")
    ;; (smart-tab :archive "melpa-stable" t)
    (switch-window :archive "melpa-stable")
    (wgrep :archive "melpa-stable")
    (wgrep-ack :archive "melpa-stable")
    (yaml-mode :archive "melpa-stable")
    (zenburn-theme :archive "melpa-stable")
    )
  "List of package tuples, in the form of NAME, package ARCHIVE pinning declaration, and a flag indicating whether we require signed package.")

;;;

(defun require-package/alist-from-tuples (tuples)
  "Convert TUPLES to alist of first two items in each tuple."
  (mapcar #'(lambda (tuple)
	      (cons (car tuple) (cadr tuple)))
	  tuples))

(defmacro require-package/with-variable (name value &rest body)
  "Sets variable NAME with VALUE, then executes BODY.  Restores NAME to original value prior to returning."
  (declare (indent 1))
  `(let* ((saved ,name)
	  (,name ,value))
     (unwind-protect
	 ,@body
       (setq ,name saved))))

(defmacro require-package/with-package-archives (archives &rest body)
  "Temporarily set package-archives to ARCHIVES and execute BODY."
  (declare (indent 1))
  `(require-package/with-variable package-archives ,archives ,@body))

(defmacro require-package/with-package-check-signature (signature &rest body)
  "Temporarily set package-check-signature set to SIGNATURE and execute BODY."
  ;; valid options: t nil 'allow-unsigned
  (declare (indent 1))
  `(require-package/with-variable package-check-signature ,signature ,@body))

(defmacro require-package/with-package-pinned-packages (packages &rest body)
  "Temporarily set package-pinned-packages to PACKAGES and execute BODY."
  (declare (indent 1))
  `(require-package/with-variable package-pinned-packages ,packages ,@body))

(defmacro require-package/with-package-unsigned-archives (archives &rest body)
  "Temporarily set package-unsigned-archives to ARCHIVES and execute BODY."
  (declare (indent 1))
  `(require-package/with-variable package-unsigned-archives ,archives ,@body))

(defvar require-package/package-contents-refreshed nil)

(defun require-package/maybe-package-refresh-contents ()
  (unless require-package/package-contents-refreshed
    (package-refresh-contents)
    (setq require-package/package-contents-refreshed t)))

(defun require-package/ensure-installed (requirements)
  "Return t when all REQUIREMENTS loaded.

If any requirement cannot be loaded, prompt user to install
corresponding package. If user declines or package fails to
install, return nil."
  (require-package/with-package-pinned-packages
      (require-package/merge-pinned-packages require-package/package-tuples requirements)
    (catch 'missing-requirement
      (dolist (r requirements)
	(let ((feature (require-package/pt-feature r))
	      (package-name (require-package/pt-package r))
	      (archive-name (require-package/pt-archive r))
	      (signature (require-package/pt-signature-requirement r)))
	  (unless
	      (or
	       (require feature nil 'no-error)
	       (y-or-n-p (format "Install package (%s)%s? "
				 package-name
				 (if (null archive-name)
				     ""
				   (format " from (%s)" archive-name)))))
	    (throw 'missing-requirement nil))
	  (condition-case err
	      (progn
		(require-package/maybe-package-refresh-contents)
		(require-package/with-package-check-signature signature
		  (package-install package-name))
		(require feature))
	    (error (progn
		     (message "Cannot install package: %s: %s" package-name err)
		     (throw 'missing-requirement nil))))))
      t)))

;; (defun test-ensure-installed ()
;;   (interactive)
;;   (message "ensure-installed return: %s"
;;	   (require-package/ensure-installed
;;	    (list (car require-package/package-tuples)
;;		  (cadr require-package/package-tuples)))))

(defmacro require-package/with-requirements (requirements &rest body)
  "Requires REQUIREMENTS then executes BODY.

If any requirement is not found, it prompts the user to install
the corresponding specified package. If the user accepts, it
attempts to install the package. If any requirements cannot be
loaded, it bails."
  (declare (indent 1))
  `(when (require-package/ensure-installed ,requirements)
     ,@body))

(defun test-with-requirements ()
  (interactive)
  (require-package/with-requirements
      ;; '(
      ;;   (go-autocomplete :archive "melpa-stable")
      ;;   (go-eldoc :archive "melpa-stable")
      ;;   (go-mode :archive "melpa-stable")
      ;;   )
      ;; (list (car require-package/package-tuples)
      ;;	    (cadr require-package/package-tuples))
      require-package/package-tuples
    (message "everything installed")))

(setq package-pinned-packages
      (require-package/merge-pinned-packages require-package/package-tuples nil))

(setq package-archives
      (require-package/alist-from-tuples require-package/archive-tuples))

(setq package-unsigned-archives
      (require-package/alist-from-tuples
       ;; filter tuples to list where third item is nil
       (cl-remove-if #'(lambda (tuple) (nth 2 tuple))
		     require-package/archive-tuples)))

(provide 'require-package)

;;; require-package.el ends here
