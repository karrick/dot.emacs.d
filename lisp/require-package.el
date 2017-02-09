;;; require-package --- Summary

;; package initialization functions

;;; Commentary:

;;; Code:

(require 'cl-lib)

(require 'package)
;; (setq package-enable-at-startup nil)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helper functions

(defun require-package/alist-from-tuples (tuples)
  "Convert TUPLES to alist of first two items in each tuple."
  (mapcar #'(lambda (tuple)
              (cons (car tuple) (cadr tuple)))
          tuples))

(defvar require-package/package-contents-refreshed nil)

(defun require-package/maybe-package-refresh-contents ()
  (unless require-package/package-contents-refreshed
    (package-refresh-contents)
    (setq require-package/package-contents-refreshed (current-time-string))))

(defun require-package/merge-alists (a b)
  "Return alist resulting from merging A and B.

Elements from B will override any corresponding element from A."
  (let ((result nil))
    (dolist (item b)
      (unless (assoc (car item) result)
        (setq result (cons item result))))
    (dolist (item a)
      (unless (assoc (car item) result)
        (setq result (cons item result))))
    result))

(defun require-package/pin-package (dotted-pair)
  "Prepend package DOTTED-PAIR to package-pinned-packages if not yet a member."
  (add-to-list 'package-pinned-packages dotted-pair nil
               #'(lambda (a b) (equal (car a) (car b)))))

(defun require-package/value-from-key (key set)
  "Return element following KEY in the SET, or nil if not present."
  (cond ((eq set nil) nil)
        ((eq key (car set)) (cadr set))
        (t (require-package/value-from-key key (cdr set)))))

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

(defmacro require-package/with-package-pinned-packages (pinned-package-alist &rest body)
  "Temporarily set package-pinned-packages to PINNED-PACKAGE-ALIST and execute BODY."
  (declare (indent 1))
  `(require-package/with-variable package-pinned-packages ,pinned-package-alist ,@body))

(defmacro require-package/with-package-unsigned-archives (archives &rest body)
  "Temporarily set package-unsigned-archives to ARCHIVES and execute BODY."
  (declare (indent 1))
  `(require-package/with-variable package-unsigned-archives ,archives ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package-archive functions

(defcustom require-package/archive-tuples
  '(
    ("gnu" "https://elpa.gnu.org/packages/" t)                ; require signed packages
    ("melpa-stable" "https://stable.melpa.org/packages/" nil) ; do not require signed packages (causes package-level sign requirement to be ignored)
    ("melpa" "https://melpa.org/packages/" nil)               ; do not require signed packages (causes package-level sign requirement to be ignored)
    )
  "List of archive tuples, in the form of NAME, URL, and flag indicating whether we require signed packages from that archive.")

(setq package-archives (require-package/alist-from-tuples require-package/archive-tuples))

(setq package-unsigned-archives
      (require-package/alist-from-tuples
       ;; filter tuples to list where third item is nil
       (cl-remove-if #'(lambda (tuple) (nth 2 tuple))
                     require-package/archive-tuples)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom require-package/package-tuples nil
  "List of package tuples, in the form of NAME, package ARCHIVE pinning declaration, and a flag indicating whether we require signed package.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *-from-package-tuple functions
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

(defun require-package/feature-from-package-tuple (package-tuple)
  "Return feature slot of PACKAGE-TUPLE."
  (if (atom package-tuple)
      package-tuple
    (car package-tuple)))			; feature name is first element

(defun require-package/package-from-package-tuple (package-tuple)
  "Return package name slot of PACKAGE-TUPLE."
  (if (atom package-tuple)
      package-tuple
    (or (require-package/value-from-key :package package-tuple)
        (car package-tuple))))		; package name defaults to FEATURE name

(defun require-package/archive-from-package-tuple (package-tuple)
  "Return archive slot of PACKAGE-TUPLE."
  (if (atom package-tuple)
      nil
    (require-package/value-from-key :archive package-tuple)))

(defun require-package/dotted-pair-from-package-tuple (package-tuple)
  "Return dotted-pair as used by package-pinned-packages for PACKAGE-TUPLE."
  (if (atom package-tuple)
      (cons package-tuple nil)
    (cons (require-package/package-from-package-tuple package-tuple)
          (require-package/archive-from-package-tuple package-tuple))))

(defcustom require-package/signature-requirement-default nil
  "Value used for package-check-signature when installing feature without specified :signature-requirement keyword.")

(defun require-package/signature-requirement-from-package-tuple (package-tuple)
  "Return signature-requirement slot of PACKAGE-TUPLE."
  (if (atom package-tuple)
      require-package/signature-requirement-default
    (or (require-package/value-from-key :signature-requirement package-tuple)
        require-package/signature-requirement-default)))

(defun require-package/pin-package-tuple (package-tuple)
  "Add PACKAGE-TUPLE to package-pinned-packages if not yet a member."
  (add-to-list 'package-pinned-packages (require-package/dotted-pair-from-package-tuple package-tuple) nil
               #'(lambda (a b) (equal (car a) (car b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *-from-package-tuples functions

(defun require-package/pinned-package-alist-from-package-tuples (package-tuples)
  "Return alist of PACKAGE-TUPLES which are pinned."
  (cl-remove-if #'(lambda (dotted-pair) (null (cdr dotted-pair)))
                (mapcar #'require-package/dotted-pair-from-package-tuple package-tuples)))

(defmacro require-package/with-merged-pinned-packages (package-tuples &rest body)
  "Merge PACKAGE-TUPLES with package-pinned-packages and execute BODY."
  (declare (indent 1))
  `(require-package/with-package-pinned-packages
       (require-package/merge-alists
        package-pinned-packages
        (require-package/pinned-package-alist-from-package-tuples ,package-tuples))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun require-package/ensure-require (package-tuples)
  "Return t when all PACKAGE-TUPLES have been successfully required.

If any package-tuple cannot be loaded, prompt user to install
corresponding package. If user declines or package fails to
install, stop processing PACKAGE-TUPLES and return nil."
  (require-package/with-merged-pinned-packages package-tuples
    (catch 'missing-requirement
      (dolist (pt package-tuples)
        (let ((feature (require-package/feature-from-package-tuple pt))
              (package-name (require-package/package-from-package-tuple pt))
              (archive-name (require-package/archive-from-package-tuple pt))
              (signature (require-package/signature-requirement-from-package-tuple pt)))
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

(defmacro require-package/with-requirements (package-tuples &rest body)
  "Requires PACKAGE-TUPLES then executes BODY.

If any requirement is not found, it prompts the user to install
the corresponding specified package. If the user accepts, it
attempts to install the package. If any package-tuples cannot be
loaded, it bails."
  (declare (indent 1))
  `(when (require-package/ensure-require ,package-tuples)
     ,@body))

(provide 'require-package)

;;; require-package.el ends here
