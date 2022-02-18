;;; require-package --- Summary

;; package initialization functions

;;; Commentary:

;;; Code:

;; This package uses a few functions from `cl-lib', included in Emacs distribution.
(require 'cl-lib)

;; This package extensively uses `package' library, with changes in "25.1" version of Emacs.
(require 'package)

(when (< emacs-major-version 27)
  (package-initialize))

(setq package-menu-hide-low-priority :archive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup require-package nil
  "Feature require and package initialization for Emacs Lisp packages."
  :package-version '(require-package "1.0.0")
  :group 'applications)

(defcustom require-package/archive-tuples
  '(
    ("melpa-stable" "https://stable.melpa.org/packages/" nil) ; do not require signed packages (causes package-level sign requirement to be ignored)
    ("gnu" "https://elpa.gnu.org/packages/" t)                ; require signed packages
    ("melpa" "https://melpa.org/packages/" nil)               ; do not require signed packages (causes package-level sign requirement to be ignored)
    )
  "Priority ordered list of archive tuples.

Each archive tuple is in the form of (NAME URL SIGNATURE-REQUIRED).

After making changes to this variable, users are advised to
invoke `require-package/update-package-archives'.

In Emacs 25.1 and newer, packages found from archives listed
earlier in this list will have priority over packages found in
archives listed later in this list.  Before Emacs 25.1, users are
advised to pin a package to a particular archive when desired
using `require-packages/pin-package-tuple'."
  ;; :type '(list (string :tag "Archive name") (string :tag "URL or directory name") (boolean :tag "Require signed"))
  :group 'require-package)

(defcustom require-package/signature-requirement-default nil
  "Value used for `package-check-signature' when installing feature without specified :signature-requirement keyword."
  :type 'boolean
  :group 'require-package)

(defcustom require-package/force-refresh nil
  "When non-nil, requires refresh of package repository contents."
  :type 'boolean
  :group 'require-package)

(defvar require-package/package-contents-refreshed nil "Non-nil after `require-package/maybe-package-refresh-contents' successfully run.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helper functions

(defun require-package/alist-from-tuples (tuples)
  "Convert TUPLES to alist of first two items in each tuple."
  (mapcar #'(lambda (tuple)
              (cons (car tuple) (cadr tuple)))
          tuples))

(defun require-package/maybe-package-refresh-contents ()
  "Refresh package contents if not yet refreshed.

Prevents every `ensure-require' from triggering invocation of
`package-refresh-contents'."
  (unless require-package/package-contents-refreshed
    (package-refresh-contents)
    (setq require-package/package-contents-refreshed (format-time-string "%FT%T%z"))))

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
  "Prepend package DOTTED-PAIR to `package-pinned-packages' if not yet a member."
  (cl-pushnew dotted-pair package-pinned-packages :key #'car))

(defun require-package/pin-package-tuples (package-tuples)
  "Add PACKAGE-TUPLES to 'package-pinned-packages' if not yet a member."
  (dolist (dotted-pair (require-package/pinned-package-alist-from-package-tuples package-tuples))
    (require-package/pin-package dotted-pair)))

(defun require-package/update-package-archives ()
  "Update various customization variables from `package' group.

This function is commonly called after making changes to
`require-package/archive-tuples' to update `package-archives',
`package-archive-priorities', and `package-unsigned-archives'."
  (setq package-unsigned-archives
        (require-package/alist-from-tuples
         ;; filter tuples to include only elements where SIGNATURE-REQUIRED, third item, is nil
         (cl-remove-if #'(lambda (tuple) (nth 2 tuple))
                       require-package/archive-tuples)))
  (let ((archive-names (reverse (mapcar #'car require-package/archive-tuples)))
        (priority 0))
    (setq package-archive-priorities nil)
    (dolist (archive-name archive-names)
      (push (cons archive-name priority) package-archive-priorities)
      (setq priority (+ priority 1))))
  (setq package-archives (require-package/alist-from-tuples require-package/archive-tuples)))

(defun require-package/value-from-key (key set)
  "Return element following KEY in the SET, or nil if not present."
  (cond ((eq set nil) nil)
        ((eq key (car set)) (cadr set))
        (t (require-package/value-from-key key (cdr set)))))

(defmacro require-package/with-variable (name value &rest body)
  "Set variable NAME with VALUE, then execute BODY.  Restore NAME to original value prior to return."
  (declare (indent 1))
  `(let* ((saved ,name)
          (,name ,value))
     (unwind-protect
         ,@body
       (setq ,name saved))))

(defmacro require-package/with-package-archives (archives &rest body)
  "Temporarily set `package-archives' to ARCHIVES and execute BODY."
  (declare (indent 1))
  `(require-package/with-variable package-archives ,archives ,@body))

(defmacro require-package/with-package-check-signature (signature &rest body)
  "Temporarily set `package-check-signature' set to SIGNATURE and execute BODY."
  ;; valid options: t nil 'allow-unsigned
  (declare (indent 1))
  `(require-package/with-variable package-check-signature ,signature ,@body))

(defmacro require-package/with-package-pinned-packages (pinned-package-alist &rest body)
  "Temporarily set `package-pinned-packages' to PINNED-PACKAGE-ALIST and execute BODY."
  (declare (indent 1))
  `(require-package/with-variable package-pinned-packages ,pinned-package-alist ,@body))

(defmacro require-package/with-package-unsigned-archives (archives &rest body)
  "Temporarily set `package-unsigned-archives' to ARCHIVES and execute BODY."
  (declare (indent 1))
  `(require-package/with-variable package-unsigned-archives ,archives ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *-from-package-tuple functions
;;
;; A package tuple begins with its FEATURE, as used by `require' elisp function, followed by one or
;; more keyword arguments, all of which are optional.
;;
;;   :package package-name (as used by `package-install' function, if missing, defaults to FEATURE)
;;   :archive archive-name (equivalent to first element of an archive in `package-archives' list)
;;   :signature-requirement (as used by `package-check-signature' variable)
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
  "Return dotted-pair as used by `package-pinned-packages' for PACKAGE-TUPLE."
  (if (atom package-tuple)
      (cons package-tuple nil)
    (cons (require-package/package-from-package-tuple package-tuple)
          (require-package/archive-from-package-tuple package-tuple))))

(defun require-package/signature-requirement-from-package-tuple (package-tuple)
  "Return signature-requirement slot of PACKAGE-TUPLE."
  (if (atom package-tuple)
      require-package/signature-requirement-default
    (or (require-package/value-from-key :signature-requirement package-tuple)
        require-package/signature-requirement-default)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *-from-package-tuples functions

(defun require-package/pinned-package-alist-from-package-tuples (package-tuples)
  "Return alist of PACKAGE-TUPLES which are pinned."
  ;; NOTE: convert to alist, then remove items with a null cdr
  (cl-remove-if #'(lambda (dotted-pair) (null (cdr dotted-pair)))
                (mapcar #'require-package/dotted-pair-from-package-tuple package-tuples)))

(defmacro require-package/with-merged-pinned-packages-from-package-tuples (package-tuples &rest body)
  "Temporarily merge PACKAGE-TUPLES with `package-pinned-packages' then execute BODY."
  (declare (indent 1))
  `(require-package/with-package-pinned-packages
       (require-package/merge-alists
        package-pinned-packages
        (require-package/pinned-package-alist-from-package-tuples ,package-tuples))
     ,@body))

(defun require-package/ensure-require (package-tuples)
  "Return t when all PACKAGE-TUPLES have been successfully required.

If any package-tuple cannot be loaded, prompt user to install
corresponding package.  If user declines or package fails to
install, stop processing PACKAGE-TUPLES and return nil."
  (require-package/pin-package-tuples package-tuples)
  (catch 'missing-requirement
    (dolist (pt package-tuples)
      (let ((feature (require-package/feature-from-package-tuple pt))
            (package-name (require-package/package-from-package-tuple pt))
            (archive-name (require-package/archive-from-package-tuple pt))
            (signature (require-package/signature-requirement-from-package-tuple pt)))
        (unless (or (require feature nil 'no-error)
                    (y-or-n-p (format "Install package (%s)%s? "
                                      package-name
                                      (if (null archive-name)
                                          ""
                                        (format " from (%s)" archive-name)))))
          (throw 'missing-requirement nil))
        (condition-case err
            (progn
              (when require-package/force-refresh
                (require-package/maybe-package-refresh-contents))
              (require-package/with-package-check-signature signature
                (package-install package-name))
              (require feature))
          (error (progn
                   (message "Cannot install package: %s: %s" package-name err)
                   (throw 'missing-requirement nil))))))
    t))                                 ; return t when all packages successfully required

(defmacro require-package/with-requirements (package-tuples &rest body)
  "Require PACKAGE-TUPLES then execute BODY.

If any requirement is not found, it prompts the user to install
the corresponding package.  If the user accepts, it attempts to
install the package.  If any package-tuples cannot be loaded, it
immediately returns without executing BODY."
  (declare (indent 1))
  `(when (require-package/ensure-require ,package-tuples)
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (and require-package/archive-tuples
           (> (length require-package/archive-tuples) 0))
  (require-package/update-package-archives))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'require-package)

;;; require-package.el ends here
