;;; setup-packages

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'package)

;; (setq package-enable-at-startup nil)
(package-initialize)

(defun setup-packages/compare-package-archives (a b)
  "Returns non-nil when two archive names are identical."
  (string-equal (car a) (car b)))

(defun setup-packages/compare-packages (a b)
  "Returns non-nil when two package names are identical."
  (string-equal (car a) (car b)))

(defmacro setup-packages/with-package-check-signature (arg &rest body)
  "Temporarily set package-check-signature set to ARG and execute BODY."
  ;; valid options: t nil 'allow-unsigned
  (declare (indent 1))
  `(let* ((save package-check-signature)
          (packge-check-signature ,arg))
     (unwind-protect
         (progn ,@body)
       (setq package-check-signature save))))

(defmacro setup-packages/with-package-archives (archives &rest body)
  "Temporarily set package-archives to ARCHIVES and execute BODY."
  (declare (indent 1))
  `(let* ((save-package-archives package-archives)
          (package-archives ,archives))
     (unwind-protect
         (progn
           (package-refresh-contents)
           ,@body)
       (progn
         (setq package-archives save-package-archives)
         ;; (package-refresh-contents)
         ))))

(defmacro setup-packages/with-pinned-packages (packages &rest body)
  "Temporarily set package-pinned-packages to PACKAGES and execute BODY."
  (declare (indent 1))
  `((let* ((saved package-pinned-packages)
           (package-pinned-packages ,packages))
      (unwind-protect
          (progn ,@body)
        (setq package-pinned-packages saved)))))

(defun setup-packages/install (tuples)
  "Ensure list of package TUPLES are installed."

  ;; set pinned packages
  (setup-packages/with-pinned-packages (mapcar #'(lambda (tuple)
                                                   (cons (car tuple) (cadr tuple)))
                                               tuples)
    (message "pinned packages: %s" package-pinned-packages)))

;; ;;

;; ;; group-by cadr (t nil 'allow-unsigned)
;; (dolist (sign-flag '(t nil 'allow-unsigned))
;;   (setup-packages/with-package-check-signature sign-flag
;;     (let ((filtered-list
;;            (mapcar #'(lambda (tuple)
;;                        (cons (car tuple) (cadr tuple)))
;;                    (cl-remove-if-not #'(lambda (tuple)
;;                                          (eq sign-flag (car (cdr (cdr tuple)))))
;;                                      tuples))))
;;       (setup-packages/with-pinned-packages filtered-list
;;         (setup-packages/install-missing-packages (mapcar #'car filtered-list)))))))


;; (message "sign-flag: %s; filtered-list: %s" sign-flag filtered-list)))))

;; filter t

;; filter nil

;; filter 'allow-unsigned


(defun setup-packages/install-missing-packages (packages)
  "Ensure list of PACKAGES are installed."
  (interactive)
  (let ((missing-packages (cl-remove-if #'package-installed-p packages)))
    (when missing-packages
      (message "Installing %d missing package(s)" (length missing-packages))
      ;; (package-refresh-contents)
      (mapc #'(lambda (pkg)
                (condition-case err
                    (package-install pkg)
                  (error (message "Cannot install package: %s" err))))
            missing-packages))))

(defvar setup-packages/package-list
  '(
    ;; (auto-complete "melpa-stable" t)
    ;; ;; (auto-complete-config "melpa-stable")
    ;; (bash-completion "melpa-stable" t)
    ;; (edit-server "melpa-stable" t)
    ;; ;; (erc "melpa-stable")
    ;; (expand-region "melpa-stable" t)
    ;; (fic-mode "melpa" t)
    ;; (find-file-in-repository "melpa-stable" t)
    ;; (flycheck "melpa-stable" t)
    ;; (js2-mode "melpa-stable" t)
    ;; (json-mode "melpa-stable" t)
    ;; (ivy "melpa-stable" t)
    ;; (markdown-mode "melpa-stable" t)
    ;; (maxframe "melpa-stable" t)
    ;; (multiple-cursors "melpa-stable" t)
    ;; ;; (nxml-mode "melpa-stable")
    ;; ;; (psgml "melpa-stable")
    ;; (puppet-mode "melpa-stable" t)
    ;; ;; (smart-tab "melpa-stable" t)
    ;; (wgrep "melpa-stable" t)
    ;; (wgrep-ack "melpa-stable" t)
    ;; (yaml-mode "melpa-stable" t)
    ;; (zenburn-theme "melpa-stable" t)
    ))

(setq package-pinned-packages (mapcar #'(lambda (tuple)
                                          `(,(car tuple) . ,(car (cdr tuple))))
                                      setup-packages/package-list))

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t #'setup-packages/compare-package-archives)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t #'setup-packages/compare-package-archives)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t #'setup-packages/compare-package-archives)
;; (package-refresh-contents)

;; (setq package-check-signature t)
;; ;; (setup-packages/with-package-check-signature t
;; (setup-packages/install-missing-packages (mapcar #'car package-pinned-packages))
;; ;; )

;; ;; (setup-packages/with-package-check-signature t
;; ;;   (setup-packages/with-package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;; ;;                            ("melpa-stable" . "http://stable.melpa.org/packages/"))
;; ;;     (setup-packages/install-missing-packages (list
;; ;;                        ))))

;; ;; (setup-packages/with-package-check-signature
;; ;;     (setup-packages/with-package-archives '(("melpa" . "https://melpa.org/packages/"))
;; ;;       (setup-packages/install-missing-packages (list
;; ;;                          ))))

(provide 'setup-packages)

;;; setup-packages.el ends here
