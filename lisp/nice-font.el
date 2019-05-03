;;; nice-font --- provides a few handy font manipulation functions

;;; Commentary:

;;; Includes guard to ensure only runs when emacs process is capable of using
;;; multiple fonts, typically when running as a graphical process.

;;; Code:

(add-to-list 'default-frame-alist '(font-backend . "xft"))
(setq nice-font-height 100)             ; units of 1/10th of a point

(defun nice-font--font-family-list-sorted ()
  "Return list of sorted font families."
  (cl-remove-duplicates (sort (font-family-list)
                              #'(lambda(x y) (string< (upcase x) (upcase y))))
                        :test #'string=))

(defun nice-font-change (requested)
  "Prompt the user and change the font to REQUESTED type face."
  (interactive "sName of Font: ")
  (if (member requested (nice-font--font-family-list-sorted))
      (set-face-attribute 'default nil :family requested :height nice-font-height)
    (message "Font not available: %s" requested)))

(defun nice-font--filter (condp lst)
  "Return members of LST that meet requirements of CONDP."
  (delq nil (mapcar #'(lambda (x) (and (funcall condp x) x)) lst)))

(defun nice-font-search (regexp)
  "Search for a font using the REGEXP regular expression."
  (interactive "sRegular Expression: ")
  (let* ((matches (nice-font--filter
                   #'(lambda (item) (string-match regexp item))
                   (nice-font--font-family-list-sorted)))
         (count (length matches)))
    (cond ((eq 0 count)
           (message "No Font Matches"))
          ((eq 1 count)
           (let ((selection (nth 0 matches)))
             (if (y-or-n-p (concat "Font Matches: " selection "; Use? "))
                 (set-face-attribute 'default nil :family selection :height nice-font-height))))
          (t (message "Search Matches %d fonts: %s" count (mapconcat #'identity matches ", "))))))

;; set default font to one of my preferred fonts
(require 'find-first)
(find-first '(
              "Fantasque Sans Mono"
              "Menlo"
              "DejaVu Sans Mono"
              "Liberation Mono"
              "Inconsolata"
              "monofur"
              )
            #'(lambda (f)
                (when (member f (nice-font--font-family-list-sorted))
                  (add-to-list 'default-frame-alist `(font . ,(concat f "-12"))))))

(add-hook 'after-make-frame-functions
          #'(lambda ()
              (when (display-graphic-p)
                ;; (when (boundp 'font-use-system-font)
                ;;   (setq font-use-system-font t))

                ;; Darwin: command key maps to super modifier; therefore use super so that
                ;; Command =, Command -, and Command 0, all behave similar to how browsers
                ;; will scale text size due to same key bindings.
                (define-key global-map (kbd "s-=") #'text-scale-increase)
                (define-key global-map (kbd "s--") #'text-scale-decrease)
                (define-key global-map (kbd "s-0") #'(lambda () (interactive) (text-scale-set 0))))))

(provide 'nice-font)

;;; nice-font.el ends here
