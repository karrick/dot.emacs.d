;;; nice-font --- provides a few handy font manipulation functions

;;; Commentary:

;;; Code:

;; only enable when emacs running under a graphical window system
(when window-system
  (add-to-list 'default-frame-alist '(font-backend . "xft"))
  (when (boundp 'font-use-system-font)
    (setq font-use-system-font t))

  (defun nice-font--font-family-list-sorted ()
    "Return list of sorted font families."
    (cl-remove-duplicates (sort (font-family-list)
                                #'(lambda(x y) (string< (upcase x) (upcase y))))
                          :test #'string=))

  (defun nice-font-change (requested)
    "Prompt the user and change the font to REQUESTED type face."
    (interactive "sName of Font: ")
    (if (member requested (nice-font--font-family-list-sorted))
        (set-face-attribute 'default nil :family requested :height 140)
      (message "Font not available: %s" requested)))

  (defun nice-font--filter (condp lst)
    (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

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
                   (set-face-attribute 'default nil :family selection :height 140))))
            (t (message "Search Matches %d fonts: %s" count (mapconcat #'identity matches ", "))))))

  ;; Darwin: command key maps to super modifier; therefore use super so that Command =, Command -,
  ;; and Command 0, all behave similar to how browsers will scale text size due to same key
  ;; bindings.
  (define-key global-map (kbd "s-=") #'text-scale-increase)
  (define-key global-map (kbd "s--") #'text-scale-decrease)
  (define-key global-map (kbd "s-0") #'(lambda () (interactive) (text-scale-set 0)))

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
                    (add-to-list 'default-frame-alist `(font . ,(concat f "-16")))))))

(provide 'nice-font)

;;; nice-font.el ends here
