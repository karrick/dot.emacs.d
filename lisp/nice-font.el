(add-to-list 'default-frame-alist '(font-backend . "xft"))
(setq font-use-system-font t)

(defun font-family-list-sorted ()
  (cl-remove-duplicates (sort (font-family-list)
                              #'(lambda(x y) (string< (upcase x) (upcase y))))
                        :test #'string=))

(defun font-change (requested)
  (interactive "sName of Font: ")
  (if (member requested (font-family-list-sorted))
      (set-face-attribute 'default nil :family requested :height 140)
    (message "Font not available: %s" requested)))

(defun my-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun font-search (regexp)
  (interactive "sRegular Expfession: ")
  (let* ((matches (my-filter
                  #'(lambda (item) (string-match regexp item))
                  (font-family-list-sorted)))
         (count (length matches)))
    (cond ((eq 0 count)
           (message "No Font Matches"))
          ((eq 1 count)
           (let ((selection (nth 0 matches)))
             (if (y-or-n-p (concat "Font Matches: " selection "; Use? "))
                 (set-face-attribute 'default nil :family selection :height 140))))
          (t (message "Search Matches %d fonts: %s" count (mapconcat #'identity matches ", "))))))

;; set default font to one of my preferred fonts
(catch 'break
  (dolist (f '(
               "DejaVu Sans Mono"
               "Liberation Mono"
               "Inconsolata"
               "monofur"
               ))
    (when (member f (font-family-list-sorted))
      (add-to-list 'default-frame-alist `(font . ,(concat f "-10")))
      (throw 'break nil))))

(provide 'nice-font)