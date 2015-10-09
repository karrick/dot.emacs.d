(when nil
  (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 140)
  (set-face-attribute 'default nil :family "Inconsolata" :height 140)
  (set-face-attribute 'default nil :family "Liberation Mono" :height 140)
  (set-face-attribute 'default nil :family "Monofur" :height 140)
  )

(add-to-list 'default-frame-alist '(font-backend . "xft"))
(setq font-use-system-font t)

(let ((font-families (cl-remove-duplicates (sort (font-family-list)
                                                 #'(lambda(x y) (string< (upcase x) (upcase y))))
                                           :test #'string=)))
  (catch 'break
    (dolist (f '(
                 "DejaVu Sans Mono"
                 "Liberation Mono"
                 "Inconsolata"
                 "Monofur"
                 ))
      (message "trying font: %s" f)
      (if (member f font-families)
          (let ((font (concat f "-12")))
            (message "how's this? %s" font)
            (add-to-list 'default-frame-alist `(font . ,font))
            (message "%s" default-frame-alist)
            (throw 'break nil))))))

(provide 'nice-font)
