;;; setup-org-mode --- customizations for org mode

;;; Commentary:

;;; Code:

(require 'org)
(require 'org-mode-begin-src)

(define-key org-mode-map (kbd "C-c s") #'org-mode-begin-src)

(require 'ol)
(define-key org-mode-map (kbd "C-c l") #'org-store-link)

(provide 'setup-org-mode)

;;; setup-org-mode.el ends here
