;;; clean-and-indent -- clean whitespace and fix indentation

;;; Commentary:

;;; Code:

(defun clean-and-indent ()
  "Cleans up whitespace and re-indents buffer."
  (interactive "*")
  (apply #'indent-region (if (use-region-p)
                             (list (min (point) (mark)) (max (point) (mark)))
                           (list (point-min) (point-max))))
  (whitespace-cleanup))

(provide 'clean-and-indent)

;;; clean-and-indent.el ends here
