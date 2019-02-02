;;; ksm-window-scrolling --- provides a few handy window scrolling functions

;;; Commentary:
;;;
;;; Use:
;;;
;;; I put this file somewhere Emacs will find it, then added the below to my
;;; ~/.emacs/init.el file. Note the first two key-bindings override sometimes
;;; useful defaults: forward-list and backward-list.
;;;
;;;     (global-set-key (kbd "C-M-n") #'ksm/forward-line-scroll-up)
;;;     (global-set-key (kbd "C-M-p") #'ksm/previous-line-scroll-down)
;;;     (global-set-key (kbd "M-p") #'ksm/see-more-up)
;;;     (global-set-key (kbd "M-n") #'ksm/see-more-down)
;;;
;;; Code:

(defun ksm/forward-line-scroll-up (&optional n)
  "Scroll window down N lines, keeping point at same relative position."
  (interactive "^p")                    ; number, if no prefix argument, defaults to 1
  (or n (setq n 1))
  (forward-line n)
  (scroll-up n))

(defun ksm/previous-line-scroll-down (&optional n)
  "Scroll window up N lines, keeping point at same relative position."
  (declare (interactive-only
            "use `ksm/forward-line-scroll-up' with negative argument instead."))
  (interactive "^p")                    ; number, if no prefix argument, defaults to 1
  (or n (setq n 1))
  (ksm/forward-line-scroll-up (- n)))

(defun ksm/see-more-down (&optional n)
  "Scroll window down N lines, keeping point at same relative position."
  (interactive "^p")                    ; number, if no prefix argument, defaults to 1
  (or n (setq n 1))
  (scroll-up n))

(defun ksm/see-more-up (&optional n)
  "Scroll window up N lines, keeping point at same relative position."
  (declare (interactive-only
            "use `ksm/forward-line-scroll-up' with negative argument instead."))
  (interactive "^p")                    ; number, if no prefix argument, defaults to 1
  (or n (setq n 1))
  (ksm/see-more-down (- n)))

(provide 'ksm-window-scrolling)

;;; ksm-window-scrolling.el ends here
