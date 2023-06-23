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
  "Scroll window down N lines; keep point at same relative position from top."
  (interactive "^p")                    ; number, if no prefix argument, defaults to 1
  (or n (setq n 1))
  (forward-line n)
  (scroll-up n))

(defun ksm/previous-line-scroll-down (&optional n)
  "Scroll window up N lines; keep point at same relative position from top."
  (declare (interactive-only
			"use `ksm/forward-line-scroll-up' with negative argument instead."))
  (interactive "^p")                    ; number, if no prefix argument, defaults to 1
  (or n (setq n 1))
  (ksm/forward-line-scroll-up (- n)))

(defalias 'scroll-forward 'scroll-up)
(defalias 'scroll-backward 'scroll-down)

(defun scroll-n-lines-forward (&optional n)
  "Scroll forward N lines (1 by default)."
  (interactive "P")
  (scroll-forward (prefix-numeric-value n)))

(defun scroll-n-lines-backward (&optional n)
  "Scroll backward N lines (1 by default)."
  (interactive "P")
  (scroll-forward (- (prefix-numeric-value n))))

(put 'scroll-up 'unscrollable t)
(put 'scroll-right 'unscrollable t)
(put 'scroll-down 'unscrollable t)
(put 'scroll-left 'unscrollable t)

(defvar unscroll-point (make-marker)
  "Cursor position for next call to `unscroll'.")
(defvar unscroll-window-start (make-marker)
  "Window start for next call to `unscroll'.")
(defvar unscroll-hscroll nil
  "Horizontal position for next call to `unscroll'.")

(defun unscroll-maybe-remember ()
  "Store point before scrolling unless `last-command' was also scroll."
  (unless (get last-command 'unscrollable)
	;; (message "remembering position for unscroll...")
	(set-marker unscroll-point (point))
	(set-marker unscroll-window-start (window-start))
	(setq unscroll-hscroll (window-hscroll))))

(defun unscroll ()
  "Revert to `unscroll-point' and `unscroll-window-start'."
  (interactive)
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))

(defadvice scroll-up (before remember-for-unscroll
							 activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-right (before remember-for-unscroll
								activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-down (before remember-for-unscroll
							   activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-left (before remember-for-unscroll
							   activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))


(provide 'ksm-window-scrolling)

;;; ksm-window-scrolling.el ends here
