;;; raghu -- various awesome functions from Raghu

;;; Commentary:

;;; Code:

(defun raghu/insert-new-line-above (&optional lines)
  "Insert LINES (default=1) new lines above current line.

As new lines are inserted, point's position will remain constant
relative to the line on which point was located originally."
  (interactive "p")
  (when (> lines 0)
    (save-excursion
      (beginning-of-line 1)
      (open-line lines))
    ;; account for save-excursion working differently when column is 0
    (when (= (current-column) 0)
      (forward-line lines))))
(define-key global-map (kbd "C-c RET") #'raghu/insert-new-line-above)

(defun raghu/insert-new-line-below (&optional lines)
  "Insert LINES (default=1) new lines below current line.

As new lines are inserted, point's position will remain constant
relative to the line on which point was located originally."
  (interactive "p")
  (when (> lines 0)
    (save-excursion
      (end-of-line 1)
      (newline lines nil))))
(define-key global-map (kbd "C-c M-RET") #'raghu/insert-new-line-below)

(defun raghu/insert-and-go-to-new-line-above (&optional lines)
  "Insert LINES (default=1) new lines above current line.

The point is moved to the top-most line inserted."
  (interactive "p")
  (when (> lines 0)
    (beginning-of-line 1)
    (open-line lines)
    (indent-according-to-mode)))
(define-key global-map (kbd "C-c O") #'raghu/insert-and-go-to-new-line-above)

(defun raghu/insert-and-go-to-new-line-below (&optional lines)
  "Insert LINES (default=1) new lines below current line.

Point moves to the newly-inserted line immediately below the line
on which point originally was."
  (interactive "p")
  (when (> lines 0)
    (save-excursion
      (end-of-line 1)
      (newline lines nil))
    (forward-line)
    (indent-according-to-mode)))
(define-key global-map (kbd "C-c o") #'raghu/insert-and-go-to-new-line-below)

(defun raghu/scroll-up (&optional lines)
  "Scroll up LINES keeping point stationary relative to the screen/window.

LINES is 1 if not supplied or non-positive."
  (interactive "p")
  (when (<= lines 0)
    (setq lines 1))
  (scroll-up lines))
(define-key global-map (kbd "M-n") #'raghu/scroll-up)

(defun raghu/scroll-down (&optional lines)
  "Scroll down LINES keeping point stationary relative to the screen/window.

LINES is 1 if not supplied or non-positive."
  (interactive "p")
  (when (<= lines 0)
    (setq lines 1))
  (scroll-down lines))
(define-key global-map (kbd "M-p") #'raghu/scroll-down)

(provide 'raghu)

;;; raghu.el ends here
