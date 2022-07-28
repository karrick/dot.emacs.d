(defun raghu/duplicate-region-comment-original (beginning end)
  "Duplicate lines containing region, make the originals comments.

Take the lines that encapsulate the region defined by BEGINNING
and END, place a copy of these lines above the first line of the
region, and make those lines into comments.  If BEGINNING or END
is at the beginning (or end) of a line, that line will be
included.  This behaviour is to make it easy to \"mark\" a line
for consideration without having to worry about where point is in
in that line.  Just be somewhere on the starting line, activate
the mark if not already activated, and go somewhere on the ending
line to include both lines and the lines in between.

Do work only when the major mode of the buffer is derived from
prog-mode.  The concept of comments may not be well-defined for
non-programming modes, so err on the side of caution."
  (interactive "*r")
  (when (and (derived-mode-p 'prog-mode)
	     (integerp beginning)
	     (integerp end)
	     (>= beginning 1)
	     (>= end 1))
    ;; Ensure beginning <= end for ease of implementation.
    ;; (interactive "*r") guarantees that this requirement is
    ;; satisfied when this function is called interactively.  When
    ;; called in lisp, there are no such guarantees, so let us do it
    ;; ourselves.
    (let* ((start (if (< beginning end) beginning end))
	   (finish (if (= start beginning) end beginning))
	   (start-bol nil)
	   (finish-eol nil)
	   (copied-lines nil))
      (save-excursion
	(setq start-bol (progn (goto-char start)
			       (beginning-of-line)
			       (point)))
	(setq finish-eol (progn (goto-char finish)
				(end-of-line)
				(point)))
	;; Use buffer-substring instead of kill-ring-save because we
	;; do not want the copied text to end up on the kill-ring.
	;; The idea is to duplicate the lines, not to save them
	;; anywhere for yanking later.
	(setq copied-lines (buffer-substring start-bol finish-eol))
	(goto-char start-bol)
	(open-line 1)
	(insert copied-lines)
	(ignore-errors (comment-region start-bol finish-eol)))
      ;; Account for save-excursion behavior at beginning of line.
      (when (and (bolp) (= start (point)))
	(forward-line (count-lines start-bol finish-eol))))))

(defun raghu/duplicate-line-comment-original (arg)
  "Duplicate current line and make the duplicate a comment.

Starting from and including the current line, take ARG lines,
place a copy of them above the current line, and convert those
lines into comments.  If ARG is a non-zero positive integer,
perform this work on ARG lines below.  If ARG is a non-zero
negative integer, perform this work on ARG lines above.  In
either case, ARG includes the current line.  So, to perform the
work on the current line only, ARG must be either 1 or -1.  If
ARG is anything else, do nothing.

Do work only when the major mode of the buffer is derived from
prog-mode.  The concept of comments may not be well-defined for
non-programming modes, so err on the side of caution."
  (interactive "*p")
  (when (and (derived-mode-p 'prog-mode)
	     (integerp arg)
	     (not (= 0 arg)))
    (let ((original (point))
	  (start nil)
	  (end nil)
	  (copied-lines nil))
      (save-excursion
	(if (> arg 0)
	    (progn (forward-line (1- arg))
		   (end-of-line)
		   (setq end (point))
		   (goto-char original)
		   (beginning-of-line)
		   (setq start (point)))
	  (forward-line (1+ arg))	; arg is never zero here.
	  (setq start (point))
	  (goto-char original)
	  (end-of-line)
	  (setq end (point)))
	(setq copied-lines (buffer-substring start end))
	(goto-char start)
	(open-line 1)
	(insert copied-lines)
	(ignore-errors (comment-region start end)))
      ;; Account for save-excursion behavior at beginning of line.
      (when (and (bolp) (= start (point)))
	(forward-line (count-lines start end))))))

(defun raghu/duplicate-and-comment-original (arg)
  "Duplicate lines, make the originals into comments.

If region is active, call
`raghu/duplicate-region-comment-original' on the region.  If
region is not active and ARG is an integer, act on ARG lines by
calling `raghu/duplicate-line-comment-original' with argument
ARG.  If region is not active and ARG is a list, fetch its `car'.
If the result is an integer, say N, act on N lines by calling
`raghu/duplicate-line-comment-original' with argument N.  If
region is not active and ARG is none of the above, call
`raghu/duplicate-line-comment-original' with argument 1.

Do work only when the major mode of the buffer is derived from
prog-mode.  The concept of comments may not be well-defined for
non-programming modes, so err on the side of caution."
  (interactive "*P")
  (when (derived-mode-p 'prog-mode)
    (cond ((use-region-p) (raghu/duplicate-region-comment-original
			   (region-beginning)
			   (region-end)))
	  ((integerp arg) (raghu/duplicate-line-comment-original arg))
	  ((null arg) (raghu/duplicate-line-comment-original 1))
	  ((listp arg) (let ((n (car arg)))
			 (when (integerp n)
			   (raghu/duplicate-line-comment-original n))))
	  (t (raghu/duplicate-line-comment-original 1)))))
(define-key global-map (kbd "C-c I") #'raghu/duplicate-and-comment-original)

(provide 'raghu-duplicate-and-comment)
