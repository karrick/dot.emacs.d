(defun raghu/duplicate-region-comment-original (beginning end)
  "Duplicate lines containing region, make the originals comments.

Consider the full lines CONTAINING the region defined by
BEGINNING and END.  This means if point is at the end of a line
or at the beginning of a line, that line will be considered.
This behaviour is to make it easy to \"mark\" a line for
consideration without having to be in any specific position in
that line; anywhere in that line will do.  Make a copy of these
lines below the last line of the region, and make the original
lines into comments."
  (interactive "*r")
  (when (derived-mode-p 'prog-mode)
    (save-excursion
      (let* ((beg-bol (progn (goto-char beginning)
                             ;; Yes, include the line even if point
                             ;; was at the end of the line.
                             (beginning-of-line)
                             (point)))
             (end-eol (progn (goto-char end)
                             ;; Yes, include the line even if point was
                             ;; at the beginning of the line.
                             (end-of-line)
                             (point))))
        ;; Use buffer-substring instead of kill-ring-save because we
        ;; do not want the copied text to end up on the kill-ring.
        ;; The idea is to duplicate the lines, and not to save them
        ;; anywhere for yanking later.
        (let ((copied-lines (buffer-substring beg-bol end-eol)))
          (goto-char beg-bol)
          (insert copied-lines)
          (newline 1 nil)
          (ignore-errors (comment-region beg-bol end-eol)))))
    ;; Account for save-excursion behavior at beginning of line.
    (when (and (bolp) (equal beginning (point)))
      (forward-line (1+ (count-lines beginning end))))))

(defun raghu/duplicate-line-comment-original (arg)
  "Duplicate current line and make the original a comment.

Starting from and including the current line, duplicate ARG
lines, and convert the original lines into comments.  If ARG is a
positive integer, work on ARG lines below.  If ARG is a negative
integer, work on ARG lines above.  In either case, ARG includes
the current line.  So, to work on the current line only, ARG must
be 1 (or -1).  If ARG is 0, do nothing."
  (interactive "*p")
  (when (derived-mode-p 'prog-mode)
    (unless (= 0 arg)
      (save-excursion
        (if (< arg 0) (forward-line 1) (forward-line 0))
        (let ((original-beginning (point)))
          ;; If arg is negative (that is, we want to work on lines
          ;; *above* the current line), then (forward-line 1) will have
          ;; been called above.  If that call puts point at the end of
          ;; the same line rather than the beginning of the next line,
          ;; it means that we are at the very last line, and that line
          ;; is not followed by a new line.  In this case, the
          ;; expression (and (eolp) (not bolp)) will evaluate to t.
          ;; Starting from the end of the line and including the
          ;; current, if we must copy arg lines above, we ought to move
          ;; up by one line lesser than (abs arg) lines.  Therefore, do
          ;; (1+ arg) as arg is negative.  Otherwise (that is, we are at
          ;; the beginning of the next line), we ought to move up by
          ;; (abs arg) lines.
          (forward-line (if (and (eolp) (not (bolp))) (1+ arg) arg))
          ;; Use buffer-substring instead of kill-ring-save because we
          ;; do not want the copied text to end up on the kill-ring.
          ;; The idea is to duplicate the lines, and not to save them
          ;; anywhere for yanking later.
          (let* ((original-end (point))
                 (copied-lines (buffer-substring original-beginning
                                                 original-end)))
            (goto-char (if (< arg 0) original-end original-beginning))
            ;; We do not want to insert text at the end of an existing
            ;; line.  The text inserted must go either above the first
            ;; of the copied lines, or below the last of the copied
            ;; lines, and never on the same line as the first or the
            ;; last. So insert a new line first in such cases.
            (unless (bolp) (newline 1 nil))
            (insert copied-lines)
            ;; After inserting the copied lines, if we are not at the
            ;; start of a newline (because the original text did not end
            ;; with a newline), then insert one now.  Doing so will
            ;; preserve any leading space (indentation) of the last of
            ;; the copied lines.
            (unless (bolp) (newline 1 nil))
            (ignore-errors (comment-region original-beginning original-end))))))
    ;; Account for save-excursion behavior at beginning of line.
    (when (and (> arg 0) (bolp)) (forward-line arg))))

(defun raghu/duplicate-and-comment-original (arg)
  "Duplicate lines, make the originals into comments.

Do work only if buffer is derived from a programming mode, as the
concept of comments are not clearly defined for other modes.  If
a region is active, call
`raghu/duplicate-region-comment-original' on the region.  If a
region is not active, and ARG is an integer, act on ARG lines by
calling `raghu/duplicate-line-comment-original' with argument
ARG.  If a region is not active, and ARG is a list, fetch its
`car', and if the result is an integer, say N, act on N lines by
calling `raghu/duplicate-line-comment-original' with argument N.
If a region is not active, and ARG is neither an interger nor a
list, call `raghu/duplicate-line-comment-original' with argument
1."
  (interactive "*P")
  (when (derived-mode-p 'prog-mode)
    (cond ((use-region-p)
           (raghu/duplicate-region-comment-original (region-beginning)
                                                    (region-end)))
          ((integerp arg)
           (raghu/duplicate-line-comment-original arg))
          ((null arg)
           (raghu/duplicate-line-comment-original 1))
          ((listp arg)
           (let ((n (car arg)))
             (when (integerp n) (raghu/duplicate-line-comment-original n))))
          (t
           (raghu/duplicate-line-comment-original 1)))))
(define-key global-map (kbd "C-c I") #'raghu/duplicate-and-comment-original)

(provide 'raghu/duplicate-and-comment)
