;;; browser-open -- open URL in browser

;;; Commentary:

;;; Code:

(defun browser-open (url)
  "Open URL in browser.

When URL is empty, and region is selected, open URL enclosed by
region.  Otherwise, prompt user for URL to open."
  (interactive "P")
  (browse-url
   (cond ((stringp url) url)
	 ((use-region-p) (buffer-substring (region-beginning) (region-end)))
	 (t (read-string "URL: ")))))

(provide 'browser-open)

;;; browser-open.el ends here
