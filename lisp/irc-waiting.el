(when nil
  (require 'erc-autoaway)
  (setq erc-auto-discard-away t
	erc-auto-set-away t
	erc-autoaway-idle-seconds 300)
  (erc-autoaway-enable))

(when nil
  ;; http://www.emacswiki.org/emacs/ErcLogging
  (setq erc-log-insert-log-on-open t)
  (defun erc-log-all-but-server-buffers (buffer)
    (save-excursion
      (save-window-excursion
	(not (erc-server-buffer-p))))))

(when nil
  (condition-case err
      (erc-spelling-mode 1)
    (message "cannot initialize erc-spelling: %s" (error-message-string err))))

(when nil
  (require 'smiley)
  (add-to-list 'smiley-regexp-alist '("\\(:-?]\\)\\W" 1 "forced"))
  (add-to-list 'smiley-regexp-alist '("\\s-\\(:-?/\\)\\W" 1 "wry"))
  (add-to-list 'smiley-regexp-alist '("\\(:-?(\\)\\W" 1 "sad"))
  (add-to-list 'smiley-regexp-alist '("\\((-?:\\)\\W" 1 "reverse-smile"))
  (add-to-list 'smiley-regexp-alist '("\\(:-?D\\)\\W" 1 "grin"))
  (add-to-list 'smiley-regexp-alist '("\\(:-?P\\)\\W" 1 "poke")))

(when nil            ; minimal distraction mode
  (setq erc-format-query-as-channel-p t
	erc-track-priority-faces-only 'all
	erc-track-faces-priority-list '(erc-error-face
					erc-current-nick-face
					erc-keyword-face
					erc-nick-msg-face
					erc-direct-msg-face
					erc-dangerous-host-face
					erc-notice-face
					erc-prompt-face)))

(add-to-list 'erc-modules 'notifications)
