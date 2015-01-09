(configure-package '(erc)
                   (defmacro erc-bouncer-connect (command server port nick ssl pass)
                     "Create interactive command `command', for connecting to an IRC server. The
   command uses interactive mode if passed an argument."
                     (fset command
                           `(lambda (arg)
                              (interactive "p")
                              (if (not (= 1 arg))
                                  (call-interactively 'erc)
                                (let ((erc-connect-function ',(if ssl
                                                                  'erc-open-ssl-stream
                                                                'open-network-stream)))
                                  (erc :server ,server :port ,port :nick ,nick :password ,pass))))))
                   (setq erc-server-auto-reconnect nil)

                   ;; (add-to-list 'erc-modules 'notifications)

                   ;; nickserv support
                   (when t
                     (setq erc-autojoin-timing 'ident)
                     (load "~/.ercpass.el")
                     (require 'erc-services)
                     (erc-services-mode 1)
                     (setq erc-prompt-for-nickserv-password nil)
                     (add-hook 'erc-after-connect
                               '(lambda (server nick)
                                  (cond
                                   ((string-match "\.corp\.linkedin\.com" server)
                                    (message "sending nickserv identity")
                                    (erc-message "PRIVMSG" (concat "NickServ identify " linkedin-kmcdermo))))))
                     )

                   (require 'erc-join)
                   (erc-autojoin-enable)

                   (erc-match-enable)
                   (setq erc-track-exclude-server-buffer t)
                   (setq erc-current-nick-highlight-type 'nick)
                   (setq erc-keywords '("\\berc[-a-z]*\\b" "\\bemms[-a-z]*\\b"))
                   (setq erc-track-use-faces t)
                   (setq erc-track-faces-normal-list '(erc-current-nick-face erc-keyword-face))
                   (setq erc-track-priority-faces-only 'all)
                   (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                                   "324" "329" "332" "333" "353" "477"))

                   (defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
                     (if (erc-query-buffer-p)
                         (setq ad-return-value (intern "erc-current-nick-face"))
                       ad-do-it))

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
                                                           erc-prompt-face))
                     (defadvice erc-track-modified-channels (around erc-track-modified-channels-promote-query activate)
                       (if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'nil))
                       ad-do-it
                       (if (erc-query-buffer-p) (setq erc-track-priority-faces-only 'all)))
                     )
                   )

(provide 'irc)
