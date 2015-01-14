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
                   ;; (setq erc-kill-buffer-on-part t
                   ;;       erc-kill-queries-on-quit t
                   ;;       erc-kill-server-buffer-on-quit t
                   ;;       erc-server-auto-reconnect nil)
                   (when t
                     (require 'erc-join)
                     (erc-autojoin-enable))
                   (when t
                     (require 'erc-services)
                     (load "~/.ercpass.el")
                     (setq erc-prompt-for-nickserv-password nil)
                     (erc-services-mode 1)
                     (setq erc-autojoin-timing 'ident))
                   (when t
                     (require 'erc-log)
                     (setq erc-log-channels-directory "~/logs"
                           erc-save-buffer-on-part nil
                           erc-save-queries-on-quit nil
                           erc-log-write-after-send t
                           erc-log-write-after-insert t
                           erc-enable-logging 'erc-log-all-but-server-buffers)
                     (erc-log-enable))
                   (when t
                     (require 'erc-autoaway)
                     (setq erc-auto-discard-away t
                           erc-auto-set-away t
                           erc-autoaway-idle-seconds 300)
                     (erc-autoaway-enable)))

(provide 'irc)
