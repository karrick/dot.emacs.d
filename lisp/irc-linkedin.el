(require 'irc)

(erc-bouncer-connect erc-linkedin "irc.corp.linkedin.com" 6667 "kmcdermo" nil nil)
(erc-bouncer-connect erc-linkedin-znc "lva1-sreircpxy01.corp.linkedin.com" 1337 "kmcdermo" t "what's my password?")

(add-to-list 'erc-autojoin-channels-alist
             '(".*\.corp\.linkedin\.com" . (
                                            "#metrics"
                                            ;; "#alert-onboarding"
                                            "#ansible"
                                            ;; "#central"
                                            "#coresre"
                                            ;; "#couchbase"
                                            ;; "#databus"
                                            ;; "#dba"
                                            "#go"
                                            "#infra"
                                            "#metrics"
                                            ;; "#money"
                                            ;; "#netops"
                                            "#noc"
                                            "#nyc"
                                            "#nyc-sre"
                                            "#python"
                                            ;; "#release"
                                            "#salt"
                                            "#sand"
                                            "#secsre"
                                            "#security"
                                            ;; "#siteops"
                                            "#sre"
                                            "#sre-infra"
                                            ;; "#sysops"
                                            ;; "#tools-sre"
                                            ;; "#trafficshift"
                                            )))

(dolist (pal '("fsillima" "gleffler" "nberry"))
  (add-to-list 'erc-pals pal))             

(provide 'irc-linkedin)
