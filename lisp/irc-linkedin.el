(require 'irc)

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

(provide 'irc-linkedin)
