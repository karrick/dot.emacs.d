(require 'irc)

(setq erc-anonymous-login nil)
(add-to-list 'erc-keywords "\\bkmcdermo\\b")
(add-to-list 'erc-autojoin-channels-alist
             '(".*\.corp\.linkedin\.com" . (
                                            "#ansible"
                                            "#coresre"
                                            "#go"
                                            "#infra"
                                            "#inmon"
                                            "#metrics"
                                            "#noc"
                                            "#nyc"
                                            "#nyc-sre"
                                            "#python"
                                            "#sand"
                                            "#secsre"
                                            "#security"
                                            "#sre"
                                            "#sre-infra"
                                            )))

(provide 'irc-linkedin)
