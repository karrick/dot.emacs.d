(setenv "M2_HOME" "/usr/local/apache-maven")
(setenv "M2" (concat (getenv "M2_HOME") "/bin"))
(setenv "PATH" (concat (getenv "M2") ":" (getenv "PATH")))

(provide 'maven)
