;;; setup-codesearch --- codesearch configuration

;;; Commentary:

;;; Code:


(dolist (tuple '(
                 ("cgrep" "github.com/google/codesearch/cmd/...")
                 ("cindex" "github.com/google/codesearch/cmd/...")
                 ("csearch" "github.com/google/codesearch/cmd/...")
                 ))
  (unless (executable-find (car tuple))
    (message "Cannot find %s: `go get -u %s`" (car tuple) (cadr tuple))))

(require 'codesearch)

(provide 'setup-codesearch)

;;; setup-codesearch.el ends here
