(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/")) ; considered to be stable, but unreliable today
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(setq package-enable-at-startup nil)
(setq package-check-signature 'allow-unsigned)
(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(dolist (pkg '(
               auto-complete
               ;; auto-complete-config
               bash-completion
               edit-server
               erc
               expand-region
               fic-mode
               find-file-in-repository
               flycheck
               ;; go-autocomplete
               go-mode
               go-eldoc
               ;; go get -u github.com/google/codesearch/cmd/...
               ;; go get -u github.com/kisielk/errcheck
               ;; go get -u github.com/mdempsky/unconvert
               ;; go get -u github.com/nsf/gocode
               ;; go get -u github.com/rogpeppe/godef
               ;; go get -u golang.org/x/tools/cmd/goimports
               ;; go get -u golang.org/x/tools/cmd/guru
               go-rename ;; go get -u golang.org/x/tools/cmd/gorename
               golint ;; go get -u github.com/golang/lint/golint
               js2-mode
               json-mode
               ivy
               markdown-mode
               maxframe
               multiple-cursors
               ;; nxml-mode
               ;; psgml
               puppet-mode
               shell-command
               smart-tab
               wgrep
               wgrep-ack
               yaml-mode
               zenburn-theme
               ))
  (if (not (package-installed-p pkg))
      (condition-case err
          (package-install pkg)
        (error (message "Cannot install package: %s" err)))))
