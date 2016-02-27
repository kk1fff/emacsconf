(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install package from builtin repository

;; Refresh
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-refresh-contents)

;; Define the package list.
(defvar my-packages '(ggtags
                      js2-mode
                      rainbow-mode
                      haskell-mode
                      lua-mode
                      expand-region
                      rust-mode
                      ibuffer-vc
                      emacs-eclim
                      popup
                      go-mode
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; close
(kill-emacs)
