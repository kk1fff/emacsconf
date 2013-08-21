(require 'package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install package from builtin repository

;; Refresh
(package-initialize)
(package-refresh-contents)

;; Define the package list.
(defvar my-packages '(js2-mode
                      rainbow-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up 3rd party sources.
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Refresh
(package-initialize)
(package-refresh-contents)

;; Define the package list.
(defvar my-packages '(haskell-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install el-get package manager.
;(url-retrieve
; "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
; (lambda (s)
;   (let (el-get-master-branch)
;     (goto-char (point-max))
;     (eval-print-last-sexp))))
;
;;; Install lua-mode
;(el-get-install 'lua-mode)

;; close
(kill-emacs)
