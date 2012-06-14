(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(defvar my-packages '(minimap clojure-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
