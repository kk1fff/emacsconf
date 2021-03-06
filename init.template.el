(add-to-list 'load-path "~/.emacs.d/loc_pkg/misc")
(add-to-list 'custom-theme-load-path "~/.emacs.d/loc_themes")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize variables

(add-to-list 'exec-path "/usr/local/bin")

(custom-set-variables
  ; Don't display start-up screen
  '(inhibit-startup-screen t)
  ; Recent file
  '(recentf-max-saved-items 100)
  ; Highlight color
  '(highlight-symbol-foreground-color "white")
  ; Symbol highlight color list
  '(highlight-symbol-colors
    '("purple1" "orange4" "SeaGreen4" "medium blue" "saddle brown"
      "orange red" "magenta" "LavenderBlush4" "blue2" "PaleVioletRed3"
      "tomato2")))

;; Highlight current line
(global-hl-line-mode 1)

;; Show file full path in title bar
(setq-default frame-title-format
   (list '((buffer-file-name " %f"
             (dired-directory
              dired-directory
              (revert-buffer-function " %b"
              ("%b - Dir:  " default-directory)))))))

;; Shows parenthesis 
(show-paren-mode 1)

;; Shows column number, line number.
(column-number-mode 1)

;; disable toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; no wrap
(setq-default truncate-lines t)

;; Show line number
(global-linum-mode 1)

;; Use ibuffer to replace buffer menu.
(global-set-key "\C-x\C-b" 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; Theme
(set-default 'custom-safe-themes t)
(load-theme 'patrick-local)

;; Start server when emacs start-up
(server-force-delete)
(server-start)

;; Semantic mode
(semantic-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-breadcrumbs-mode 1)
(global-semantic-highlight-func-mode 1)
(global-semantic-idle-summary-mode 1)
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq ac-sources '(ac-source-semantic-raw))))

;; subword mode
(global-subword-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init packages
(setq package-enable-at-startup t)
(package-initialize)

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

(let ((init-ed nil))
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (message "Package %s missing, installing..." p)
      (when (not init-ed)
        (add-to-list 'package-archives
                     '("marmalade" . "http://marmalade-repo.org/packages/") t)
        (add-to-list 'package-archives
                     '("melpa" . "http://melpa.milkbox.net/packages/") t)
        (package-refresh-contents)
        (setq init-ed t))
      (package-install p))))

;; Easy PG
(require 'epa-file)
(epa-file-enable)

;; highlight current symbol.
(require 'highlight-symbol)

;; Markdown
(autoload 'markdown-mode "markdown-mode.el" 
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;; Enable history
(require 'recentf)
(recentf-mode 1)

;; Enable js-mode when opening a jsm file
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq auto-mode-alist (cons '("\\.jsm\\'" . js2-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sjs\\'" . js2-mode) auto-mode-alist))
(eval-after-load "js2-mode"
  '(progn
     (setq js2-missing-semi-one-line-override t)
     (setq-default js2-basic-offset 4) ; 4 spaces for indentation (if you prefer 2 spaces instead of default 4 spaces for tab)

     ;; following is from http://www.emacswiki.org/emacs/Js2Mode
     (add-hook 'js2-post-parse-callbacks 'my-js2-parse-global-vars-decls)
     (defun my-js2-parse-global-vars-decls ()
       (let ((btext (replace-regexp-in-string
                     ": *true" " "
                     (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
         (setq js2-additional-externs
               (split-string
                (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
                " *, *" t))
         ))
     ))

;; ibuffer filter
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*")

;; helm
(require 'helm-config)
(helm-mode 1)
(setq helm-display-function ; Display helm in new vertically separated window.
      (lambda (buf)
        (split-window-vertically)
        (other-window 1)
        (switch-to-buffer buf)))
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
;; Ace Jump Mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Dot mode
(require 'graphviz-dot-mode)

;; Multi-web-mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;; popup-kill-ring
(require 'pos-tip)
(require 'popup-kill-ring)
(global-set-key "\M-y" 'popup-kill-ring)
(setq popup-kill-ring-interactive-insert t)

;; autocomplete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")

;; ggtags
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'js2-mode 'js-mode)
              (ggtags-mode 1))))
(global-set-key (kbd "C-c k") 'ggtags-find-file)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-c =") 'er/expand-region)

;; ibuffer-vc-hook
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (define-ibuffer-sorter pathname
              "Sort the buffers by their pathnames."
             (:description "Sort the buffers by their pathname.")
             (string-lessp
              (with-current-buffer (car a)
                (or buffer-file-name
                    (if (eq major-mode 'dired-mode)
                        (expand-file-name dired-directory))
                    ;; so that all non pathnames are at the end
                    "~"))
              (with-current-buffer (car b)
                (or buffer-file-name
                    (if (eq major-mode 'dired-mode)
                        (expand-file-name dired-directory))
                    ;; so that all non pathnames are at the end
                    "~"))))
            (ibuffer-do-sort-by-pathname)))

;; Rust
(require 'rust-mode)
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizes that runs after loading extenstions


;; keybinds

;; Turn on my mode by default except in minibuffer.
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)
(define-key my-keys-minor-mode-map (kbd "C-c <") 'highlight-symbol-prev)
(define-key my-keys-minor-mode-map (kbd "C-c >") 'highlight-symbol-next)
(define-key my-keys-minor-mode-map (kbd "C-c /") 'highlight-symbol-at-point)
(define-key my-keys-minor-mode-map (kbd "C-c r") 'iresize-mode)
(define-key my-keys-minor-mode-map (kbd "C-c o") 'ff-find-other-file)
(define-key my-keys-minor-mode-map (kbd "C-c f") 'helm-imenu)
(my-keys-minor-mode 1)
(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

;; Org mode.
(add-hook 'org-mode-hook (lambda ()
                           (setq truncate-lines nil)
                           (visual-line-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In-init script

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

;; Move to minibuffer
(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))
(global-set-key (kbd "") 'switch-to-minibuffer-window)

;; Switching between windows.
(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))
(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))
(global-set-key (kbd "C-x o") 'select-next-window)
(global-set-key (kbd "C-x O")  'select-previous-window)

;; Define resizing mode.
(defvar iresize-mode-map 
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "H") 'enlarge-window)
    (define-key m (kbd "h") 'shrink-window)
    (define-key m (kbd "W") 'enlarge-window-horizontally)
    (define-key m (kbd "w") 'shrink-window-horizontally)
    (define-key m (kbd "C-g") 'iresize-mode)
    m))
(define-minor-mode iresize-mode
  :initial-value nil
  :lighter " IResize"
  :keymap iresize-mode-map
  :group 'iresize)
(provide 'iresize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coding style

(setq-default basic-offset 4)
(setq-default tab-width 4)
(setq-default tab-stop 4)
(setq-default indent-tabs-mode nil)
(setq-default js-indent-level 4)
(c-set-offset 'innamespace 0)

;; Use c++-mode as default mode of .h file
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(require 'highlight-indentation)
(set-face-background 'highlight-indentation-face "#010101")
(set-face-background 'highlight-indentation-current-column-face "#0505ff")
(add-hook 'prog-mode-hook 'highlight-indentation-mode)
