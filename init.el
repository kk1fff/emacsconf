;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize variables
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define my customized key mode, keys are defined with each function.
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight current line
(global-hl-line-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [Additional Package] F3 to highlight current symbol.
(require 'highlight-symbol)
(define-key my-keys-minor-mode-map (kbd "C-c <") 'highlight-symbol-prev)
(define-key my-keys-minor-mode-map (kbd "C-c >") 'highlight-symbol-next)
(define-key my-keys-minor-mode-map (kbd "C-c /") 'highlight-symbol-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show file full path in title bar
(setq-default frame-title-format
   (list '((buffer-file-name " %f"
             (dired-directory
              dired-directory
              (revert-buffer-function " %b"
              ("%b - Dir:  " default-directory)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shows parenthesis 
(show-paren-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shows column number, line number.
(column-number-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disable toolbar
(progn
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
)
(setq-default truncate-lines t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [Additional Package] Use column indent
; (require 'fill-column-indicator)
; (setq fci-rule-width 1)
; (setq fci-rule-color "white")
; (add-hook 'after-change-major-mode-hook 'fci-mode)
; (setq-default fill-column 80)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show line number
(global-linum-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight 80 column
; (require 'column-marker)
; (add-hook 'after-change-major-mode-hook
;          (lambda () (interactive) (column-marker-2 80)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [Additional Package] Autocomplete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coding style
;; Using google style
;; We're at Mozilla, so we don't use Google's style.
; (require 'google-c-style)
; (add-hook 'c-mode-common-hook 'google-set-c-style)
; (add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mozilla's style
(require 'mozilla-c-style)
; (add-hook 'c-mode-common-hook 'mozilla-c-mode-style-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My additional style
(setq-default basic-offset 2)
(setq-default tab-width 2)
(setq-default tab-stop 2)
(setq-default indent-tabs-mode nil)
(setq-default js-indent-level 2)
(c-set-offset 'innamespace 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use c++-mode as default mode of .h file
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
(autoload 'markdown-mode "markdown-mode.el" 
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popup kill ring.
(require 'popup)
(require 'pos-tip)
(require 'popup-kill-ring)
(global-set-key "\M-y" 'popup-kill-ring)
(setq popup-kill-ring-interactive-insert t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add revert-all-buffers command.
(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable history
(require 'recentf)
(recentf-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable js-mode when opening a jsm file
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq auto-mode-alist (cons '("\\.jsm" . js2-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sjs" . js2-mode) auto-mode-alist))
(eval-after-load "js2-mode"
  '(progn
     (setq js2-missing-semi-one-line-override t)
     (setq-default js2-basic-offset 2) ; 2 spaces for indentation (if you prefer 2 spaces instead of default 4 spaces for tab)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load minimap
; (require 'minimap)

; (defun ww-next-gtag ()
;   "Find next matching tag, for GTAGS."
;   (interactive)
;   (let ((latest-gtags-buffer
;          (car (delq nil  (mapcar (lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
;                                  (buffer-list)) ))))
;     (cond (latest-gtags-buffer
;            (switch-to-buffer latest-gtags-buffer)
;            (forward-line)
;            (gtags-select-it nil))
;           ) ))
; (global-set-key "\M-;" 'ww-next-gtag)   ;; M-; cycles to next result, after doing M-. C-M-. or C-M-,
; (global-set-key "\M-." 'gtags-find-tag) ;; M-. finds tag
; (global-set-key [(control meta .)] 'gtags-find-rtag)   ;; C-M-. find all references of tag
; (global-set-key [(control meta ,)] 'gtags-find-symbol) ;; C-M-, find all usages of symbol.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-nav
(require 'nav)
(nav-disable-overeager-window-splitting)
;; Optional: set up a quick key to toggle nav
(global-set-key [f8] 'nav-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popup global mark ring.
(require 'popup-global-mark-ring)
(global-set-key "\C-c\C-g" 'popup-global-mark-ring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move to minibuffer
(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))
(global-set-key (kbd "<f7>") 'switch-to-minibuffer-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using buffer menu instead of buffer list.
(global-set-key "\C-x\C-b" 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
(require 'helm-config)
(helm-mode 1)
;; Display helm in new vertically separated window.
(setq helm-display-function
      (lambda (buf)
        (split-window-vertically)
        (other-window 1)
        (switch-to-buffer buf)))

;; Enable helm-gtags
; (require 'helm-gtags)
; (add-hook 'c-mode-hook (lambda () (helm-gtags-mode)))

;; helm-etags+
; (require 'helm-etags+)

;; id-utils
; (require 'idutils "id-utils")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ace Jump Mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Centered cursor
; (and
;  (require 'centered-cursor-mode)
;  (global-centered-cursor-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Powerline
(require 'powerline)
(powerline-default-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dot mode
(require 'graphviz-dot-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
(set-default 'custom-safe-themes t)
(load-theme 'patrick-local)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jade Mode
(require 'sws-mode)
(require 'jade-mode)    
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHP Mode
(require 'php-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi-web-mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(define-key my-keys-minor-mode-map (kbd "C-c r") 'iresize-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on my mode by default except in minibuffer.
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)
(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start server when emacs start-up
(server-force-delete)
(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic mode
(semantic-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-breadcrumbs-mode 1)
(global-semantic-highlight-func-mode 1)
(global-semantic-idle-summary-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subword mode
(global-subword-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; indent guide mode
(require 'indent-guide)
(indent-guide-global-mode)



;; -----------------------------------------------------------------------------
;; Load packages. Package initialization must be written after this.
(setq package-enable-at-startup nil)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ggtags
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'js2-mode 'js-mode)
              (ggtags-mode 1))))
(global-set-key (kbd "C-c k") 'ggtags-find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-c =") 'er/expand-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; -----------------------------------------------------------------------------
;; keybinds
(global-set-key (kbd "C-c o") 'ff-find-other-file)
(global-set-key (kbd "C-c f") 'helm-imenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet
(yas-global-mode 1)
