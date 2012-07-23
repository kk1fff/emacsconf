;; Customize variables
(custom-set-variables
 '(inhibit-startup-screen t)
 '(recentf-max-saved-items 100))

;; Highlight current line
(global-hl-line-mode 1)

;; [Additional Package] F3 to highlight current symbol.
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-prev)

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
(progn
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;  (menu-bar-mode -1)
  (scroll-bar-mode -1)
)
(setq-default truncate-lines t)

;; [Additional Package] Use column indent
; (require 'fill-column-indicator)
; (setq fci-rule-width 1)
; (setq fci-rule-color "black")
; (add-hook 'after-change-major-mode-hook 'fci-mode)
; (setq-default fill-column 80)

;; Show line number
(global-linum-mode 1)

;; highlight 80 column
(require 'column-marker)
(add-hook 'after-change-major-mode-hook
          (lambda () (interactive) (column-marker-2 80)))

;; [Additional Package] Autocomplete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

;; Coding style
;; Using google style
;; We're at Mozilla, so we don't use Google's style.
; (require 'google-c-style)
; (add-hook 'c-mode-common-hook 'google-set-c-style)
; (add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Mozilla's style
(require 'mozilla-c-style)
(add-hook 'c-mode-common-hook 'mozilla-c-mode-style-hook)

;; My additional style
(setq-default basic-offset 2)
(setq-default tab-width 2)
(setq-default tab-stop 2)
(setq-default indent-tabs-mode nil)
(setq-default js-indent-level 2)
(c-set-offset 'innamespace 0)

;; Use c++-mode as default mode of .h file
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Markdown
(autoload 'markdown-mode "markdown-mode.el" 
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))

;; Popup kill ring.
(require 'popup)
(require 'pos-tip)
(require 'popup-kill-ring)
(global-set-key "\M-y" 'popup-kill-ring)
(setq popup-kill-ring-interactive-insert t)

;; Add revert-all-buffers command.
(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )

;; Enable history
(require 'recentf)
(recentf-mode 1)

;; Enable js-mode when opening a jsm file
(setq auto-mode-alist (cons '("\\.jsm" . js-mode) auto-mode-alist))

;; Load color theme
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-tty-dark)
