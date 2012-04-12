(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(inhibit-startup-screen t)
 '(recentf-max-saved-items 100))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/ecb-2.40")
(add-to-list 'load-path "~/.emacs.d/themes")

;; NXHtml mode
; (load "~/.emacs.d/nxhtml/autostart.el")

;; highlight current line
(global-hl-line-mode 1)

;; highlight symbol
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-prev)

; Show file full path in title bar
(setq-default frame-title-format
   (list '((buffer-file-name " %f"
             (dired-directory
              dired-directory
              (revert-buffer-function " %b"
              ("%b - Dir:  " default-directory)))))))

; Shows parenthesis 
(show-paren-mode 1)

; Shows column number
(column-number-mode 1)

; Change default colors
(set-background-color "grey14")
(set-foreground-color "white")
(set-cursor-color "white")

; disable toolbar
(progn
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;  (menu-bar-mode -1)
  (scroll-bar-mode -1)
)
(setq-default truncate-lines t)

; use column indent
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "white")
(add-hook 'after-change-major-mode-hook 'fci-mode)
(setq-default fill-column 80)

; show line number
(global-linum-mode 1)

; highlight 80 column
(require 'highlight-80+)

;; Cscope, commented out cause xgtags is in use.
;(require 'xcscope)
;(add-hook 'java-mode-hook (function cscope:hook))

;; cedet, comment out if there's a system cedet.
; (load-file "~/local/cedet/common/cedet.el")
; (global-ede-mode 1)                      ; Enable the Project management system
; (semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
; (global-srecode-minor-mode 1)            ; Enable template insertion menu

;; ecb
(require 'ecb)

;; Autocomplete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

;; Coding style
; Using google style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Customize coding style.
(setq-default basic-offset 2)
(setq-default tab-width 2)
(setq-default tab-stop 2)
(setq-default indent-tabs-mode nil)
(setq-default js-indent-level 2)
(c-set-offset 'innamespace 0)

;; Create dedicate windows for given buffer
(defun toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))
; Set pause key to toggle current window dedication
(global-set-key [pause] 'toggle-current-window-dedication)

;; GNU GLOBAL
(require 'xgtags)
(add-hook 'c-mode-common-hook
          (lambda () (xgtags-mode 1)))

;; Use c++-mode as default mode of .h file
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;; Enable history
(require 'recentf)
(recentf-mode 1)

;; Markdown
(autoload 'markdown-mode "markdown-mode.el" 
  "Major mode for editing Markdown files" t)

(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; Python
; Python Hook
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))

;; Color scheme 
; (require 'color-theme)
; (color-theme-initialize)
; (color-theme-robin-hood)

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