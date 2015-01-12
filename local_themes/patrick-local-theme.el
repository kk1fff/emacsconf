(deftheme patrick-local "Local theme from Patrick.")

(custom-theme-set-faces
 'patrick-local
 '(cursor ((t (:background "white"))))
 '(fixed-pitch ((t (:family "Source Code Pro"))))
 '(variable-pitch ((t (:family "Source Code Pro"))))
 '(escape-glyph ((t (:box (:line-width 1 :color "blue" :style released-button) :foreground "#a40000" :background "gold"))))
 '(minibuffer-prompt ((t (:foreground "white" :background "#BF3249" :weight bold))))
 '(highlight ((t (:background "#364239"))))
 '(region ((t (:background "yellow1" :foreground "#3b3b3b"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((t (:foreground "black" :background "#8cc4ff"))))
 '(trailing-whitespace ((t (:background "#ef2929"))))
 '(font-lock-builtin-face ((t (:foreground "#8787C4"))))
 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face) :foreground "#B09900" :background "#404040"))))
 '(font-lock-comment-face ((t (:slant italic :foreground "#827100" :background "#404040"))))
 '(font-lock-constant-face ((t (:foreground "#4F4FC9" :weight bold))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "#A100B0"))))
 '(font-lock-keyword-face ((t (:foreground "#816CEB"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#838A00"))))
 '(font-lock-type-face ((t (:foreground "#C377E0"))))
 '(font-lock-variable-name-face ((t (:foreground "#A3C478"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "red" :inherit (error)))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:foreground "DodgerBlue2" :underline t))))
 '(link-visited ((t (:inherit link :foreground "SlateBlue4" :underline t))))
 '(header-line ((t (:inherit mode-line :background "#383838" :foreground "grey90" :box nil))))
 '(tooltip ((t (:foreground "black" :background "lightyellow" :inherit (variable-pitch)))))
 '(mode-line ((t (:box nil :foreground "gray10" :background "gray70"))))
 '(mode-line-buffer-id ((t (:underline "red" :weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:box nil :weight light :foreground "#2e3436" :background "gray55" :inherit (mode-line)))))
 '(isearch ((t (:background "#ce5c00" :foreground "#ffffff"))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((t (:background "#A83B00" :foreground "white"))))
 '(match ((t (:background "SkyBlue"))))
 '(next-error ((t (:background "SkyBlue" :inherit (region)))))
 '(query-replace ((t (:foreground "black" :background "white" :inherit (isearch)))))
 '(indent-guide-face ((t (:foreground "gray40"))))
 '(powerline-active1 ((t (:inherit mode-line :background "gray20" :foreground "OliveDrab1"))))
 '(powerline-active2 ((t (:inherit mode-line :background "gray50"))))
 '(powerline-inactive1 ((t (:inherit mode-line :foreground "gray75" :background "gray45"))))
 '(powerline-inactive2 ((t (:inherit mode-line :foreground "gray75" :background "gray40"))))
 '(org-level-1 ((t (:font "Noto Serif 20" :foreground "gray30"))))
 '(org-level-2 ((t (:font "Noto Serif 18" :foreground "gray45"))))
 '(org-level-3 ((t (:font "Noto Serif 16" :foreground "gray55"))))
 '(org-level-4 ((t (:font "Noto Sans 14" :foreground "gray70"))))
 '(default ((t (:inherit nil
                :font "Source Code Pro 10"
                :background "#202020"
                :foreground "#c0c0c0"
                :inverse-video nil
                :box nil
                :strike-through nil
                :overline nil
                :underline nil
                :slant normal
                :weight normal)))))

(provide-theme 'patrick-local)
