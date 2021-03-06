(deftheme patrick-local "Local theme from Patrick.")

(custom-theme-set-faces
 'patrick-local
 '(escape-glyph ((t (:box (:line-width 1 :color "blue" :style released-button) :foreground "#a40000" :background "gold"))))
 '(minibuffer-prompt ((t (:foreground "white" :background "#BF3249" :weight bold))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((t (:foreground "black" :background "#8cc4ff"))))
 '(trailing-whitespace ((t (:background "#ef2929"))))
 '(font-lock-builtin-face ((t (:foreground "aquamarine1"))))
 '(font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face) :foreground "#B09900"))))
 '(font-lock-comment-face ((t (:slant italic :foreground "#00a000"))))
 '(font-lock-constant-face ((t (:foreground "PeachPuff1" :weight bold))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "SkyBlue1"))))
 '(font-lock-keyword-face ((t (:foreground "deep sky blue"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "white"))))
 '(font-lock-type-face ((t (:foreground "#C377E0"))))
 '(font-lock-variable-name-face ((t (:foreground "#A3C478"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "red" :inherit (error)))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:foreground "DeepSkyBlue1" :underline t))))
 '(link-visited ((t (:inherit link :foreground "DarkOrchid1" :underline t))))
 '(header-line ((t (:inherit mode-line :background "#383838" :foreground "grey90" :box nil))))
 '(tooltip ((t (:foreground "black" :background "lightyellow" :inherit (variable-pitch)))))
 '(isearch ((t (:background "#ce5c00" :foreground "#ffffff"))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((t (:background "#A83B00" :foreground "white"))))
 '(match ((t (:background "SkyBlue"))))
 '(next-error ((t (:background "SkyBlue" :inherit (region)))))
 '(query-replace ((t (:foreground "black" :background "white" :inherit (isearch)))))
 '(indent-guide-face ((t (:foreground "gray40"))))

 '(highlight-indentation-face ((t (:background "#e3e3d3"))))
 '(highlight-indentation-current-column-face ((t (:background "#c3b3b3"))))
 
 ;; Mode line
 '(mode-line ((t (:box nil
                  :foreground "gray85"
                  :background "gray30"))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:box nil
                           :weight light
                           :foreground "gray40"
                           :background "gray20"
                           :inherit (mode-line)))))

 ;; Popup
 ;; '(popup-face ((t (:foreground "gray50" :background "gray10"))))
 ;; '(popup-selection-face ((t (:foreground "while"
 ;;                             :background "PaleGreen4"))))
 ;; '(popup-menu-selection-face ((t (:foreground "gray50" :background "while"))))

 ;; org mode.
 '(org-level-1 ((t (:font "Noto Serif 17"
                    :foreground "medium sea green"))))
 '(org-level-2 ((t (:font "Noto Serif 15"
                    :foreground "aquamarine"))))
 '(org-level-3 ((t (:font "Noto Serif 13"
                    :foreground "cornflower blue"))))
 '(org-level-4 ((t (:font "Noto Sans 12"
                    :foreground "LightBlue2"))))

 ;; global setting.
 '(region ((t (:background "gray70" :foreground "gray10"))))
 '(cursor ((t (:background "yellow"))))
 '(highlight ((t (:background "#04016b"))))
 '(default ((((type x))
             (:font "Fantasque Sans Mono 11"))
            (((type graphic))
             (:font "Source Code Pro 10"))))
 '(default ((t (:inherit nil
                :background "#202020"
                :foreground "#d0d0d8"
                :inverse-video nil
                :box nil
                :strike-through nil
                :overline nil
                :underline nil
                :slant normal
                :weight normal)))))

(provide-theme 'patrick-local)
