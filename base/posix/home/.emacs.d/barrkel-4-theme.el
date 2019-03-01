(deftheme barrkel-4
  "Designed to work with customized terminal colors set barrkel-4")

(if (display-graphic-p)
    (setq color-yellow "#f57900"
          color-bright-yellow "#fce94f"
          color-red "#ff6464"
          color-bright-red "#ef2929"
          color-bright-green "#73d216"
          color-green "#4e9a06"
          color-blue "#729fcf"
          color-bright-blue "#204a87"
          color-white "#babdb6"
          color-bright-white "#eeeeec"
          color-magenta "#ad7fa8"
          color-bright-magenta "#1d324b"
          color-black "#1a2022"
          color-bright-black "#2e3436"
          color-bright-cyan "#555753"
          color-cyan "#888a85")
  (setq color-black "black"
        color-white "white"
        color-red "red"
        color-green "green"
        color-blue "blue"
        color-yellow "yellow"
        color-cyan "cyan"
        color-magenta "magenta"
        color-bright-black "brightblack"
        color-bright-white "brightwhite"
        color-bright-red "brightred"
        color-bright-green "brightgreen"
        color-bright-blue "brightblue"
        color-bright-yellow "brightyellow"
        color-bright-cyan "brightcyan"
        color-bright-magenta "brightmagenta"))

(custom-theme-set-faces
 'barrkel-4
 `(default
    ((t
      (:inherit nil
                :stipple nil
                :background ,color-black
                :foreground ,color-white
                :inverse-video nil
                :box nil
                :strike-through nil
                :overline nil
                :underline nil
                :slant normal
                :weight normal
                ,@(cond ((eq window-system 'ns)
                         '(:height 110))
                        (t
                         '(:height 80)))
                :width normal
                ;; , on its own is unquote inside a backtick quote; ,@ is a list splicing unquote
                ,@(cond ((eq window-system 'w32)
                         '(:foundry "outline" :family "Dina TTF"))
                        ((eq window-system 'x)
                         '(:foundry "windows" :family "Dina"))
                        ((eq window-system 'ns)
                         '(:foundry "windows" :family "Andale Mono"))
                        (t
                         '(:foundry "default" :family "default")))))))
 `(cursor
   ((((background light)) (:background ,color-black)) (((background dark)) (:background ,color-white))))
 `(fixed-pitch
   ((t (:family "Monospace"))))
 `(variable-pitch
   ((t (:family "Sans Serif"))))
 `(escape-glyph
   ((t (:foreground ,color-magenta))))
 `(minibuffer-prompt
   ((t (:foreground ,color-blue))))
 `(highlight
   ((t (:background ,color-bright-yellow))))
 `(region
   ((t (:background ,color-bright-blue))))
 `(shadow
   ((t (:foreground ,color-bright-cyan))))
 `(secondary-selection
   ((t (:background ,color-bright-magenta))))
 `(trailing-whitespace
   ((t (:background ,color-bright-red))))
 `(font-lock-builtin-face
   ((t (:foreground ,color-blue))))
 `(font-lock-comment-delimiter-face
   ((t (:inherit font-lock-comment-face))))
 `(font-lock-comment-face
   ((t (:foreground ,color-green :weight bold))))
 `(font-lock-constant-face
   ((t (:foreground ,color-white :weight bold))))
 `(font-lock-doc-face
   ((t (:foreground ,color-cyan))))
 `(font-lock-function-name-face
   ((t (:foreground ,color-red))))
 `(font-lock-keyword-face
   ((t (:foreground ,color-blue))))
 `(font-lock-negation-char-face
   ((t nil)))
 `(font-lock-preprocessor-face
   ((t (:foreground ,color-bright-cyan :weight bold))))
 `(font-lock-regexp-grouping-backslash
   ((t (:inherit bold))))
 `(font-lock-regexp-grouping-construct
   ((t (:inherit bold))))
 `(font-lock-string-face
   ((t (:foreground ,color-green))))
 `(font-lock-type-face
   ((t (:foreground ,color-magenta))))
 `(font-lock-variable-name-face
   ((t (:foreground ,color-white))))
 `(font-lock-warning-face
   ((t (:inherit error))))
 `(button
   ((t (:inherit link))))
 `(link
   ((t (:foreground ,color-blue :underline t))))
 `(link-visited
   ((t (:inherit link :foreground ,color-bright-blue))))
 `(fringe
   ((t (:background ,color-bright-black))))
 `(header-line
   ((t (:inherit mode-line))))
 `(mode-line ((t (:background ,color-bright-magenta :foreground ,color-white))))
 `(mode-line-buffer-id
   ((t (:foreground ,color-bright-green :weight bold))))
 `(mode-line-emphasis
   ((t (:weight bold))))
 `(mode-line-highlight
   ((t (:box (:line-width 2 :color ,color-bright-blue)))))
 `(mode-line-inactive
   ((t (:inherit mode-line :background ,color-bright-black :foreground ,color-cyan))))

 `(isearch-fail ((t (:background ,color-bright-red))))
 `(isearch ((t (:background ,color-blue :foreground ,color-black))))
 `(lazy-highlight ((t (:background ,color-bright-blue :foreground ,color-white))))

 `(match
   ((t (:background ,color-bright-green))))
 `(next-error
   ((t (:inherit region))))
 `(query-replace
   ((t (:inherit isearch))))

 `(linum ((t (:foreground ,color-bright-cyan :background ,color-black :underline nil :slant normal :weight normal))))

 `(show-paren-match ((t (:foreground ,color-bright-yellow :weight bold))))

 ;; helm
 `(helm-source-header ((t (:inherit mode-line))))
 `(helm-selection ((t (:background ,color-bright-blue :foreground ,color-bright-white))))
 `(helm-selection-line ((t (:background ,color-bright-blue :foreground ,color-bright-white))))
 `(helm-candidate-number ((t (:foreground ,color-blue :background nil))))

 ;; magit
 `(magit-blame-heading ((t (:foreground ,color-yellow :background ,color-bright-black :weight normal))))

 ;; ruby
 `(enh-ruby-op-face ((t (:foreground ,color-bright-white :background nil))))
 `(enh-ruby-regexp-delimiter-face ((t (:foreground ,color-bright-white :background nil))))
 `(enh-ruby-heredoc-delimiter-face ((t (:foreground ,color-bright-white :background nil))))
 `(enh-ruby-regexp-face ((t (:foreground ,color-yellow :background nil))))
 `(enh-ruby-string-delimiter-face ((t (:foreground ,color-green :background nil))))

 ;; whitespace
 `(whitespace-space ((t (:foreground ,color-bright-black))))
 `(whitespace-tab ((t (:foreground ,color-bright-black))))
 `(whitespace-newline ((t (:foreground ,color-bright-black))))
 )

(provide-theme 'barrkel-4)
