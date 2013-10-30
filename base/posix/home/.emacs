;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TERMINAL FIXES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (string= (getenv "TERM") "screen.rxvt")
  (define-key input-decode-map "\e[a" (kbd "S-<up>"))
  (define-key input-decode-map "\e[b" (kbd "S-<down>"))
  (define-key input-decode-map "\e[7$" (kbd "S-<home>"))
  (define-key input-decode-map "\e[7^" (kbd "C-<home>"))
  (define-key input-decode-map "\e[7@" (kbd "C-S-<home>"))
  (define-key input-decode-map "\e[8$" (kbd "S-<end>"))
  (define-key input-decode-map "\e[8^" (kbd "C-<end>"))
  (define-key input-decode-map "\e[8@" (kbd "C-S-<end>"))
  (define-key input-decode-map "\e[5^" (kbd "C-<prior>"))
  (define-key input-decode-map "\e[6^" (kbd "C-<next>"))
  (define-key input-decode-map "\e\e[5^" (kbd "C-M-<prior>"))
  (define-key input-decode-map "\e\e[6^" (kbd "C-M-<next>"))
  (define-key input-decode-map "\e\e[5$" (kbd "M-S-<prior>"))
  (define-key input-decode-map "\e\e[6$" (kbd "M-S-<next>"))
  (define-key input-decode-map "\e\e[5~" (kbd "M-<prior>"))
  (define-key input-decode-map "\e\e[6~" (kbd "M-<next>"))
  
  (define-key input-decode-map "\e[3^" (kbd "C-<delete>"))
  (define-key input-decode-map "\eOa" (kbd "C-<up>"))
  (define-key input-decode-map "\eOb" (kbd "C-<down>"))
  (define-key input-decode-map "\eOd" (kbd "C-<left>"))
  (define-key input-decode-map "\eOc" (kbd "C-<right>")))

;; actually, mintty on windows
(when (string= (getenv "TERM") "xterm")
  (define-key input-decode-map "\e[1;5m" (kbd "C--")))

;; actually, screen in mintty
(when (string= (getenv "TERM") "screen")
  (define-key input-decode-map "\e[1;5F" (kbd "C-<end>"))
  (define-key input-decode-map "\e[1;2F" (kbd "S-<end>"))
  (define-key input-decode-map "\e[1;6F" (kbd "C-S-<end>"))
  (define-key input-decode-map "\e[1;5H" (kbd "C-<home>"))
  (define-key input-decode-map "\e[1;2H" (kbd "S-<home>"))
  (define-key input-decode-map "\e[1;6H" (kbd "C-S-<home>"))
  (define-key input-decode-map "\e[3;3~" (kbd "M-<delete>"))
  (define-key input-decode-map "\e[3;5~" (kbd "C-<delete>"))
  (define-key input-decode-map "\e[1;2A" (kbd "S-<up>"))
  (define-key input-decode-map "\e[1;2B" (kbd "S-<down>"))
  (define-key input-decode-map "\e[1;2C" (kbd "S-<right>"))
  (define-key input-decode-map "\e[1;2D" (kbd "S-<left>"))
  (define-key input-decode-map "\e[1;5A" (kbd "C-<up>"))
  (define-key input-decode-map "\e[1;5B" (kbd "C-<down>"))
  (define-key input-decode-map "\e[1;5C" (kbd "C-<right>"))
  (define-key input-decode-map "\e[1;5D" (kbd "C-<left>"))
  (define-key input-decode-map "\e[1;6A" (kbd "C-S-<up>"))
  (define-key input-decode-map "\e[1;6B" (kbd "C-S-<down>"))
  (define-key input-decode-map "\e[1;6C" (kbd "C-S-<right>"))
  (define-key input-decode-map "\e[1;6D" (kbd "C-S-<left>"))
  (define-key input-decode-map "\e[1;3A" (kbd "M-<up>"))
  (define-key input-decode-map "\e[1;3B" (kbd "M-<down>"))
  (define-key input-decode-map "\e[1;3C" (kbd "M-<right>"))
  (define-key input-decode-map "\e[1;3D" (kbd "M-<left>"))
  (define-key input-decode-map "\e[1;4A" (kbd "M-S-<up>"))
  (define-key input-decode-map "\e[1;4B" (kbd "M-S-<down>"))
  (define-key input-decode-map "\e[1;4C" (kbd "M-S-<right>"))
  (define-key input-decode-map "\e[1;4D" (kbd "M-S-<left>"))
  (define-key input-decode-map "\e[5;3~" (kbd "M-<prior>"))
  (define-key input-decode-map "\e[6;3~" (kbd "M-<next>"))
  (define-key input-decode-map "\e[5;5~" (kbd "C-<prior>"))
  (define-key input-decode-map "\e[6;5~" (kbd "C-<next>")))

(when (string= (getenv "TERM") "rxvt")
  (define-key input-decode-map "\e[7@" (kbd "C-S-<home>"))
  (define-key input-decode-map "\e[8@" (kbd "C-S-<end>"))
  (define-key input-decode-map "\e[24~" (kbd "<f12>"))
  (define-key input-decode-map "\e[24$" (kbd "S-<f12>"))
  (define-key input-decode-map "\e[23~" (kbd "<f11>"))
  (define-key input-decode-map "\e[23$" (kbd "S-<f11>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC INIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tab-width 4)

;; case sensitivity is evil in user input
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; don't leap around the place, not with arrow keys
(setq scroll-step 1 scroll-conservatively 10000)
;; or page up / down
(setq scroll-preserve-screen-position t)

;; go into desktop mode if desktop save file exists in current directory
;(add-to-list 'desktop-path default-directory)
(if (file-exists-p ".emacs.desktop")
    (progn
      (setq desktop-path (list default-directory))
      (desktop-save-mode 1)))

;; reduce wordy prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; remove horrific git backend
(delete 'Git vc-handled-backends)

;; get an empty buffer on startup
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
;; (switch-to-buffer (get-buffer-create "empty"))
;; (delete-other-windows)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; no menu
(menu-bar-mode -1)

;; overwrite selection
(delete-selection-mode 1)

;; remember recent files; not good enough, also has issues with cleanup, network
;;(recentf-mode 1)
;;(setq recentf-max-saved-items 300)
;; (defun ido-choose-recentf ()
;;   "Choose recently opened file using ido"
;;   (interactive)
;;   (find-file (ido-completing-read "Recent file: " recentf-list nil t)))
;; (global-set-key (kbd "<f11>") 'ido-choose-recentf)

;; remember where we were last time
(defun recenter-plain ()
  (recenter))
(add-hook 'find-file-hooks 'recenter-plain t)
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.local/saved-places")

;; reload files that have been changed on disk
(global-auto-revert-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO MODE CONFIG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; prefer grizzl
;;(setq ido-enable-flex-matching t)
;;(setq ido-enable-last-directory-history t)
;;(ido-indicator ((t (:background "yellow" :foreground "black" :width condensed)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLUGINS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;----------------------------------------
;; package management
;;----------------------------------------

;; melpa package repository
(when (> emacs-major-version 23)
  (require 'package)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize))

;; plugins; everything in plugins, and subdirectories
(add-to-list 'load-path "~/.emacs.d/plugins/")
(let ((default-directory "~/.emacs.d/plugins/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; note: packages that come via package management need to be loaded after package management

;; line numbers; consider replacing with nlinum or something faster, linum is horribly slow.
;(require 'nlinum)

(require 'linum)
(global-linum-mode 1)
(setq linum-format "%4d ")
(setq linum-eager nil)

;; columns
(column-number-mode)

;; expand region
(require 'expand-region)
(global-set-key (kbd "M-h") 'er/expand-region)
(global-set-key (kbd "M-H") 'er/contract-region)

(require 'auto-mark)
(auto-mark-mode)

;; wrap selection in paired elements - consider doing something to preserve selection
(wrap-region-global-mode)
(setq wrap-region-keep-mark t)

;;----------------------------------------
;; minibuffer
;;----------------------------------------

;; minibuffer completion - icicles, very heavyweight
;;(require 'icicles)
;;(icy-mode 1)

;; incremental search buffer switch with C-x b - lightweight
;;(iswitchb-mode)

;; ido-mode; does iswitchb and more
(ido-mode)
;; prefer helm-buffers-list

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

;; completion for M-x: seriously, why isn't this the default?
;;(icomplete-mode) ;; prefer helm


;;----------------------------------------
;; projectile
;;----------------------------------------

(projectile-global-mode)
;;(setq projectile-completion-system 'grizzl)
(setq projectile-completion-system 'helm-comp-read)
;; alt: (add-hook 'some-mode-hook 'projectile-on)

;;----------------------------------------
;; helm
;;----------------------------------------

(require 'helm)
(define-key helm-map (kbd "M-[") nil)
;;(helm-mode) ;; this is too extreme

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-G") 'helm-git-grep)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COLORS ETC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; marginally shorter definition helper
(defun set-face (face fg &optional bg weight)
  "Set face details.
* face: the face to set up
* fg: foreground color
* bg: background color, defaults to black
* weight: value for :weight, defaults to bold"
  (set-face-foreground face fg)
  (if bg
      (set-face-background face bg)
    (set-face-background face "black"))
  (if weight
      (set-face-attribute face nil :weight weight)
    (set-face-attribute face nil :weight 'normal)))


;; set selection highlight to something readable
(set-face 'region "white" "magenta")

;; search, active and inactive
(set-face 'isearch "black" "cyan")
(set-face 'lazy-highlight "black" "green")
(set-face 'highlight "black" "yellow")
(set-face 'query-replace "black" "yellow")

;; parenthesis highlighting
(show-paren-mode t)
(set-face 'show-paren-match-face "yellow" "black" 'extra-bold)

;; various syntactic and semantic highlights
(set-face 'font-lock-builtin-face "blue")
(set-face 'font-lock-keyword-face "blue")
(set-face 'font-lock-comment-delimiter-face "green" "black" 'extra-bold)
(set-face 'font-lock-comment-face "green" "black" 'extra-bold)
(set-face 'font-lock-constant-face "white" "black" 'extra-bold)
(set-face 'font-lock-doc-face "green")
(set-face 'font-lock-string-face "yellow")
(set-face 'font-lock-type-face "red")
(set-face 'font-lock-preprocessor-face "magenta" "black" 'extra-bold)
(set-face 'font-lock-function-name-face "cyan")
(set-face 'font-lock-negation-char-face "white" "black" 'extra-bold)
(set-face 'font-lock-variable-name-face "white")

(set-face 'font-lock-regexp-grouping-backslash "white" "black" 'extra-bold)
(set-face 'font-lock-regexp-grouping-construct "red" "black" 'extra-bold)

(set-face 'match "black" "blue")

(set-face 'linum "magenta")

;; this assumes that magenta has been repurposed to a dark color like slate gray
(set-face 'mode-line "magenta" "white")
(set-face 'fringe "white" "black")

(set-face 'show-paren-mismatch "white" "red" 'extra-bold)

;; helm
(set-face 'helm-source-header "cyan" "magenta" 'extra-bold)
;;(set-face 'helm-list-show-completion "magenta" "blue")
(set-face 'helm-header "white" "magenta")
;;(set-face 'helm-helper "cyan" "magenta")
(set-face 'helm-selection "black" "yellow" 'extra-bold)
;;(set-face 'helm-selection-line "black" "cyan")

;; red green blue yellow cyan magenta white black

;; note: list faces with list-faces-display


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; don't make backspace turn tabs into spaces (who thought that was a good idea?)
(setq backward-delete-char-untabify-method nil)

;; default to using tabs at 4
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 200 4))
(setq-default tab-always-indent 'complete)
(setq-default c-basic-offset 4)
(setq-default c-default-style "bsd")

(defun set-tab-style (a-use-tabs a-tab-width &optional a-style)
  "Set up tab according to the desired style.
	a-use-tabs - t to use tabs, nil to use spaces
	a-tab-width - set tab width
	a-style - style for c-default-style"
  (setq indent-tabs-mode a-use-tabs)
  (setq tab-width a-tab-width)
  (setq tab-stop-list (number-sequence a-tab-width 200 a-tab-width))
  (setq c-basic-offset a-tab-width) ;; c / c++ etc
  (setq coffee-tab-width a-tab-width)  ;; coffeescript
  (setq js-indent-level a-tab-width) ;; js
  (setq default-tab-width a-tab-width) ;; java
  (setq css-indent-offset a-tab-width) ;; css, scss etc.
  (if a-style
      (setq c-default-style a-style)
    (setq c-default-style "bsd")))

;; map completion to C-<space>
(global-set-key (kbd "C-@") 'indent-for-tab-command)
(global-set-key (kbd "M-`") 'hippie-expand)
(global-set-key (kbd "M-?") 'hippie-expand) ;; M-S-/
(global-set-key (kbd "TAB") 'self-insert-command) ;; should be smarter
;; move mark begin to be like C-k b in Joe
(global-set-key (kbd "C-c b") 'set-mark-command)
(global-set-key (kbd "C-c SPC") 'set-mark-command)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LANGUAGE MODE CONFIG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C
(add-hook 'c-mode-hook
          (lambda ()
            (set-tab-style t 4)))

;; C++
(add-hook 'c++-mode-hook
          (lambda ()
            (set-tab-style t 4)))

;; C & C++
(add-hook 'c-mode-common-hook
          (lambda ()
            (visual-line-mode)
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))

;; CoffeeScript
(add-hook 'coffee-mode-hook
          (lambda ()
            (visual-line-mode)
            (set-tab-style nil 2)))

;; Conf
(add-to-list 'auto-mode-alist '("my.cnf" . conf-mode))

;; CSharp-Mode
(autoload 'csharp-mode "csharp-mode-0.8.5" nil t)
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))
(add-hook 'csharp-mode-hook
          (lambda ()
            (set-tab-style t 4)))

;; elisp mode
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (set-tab-style nil 2)
            (visual-line-mode)
            (global-set-key (kbd "M-RET") 'eval-region) ;; rxvt
            (global-set-key (kbd "C-^") 'eval-region))) ;; mintty

;; Go
(require 'go-mode-load)
(add-hook 'go-mode-hook
          (lambda ()
            (visual-line-mode)
            (set-tab-style t 4)))

;; Haml
(require 'haml-mode-autoloads)
(add-to-list 'auto-mode-alist '("\\.hamlc\\'" . haml-mode)) ;; CoffeeScript haml

;; Java
(add-hook 'java-mode-hook
          (lambda()
            (set-tab-style t 4)
            (visual-line-mode)))

;; Javascript
(add-hook 'js-mode-hook
          (lambda ()
            (set-tab-style nil 2)
            (visual-line-mode)))

;; Markdown
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Puppet
(require 'puppet-mode-autoloads)

;; Ruby
(add-hook 'ruby-mode-hook
          (lambda ()
            (visual-line-mode)))
(add-hook 'enh-ruby-mode-hook
          (lambda ()
            (visual-line-mode)
            (global-set-key (kbd "RET") 'newline-and-indent)))
(add-to-list 'auto-mode-alist '("Gemfile" . enh-ruby-mode))


;; SCSS
(add-hook 'scss-mode-hook
          (lambda ()
            (visual-line-mode)
            (set-tab-style nil 2)))

;; Shell script
(add-hook 'sh-mode-hook
          (lambda ()
            (visual-line-mode)
            (set-tab-style nil 4)))

;; YAML
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          (lambda ()
            (visual-line-mode)
            (set-tab-style nil 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END MODE CONFIG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; pipe selection through shell command if any, otherwise output
(defun generalized-shell-command (command arg)
  "Unifies `shell-command' and `shell-command-on-region'. If no region is
selected, run a shell command just like M-x shell-command (M-!).  If
no region is selected and an argument is a passed, run a shell command
and place its output after the mark as in C-u M-x `shell-command' (C-u
M-!). If a region is selected pass the text of that region to the
shell and replace the text in that region with the output of the shell
command as in C-u M-x `shell-command-on-region' (C-u M-|). If a region
is selected AND an argument is passed (via C-u) send output to another
buffer instead of replacing the text in region."
  (interactive (list
                (read-from-minibuffer "Shell command: " nil nil nil 'shell-command-history)
                current-prefix-arg))
  (let ((p (if mark-active (region-beginning) 0))
        (m (if mark-active (region-end) 0)))
    (if (= p m)
        ;; No active region
        (shell-command command t)
      ;; actually, jut run the file
      ;; (if (eq arg nil)
      ;;     (shell-command command)
      ;;   (shell-command command t))
      ;; Active region
      (if (eq arg nil)
          (shell-command-on-region p m command t t)
        (shell-command-on-region p m command)))))


;; C-y doesn't do it for me as a paste, too awkward to type on Dvorak; rebind it to non-kill delete line
(defun delete-line-command ()
  "Deletes current line and don't put in kill ring"
  (interactive)
  (beginning-of-line)
  (let ((start (point)))
    (end-of-line)
    (forward-char) ;; eat newline too, but don't fail on last line of file
    (delete-region start (point))))

;; clipboard reconfiguration
(global-set-key (kbd "C-y") 'delete-line-command)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "M-v") 'yank-pop)
(global-set-key (kbd "M-S-<insert>") 'kill-ring-save)
(global-set-key (kbd "M-<insert>") 'yank) ;; mintty
(global-set-key (kbd "M-<delete>") 'kill-word) ;; for parallel with M-d
(global-set-key (kbd "ESC <insertchar>") 'yank) ;; rxvt

;;(Global-set-key (kbd "S-<delete>") 'kill-region) ;; default

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC KEY REBINDINGS / SHORTCUTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: put these in minor modes

;; note existence of this for rxvt:
;; http://www.emacswiki.org/emacs/rxvt.el

;; my custom macros bound to keys past C-j, by convention
(global-unset-key (kbd "C-j"))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x /") 'generalized-shell-command)
(global-set-key (kbd "C-x C-_") 'generalized-shell-command) ;; C-x C-/
(global-set-key (kbd "C-c /") 'generalized-shell-command)
;; C-x z and C-x C-z still background emacs
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-c w") 'write-region)
(global-set-key (kbd "C-c r") 'insert-file)
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "M-[ 1 ; 6 n") 'next-buffer) ;; C-> in mintty
(global-set-key (kbd "C-<prior>") 'previous-buffer)
(global-set-key (kbd "C-j /") 'comment-or-uncomment-region)
(global-set-key (kbd "C-j C-_") 'comment-or-uncomment-region)
(global-set-key (kbd "C-j C-j") 'reposition-window)
(global-set-key (kbd "ESC <deletechar>") 'kill-word) ;; alt delete

;; navigation start
(global-set-key (kbd "ESC <S-left>") 'pop-global-mark) ;; alt shift left
(global-set-key (kbd "ESC <left>") 'pop-to-mark-command) ;; alt left
(global-set-key (kbd "M-S-<left>") 'pop-global-mark) ;; alt shift left
(global-set-key (kbd "M-<left>") 'pop-to-mark-command) ;; alt left

;; scrolling
(defun scroll-up-line-other-window ()
  "Scroll other window up one line"
  (interactive)
  (scroll-other-window 1))
(defun scroll-down-line-other-window ()
  "Scroll other window down one line"
  (interactive)
  (scroll-other-window-down 1))

(global-set-key (kbd "C-<up>") 'scroll-down-line)
(global-set-key (kbd "C-<down>") 'scroll-up-line)
(global-set-key (kbd "M-S-<up>") 'scroll-down-line-other-window)
(global-set-key (kbd "M-S-<down>") 'scroll-up-line-other-window)
(global-set-key (kbd "ESC S-<up>") 'scroll-down-line-other-window)
(global-set-key (kbd "ESC S-<down>") 'scroll-up-line-other-window)

(global-set-key (kbd "M-a") 'backward-sexp)
(global-set-key (kbd "M-e") 'forward-sexp)

;; search and replace
(global-set-key (kbd "M-r") 'replace-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; semantic navigation with completion
(global-set-key (kbd "M-i") 'imenu)

(defun duplicate-line-or-region ()
  "Duplicate region, or line if no region selected"
  (interactive)
  (if (region-active-p)
      (let (deactivate-mark) ;; keep region after duplication
        (save-excursion
          (copy-region-as-kill (region-beginning) (region-end))
          (yank))
        (exchange-point-and-mark)
        (exchange-point-and-mark))
    (save-excursion
      (beginning-of-line)
      (let ((start (point)))
        (forward-line)
        (copy-region-as-kill start (point))
        (yank)))))
(global-set-key (kbd "M-c") 'duplicate-line-or-region)


;; don't ask multiple times about exiting with unsaved buffers
(defun my-save-buffers-kill-emacs (&optional arg)
  "Offer to save each buffer(once only), then kill this Emacs process.
With prefix ARG, silently save all file-visiting buffers, then kill."
  (interactive "P")
  (save-some-buffers arg t)
  (and (or (not (fboundp 'process-list))
           ;; process-list is not defined on MSDOS.
           (let ((processes (process-list))
                 active)
             (while processes
               (and (memq (process-status (car processes)) '(run stop open listen))
                    (process-query-on-exit-flag (car processes))
                    (setq active t))
               (setq processes (cdr processes)))
             (or (not active)
                 (progn (list-processes t)
                        (yes-or-no-p "Active processes exist; kill them and exit anyway? ")))))
       ;; Query the user for other things, perhaps.
       (run-hook-with-args-until-failure 'kill-emacs-query-functions)
       (or (null confirm-kill-emacs)
           (funcall confirm-kill-emacs "Really exit Emacs? "))
       (kill-emacs)))
(fset 'save-buffers-kill-emacs 'my-save-buffers-kill-emacs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM MACRO FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro keep-region (command)
  "Wrap command in code that saves and restores the region"
  (letrec ((command-name (symbol-name command))
           (advice-name (concat command-name "-keep-region")))
    `(progn
       (defadvice ,command (around ,(intern advice-name))
         (let (deactivate-mark)
           (save-excursion
             ad-do-it)
           (exchange-point-and-mark)
           (exchange-point-and-mark)))
       (ad-activate (quote ,command)))))

(keep-region replace-string)
(keep-region replace-regexp)

;; make zap-to-char act like zap-up-to-char
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))
;; note: bound to M-z

(defun insert-braces-macro ()
  (interactive)
  (insert "{")
  (newline) (indent-according-to-mode)
  (save-excursion
    (newline)
    (insert "}")
    (indent-according-to-mode)))


;;(defun insert-braces-macro ()
;;  (interactive)
;;  (insert "{")
;;  (newline)
;;  (indent-relative t)
;;  (insert "}")
;;  (forward-line -1)
;;  (end-of-line)
;;  (newline)
;;  (indent-relative t)
;;  (indent-relative nil))

(global-set-key (kbd "C-j b r") 'insert-braces-macro)

(defvar dumb-indent-string "	"
  "The indent string to use in dumb-indenting mode")

(defun dumb-indent-return ()
  "Insert a new line, then the same whitespace that the previous line started with"
  (interactive)
  (let (start prior-indent)
    (save-excursion
      (beginning-of-line)
      (setq start (point))
      (forward-to-indentation 0)
      (setq prior-indent (buffer-substring start (point))))
    (newline)
    (insert prior-indent)))

(global-set-key (kbd "C-j RET") 'dumb-indent-return)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load using completions from .listing file found in a parent directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq startup-directory default-directory)

;; use locate-dominating-file instead!

(defun find-file-in-parents (file directory)
  "Look for a file in directory and parent directories"

  (defun get-path-elems (path)
    "Get vector of path elements"
    (vconcat (split-string (expand-file-name path) "/" t)))

  (defun for-each-path-to-root (path fn)
    "Call fn for each directory in path (inclusive) to root while fn returns nil"
    (let (elems i p ret)
      (setq elems (get-path-elems path))
      (setq i (length elems))
      (catch 'ret
        (while (> i 0)
          (setq p (concat "/" (reduce (lambda (x y) (concat x "/" y))
                                      (subseq elems 0 i))))
          (setq ret (funcall fn (concat p "/")))
          (when ret (throw 'ret ret))
          (decf i))
        (funcall fn "/"))))
  
  (for-each-path-to-root
   directory
   (lambda (path)
     (let ((ret (concat path file)))
       (when (file-attributes ret)
         ret)))))


(defun try-read-file-lines (file)
  (if (file-attributes file)
      (with-temp-buffer
        (insert-file-contents file)
        (split-string (buffer-string) "\n" t))
    nil))

;; almost, but not quite: doesn't take into account differences in path
(defun ido-load-listing ()
  "Use ido-completing-read to find a file name from .listing file"
  (interactive)
  (let (listing-file lines found-file)
    (when (setq listing-file (find-file-in-parents ".listing" startup-directory))
      (when (setq lines (try-read-file-lines listing-file))
        (when (setq found-file (ido-completing-read "Load from listing: " lines))
          (find-file
           (concat
            (file-name-directory listing-file)
            found-file)))))))

(defun helm-load-listing ()
  "Use helm-comp-read to find a file name from .listing file"
  (interactive)
  (let (listing-file lines found-file)
    (when (setq listing-file (find-file-in-parents ".listing" startup-directory))
      (when (setq lines (try-read-file-lines listing-file))
        (when (setq found-file (helm-comp-read "Load from listing: " lines))
          (find-file
           (concat
            (file-name-directory listing-file)
            found-file)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HIGHLIGHTING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-point-text ()
  "Get 'interesting' text at point; either word, or region"
  (if mark-active
      (buffer-substring (mark) (point))
    (word-at-point)))

(defvar current-highlight-word nil
  "Current word for toggle-word-highlight if any")
(make-variable-buffer-local 'current-highlight-word)
(defun toggle-word-highlight ()
  "Toggle highlight of word-at-point"
  (interactive)
  (let ((new-word (get-point-text)))
    (unhighlight-regexp current-highlight-word)
    (if (equal new-word current-highlight-word)
        (setq-local current-highlight-word nil)
      (highlight-regexp new-word)
      (setq-local current-highlight-word new-word))))
(global-set-key (kbd "M-m") 'toggle-word-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROJECT / FILE / BUFFER NAVIGATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-j C-j") 'helm-load-listing)
(global-set-key (kbd "C-c j") 'helm-load-listing)

(global-set-key (kbd "<f11>") 'helm-load-listing)
(global-set-key (kbd "S-<f11>") 'helm-buffers-list)

;; TODO: need a version of projectile-find-file with initial text
(global-set-key (kbd "<f12>") 'projectile-find-file)
(global-set-key (kbd "S-<f12>") 'projectile-switch-to-buffer)
(global-set-key (kbd "ESC <f12>") 'helm-resume)

;; narrowing / widening act on selected region
;; C-x n n to narrow
;; C-x n w to widen
(put 'narrow-to-region 'disabled nil)
;; C-x n p
(put 'narrow-to-page 'disabled nil)


