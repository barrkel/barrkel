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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLUGINS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;----------------------------------------
;; package management
;;----------------------------------------

;; melpa package repository
(when (> emacs-major-version 23)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/")
               'APPEND))

;; plugins; everything in plugins, and subdirectories
(add-to-list 'load-path "~/.emacs.d/plugins/")
(let ((default-directory "~/.emacs.d/plugins/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; note: packages that come via package management need to be loaded after package management

;; line numbers
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%4d ")

;; columns
(column-number-mode)

;; expand region
(require 'expand-region)
;; C-* (C-S-6) in rxvt
(global-set-key (kbd "M-[ 1 ; 6 x") 'er/expand-region)
(global-set-key (kbd "M-[ 1 ; 6 w") 'er/contract-region)

(require 'auto-mark)
(auto-mark-mode)

;;----------------------------------------
;; minibuffer
;;----------------------------------------

;; minibuffer completion - icicles, very heavyweight
;;(require 'icicles)
;;(icy-mode 1)

;; incremental search buffer switch with C-x b - lightweight
(iswitchb-mode 1)

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

;; completion for M-x: seriously, why isn't this the default?
(icomplete-mode)


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
(set-face 'isearch "black" "yellow")
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

(set-face 'linum "magenta")

;; this assumes that magenta has been repurposed to a dark color like slate gray
(set-face 'mode-line "magenta" "white")
(set-face 'fringe "white" "black")

(set-face 'show-paren-mismatch "white" "red" 'extra-bold)

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
  (setq js-indent-level a-tab-width) ;; js
  (setq default-tab-width a-tab-width) ;; java
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN MODE CONFIG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; csharp-mode
(autoload 'csharp-mode "csharp-mode-0.8.5" nil t)
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

;; markdown
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
;;(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

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

;; shell script
(add-hook 'sh-mode-hook
          (lambda ()
            (visual-line-mode)
            (set-tab-style nil 4)))

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

;; Ruby
(add-hook 'ruby-mode-hook
          (lambda ()
            (visual-line-mode)))


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
(global-set-key (kbd "C-c c") 'kill-ring-save)
(global-set-key (kbd "C-c v") 'yank)
(global-set-key (kbd "C-c x") 'kill-region)
(global-set-key (kbd "M-S-<insert>") 'kill-ring-save)
(global-set-key (kbd "M-<insert>") 'yank) ;; mintty
(global-set-key (kbd "ESC <insertchar>") 'yank) ;; rxvt

;;(Global-set-key (kbd "S-<delete>") 'kill-region) ;; default

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC KEY REBINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; my custom macros bound to keys past C-j, by convention
(global-unset-key (kbd "C-j"))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x /") 'generalized-shell-command)
(global-set-key (kbd "C-c /") 'generalized-shell-command)
;; C-x z and C-x C-z still background emacs
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-c w") 'write-region)
(global-set-key (kbd "C-c r") 'insert-file)
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "M-[ 1 ; 6 n") 'next-buffer) ;; C-> in mintty
(global-set-key (kbd "C-<prior>") 'previous-buffer)
;; this is C-/; I'm using C-z for undo
(global-set-key (kbd "C-_") 'comment-or-uncomment-region)
(global-set-key (kbd "C-j C-j") 'reposition-window)
(global-set-key (kbd "ESC <deletechar>") 'kill-word) ;; alt delete

;; navigation start
(global-set-key (kbd "ESC <S-left>") 'pop-global-mark) ;; alt shift left
(global-set-key (kbd "ESC <left>") 'pop-to-mark-command) ;; alt left

(defun duplicate-line ()
  "Duplicate current line"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (forward-line)
      (copy-region-as-kill start (point))
      (yank))))


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

;; I don't think it's possible to implement the "keep region" wrap logic as a function
;; that calls defadvice, because defadvice is a macro, and the arguments will be evaluated
;; too soon. TODO for another day: implement the call to defadvice as a macro that can be
;; applied to any command, to ensure it keeps its region.
(defadvice replace-string (around replace-string-keep-region)
  "Replace string in region and restore region after replacement has been made."
  (let (deactivate-mark)
    (save-excursion
      ad-do-it))
  (exchange-point-and-mark)
  (exchange-point-and-mark))
(ad-activate 'replace-string)
(global-set-key (kbd "C-j r") 'replace-string)

;; (defadvice  (around replace-string-keep-region)
;;   "Replace string in region and restore region after replacement has been made."
;;   (let (deactivate-mark)
;;     (save-excursion
;;       ad-do-it))
;;   (exchange-point-and-mark)
;;   (exchange-point-and-mark))

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

;; narrowing / widening act on selected region
;; C-x n n to narrow
;; C-x n w to widen
(put 'narrow-to-region 'disabled nil)
;; C-x n p
(put 'narrow-to-page 'disabled nil)

