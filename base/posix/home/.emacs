(setq gc-cons-threshold 20000000)

;; custom stuff that I want on the load path
(add-to-list 'load-path "~/.emacs.d/barrkel")

;; packages
(require 'package)
(add-to-list 'package-archives
         '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(setq package-enable-at-startup nil)

;; occasionally required, e.g. when upgrading emacs
(defun recompile-all-the-things ()
  "Recompile all out of date .el files in ~/.emacs.d"
  (interactive)
  (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUTOLOADS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'smartrep-define-key "smartrep")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPERS USED IN MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-current-line-indent ()
  (let (result start)
    (save-excursion
      (beginning-of-line)
      (setq start (point))
      (forward-to-indentation 0)
      (setq result (buffer-substring start (point))))
    result))

(defun dumb-newline ()
  "Create a new line with the same indent as the previous line;
 this is classic auto-indent from ages past. Bind to RET for
 language modes that have asinine indent schemes bound to
 indent-line-function."
  (interactive)
  (let ((indent (get-current-line-indent)))
    (newline)
    (insert indent)))

;; many modes use some other random mode's indent-related variables; to save the effort of
;; figuring out each mode's magic variable, brute-force them herein.
(defun set-tab-style (a-use-tabs a-tab-width &optional a-style)
  "Set up tab according to the desired style.
        a-use-tabs - t to use tabs, nil to use spaces
        a-tab-width - set tab width
        a-style - style for c-default-style if the mode uses it; typically bsd / linux"
  (setq indent-tabs-mode a-use-tabs)
  (setq tab-width a-tab-width)
  (setq tab-stop-list nil) ; use tab-width

  (setq c-basic-offset a-tab-width) ; c / c++, csharp
  (setq default-tab-width a-tab-width) ; java, ...
  (setq coffee-tab-width a-tab-width)
  (setq js-indent-level a-tab-width)
  (setq julia-basic-offset a-tab-width)
  (setq css-indent-offset a-tab-width) ; css, scss etc.

  ;; Get rid of infuriating argument alignment
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-cont-nonempty '+)

  (if a-style
      (setq c-default-style a-style)
    (setq c-default-style "bsd")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAJOR MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Things to consider configuring per mode:
;; - tab settings via set-tab-style
;; - subword-mode - camelCase languages
;; - highlight-indentation-current-column-mode - indent-based languages
;; - whitespace-mode - show weird whitespace mixing
;; - flycheck-mode - if we have a linter
;; - toggle electric state - keys that randomly start doing weird indenting
;; - dumb-newline - bind to RET for smartass modes
;; - visual-line-mode - you usually want this
;; - nlinum-mode - disable for non-programming modes
;; - indent-line-function - define to e.g. insert-tab if it's missing or dumb
;; - unset weird random overrides of basic keys; M-h is a common one
;; - auto-fill-mode and fill-column variables

;; Things not to configure per mode:
;; - faces vias theme file, not here

;; c & c++
(add-hook 'c-mode-common-hook
          (lambda ()
            (subword-mode)
            (visual-line-mode)
            ;;(whitespace-mode)
            (set-tab-style t 4 "linux")
            ;; customize more by using these two steps:
            ;; C-c C-s to discover syntactic context symbol
            ;; C-c C-o to alter offset, choose syntactic context symbol from list
            (c-set-offset 'func-decl-cont 0)
            (c-toggle-electric-state -1)
            (local-set-key (kbd "RET") 'dumb-newline)
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))

;; coffeescript
(add-hook 'coffee-mode-hook
          (lambda ()
            (yafolding-mode)
            (subword-mode)
            (whitespace-mode)
            (visual-line-mode)
            (highlight-indentation-current-column-mode)
            (set-tab-style nil 2)
            (flycheck-mode)
            (setq flycheck-checker 'coffee-coffeelint)
            (setq fill-column 120)
            (define-key coffee-mode-map (kbd "M-,") 'coffee-indent-shift-left)
            (define-key coffee-mode-map (kbd "M-.") 'coffee-indent-shift-right)))

;; conf
(add-to-list 'auto-mode-alist '("my.cnf" . conf-mode))
(add-hook 'conf-mode-hook
          (lambda ()
            (set-tab-style t 4)
            (setq indent-line-function 'insert-tab)
            (local-set-key (kbd "RET") 'dumb-newline)))

;; csharp-mode
(add-hook 'csharp-mode-hook
          (lambda ()
            (subword-mode)
            (set-tab-style t 4)))

;; csv-mode
(add-hook 'csv-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'dumb-newline)))

;; dired
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "W") 'wdired-change-to-wdired-mode)))
(define-key global-map (kbd "C-x C-j") 'dired-jump)

;; emacs-lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map (kbd "C-.") 'find-function-at-point)
            (define-key emacs-lisp-mode-map (kbd "C-j") 'eval-print-last-sexp)
            (eldoc-mode)
            (setq fill-column 100)
            (auto-fill-mode)
            (setq indent-tabs-mode nil)))

;; go
(add-hook 'go-mode-hook
          (lambda ()
            (visual-line-mode)
            (set-tab-style t 4)))

;; groovy
(add-hook 'groovy-mode-hook
          (lambda ()
            (set-tab-style t 4)
            (subword-mode)
            (visual-line-mode)))

;; haml
(add-to-list 'auto-mode-alist '("\\.hamlc\\'" . haml-mode)) ;; coffeescript haml
(add-hook 'haml-mode-hook
          (lambda ()
            (whitespace-mode)
            (highlight-indentation-current-column-mode)))

;; help
(add-hook 'help-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)))

;; ibuffer
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)))

;; ispell
;; (add-hook 'ispell-initialize-spellchecker-hook
;;           (lambda ()
;;             ;; hack to get spellchecking on CamelCase words
;;             (defun ispell-get-word (following)
;;               (when following
;;                 (camelCase-forward-word 1))
;;               (let* ((start (progn (camelCase-backward-word 1)
;;                                    (point)))
;;                      (end (progn (camelCase-forward-word 1)
;;                                  (point))))
;;                 (list (buffer-substring-no-properties start end)
;;                       start end)))))

;; java
(add-hook 'java-mode-hook
          (lambda()
            (yafolding-mode)
            (set-tab-style t 4)
            (subword-mode)
            ;;(whitespace-mode)
            (visual-line-mode)))

;; javascript
(add-hook 'js-mode-hook
          (lambda ()
            (set-tab-style nil 2)
            (whitespace-mode)
            (subword-mode)
            (visual-line-mode)))

;; julia
(add-hook 'julia-mode-hook
          (lambda ()
            (set-tab-style nil 2)))

;; magit
(add-hook 'git-commit-mode-hook
          (lambda ()
            (auto-fill-mode 0)
            (define-key git-commit-mode-map (kbd "M-n") nil)
            (setq git-commit-summary-max-length 999)))
(add-hook 'git-commit-setup-hook
          (lambda ()
            (auto-fill-mode 0)
            ;; (define-key git-commit-mode-map (kbd "M-n") nil)
            (setq git-commit-summary-max-length 999)))

;; man
(add-hook 'Man-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)))

;; markdown
(add-hook 'markdown-mode-hook
          (lambda ()
            (whitespace-mode)
            (define-key markdown-mode-map (kbd "M-n") nil)
            (define-key markdown-mode-map (kbd "M-p") nil)
            (local-set-key (kbd "RET") 'dumb-newline)))

;; org
(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "sql")))
(add-hook 'org-mode-hook
          (lambda ()
            (require 'ox-reveal)
            (require 'ob-sql)
            (require 'ob-sqlite)
            (setq org-reveal-root "file:///home/barrykelly/.barrkel/opt/reveal.js-3.2.0/")
            (setq org-reveal-location "file:///home/barrykelly/.barrkel/opt/reveal.js-3.2.0/")
            (local-set-key (kbd "C-y") 'delete-line-command)
            (local-set-key (kbd "C-v") 'org-yank)
            (local-set-key (kbd "M-h") nil)
            (local-set-key (kbd "C-j") nil)
            (org-babel-do-load-languages
             'org-babel-do-load-languages '((sqlite t)))
            (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
            (setq org-support-shift-select t)))

;; puppet
(add-hook 'puppet-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'dumb-newline)))

;; rspec
(eval-after-load "rspec-mode"
  '(progn
     (setenv "PAGER" (executable-find "cat"))
     (setq rspec-use-rake-when-possible nil)
     (inf-ruby-switch-setup)
     (yafolding-mode)
     (define-key global-map (kbd "M-T") 'rspec-toggle-spec-and-target)))

;; ruby & enh-ruby-mode
(eval-after-load "enh-ruby-mode"
  '(progn
     (require 'ruby-mode-expansions)
     (require 'expand-region)
     (er/enable-mode-expansions 'enh-ruby-mode 'er/add-enh-ruby-mode-expansions)))
(add-hook 'ruby-mode-hook
          (lambda ()
            (yafolding-mode)
            (set-tab-style nil 2)
            (visual-line-mode)))
(add-hook 'enh-ruby-mode-hook
          (lambda ()
            (yafolding-mode)
            (visual-line-mode)
            (whitespace-mode)
            (set-tab-style nil 2)
            (define-key enh-ruby-mode-map (kbd "RET") 'newline-and-indent)
            (define-key enh-ruby-mode-map (kbd "C-M-n") nil)
            (define-key enh-ruby-mode-map (kbd "C-j") nil)
            (define-key enh-ruby-mode-map (kbd "C-c /") nil)

            ;; for long lines, prefer to wrap with one level of indentation, rather than open paren
            (setq enh-ruby-deep-indent-paren nil)

            (defun ruby-replace-symbol-map-syntax ()
              (interactive)
              (replace-regexp ":\\([^:']+\\) =>" "\\1:" nil
                              (if (and transient-mark-mode mark-active) (region-beginning))
                              (if (and transient-mark-mode mark-active) (region-end))))))
(add-to-list 'auto-mode-alist '("Gemfile" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.json.jbuilder\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; scss
(add-hook 'scss-mode-hook
          (lambda ()
            (visual-line-mode)
            (whitespace-mode)
            (setq scss-compile-at-save nil)
            (set-tab-style nil 2)))

;; shell script
(add-hook 'sh-mode-hook
          (lambda ()
            (visual-line-mode)
            (set-tab-style nil 4)))

;; slim templates
(add-hook 'slim-mode-hook
          (lambda ()
            (define-key slim-mode-map (kbd "RET") 'dumb-newline)
            (set-tab-style nil 2)))

;; sql
(add-hook 'sql-mode-hook
          (lambda ()
            (define-key sql-mode-map (kbd "RET") 'dumb-newline)))

;; text
(add-hook 'text-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'dumb-newline)
            (setq indent-tabs-mode nil)
            (setq tab-stop-list (number-sequence 2 200 2))
            (setq indent-line-function 'insert-tab)))

;; web-mode
(add-hook 'web-mode-hook
          (lambda ()
            (define-key web-mode-map (kbd "RET") 'newline-and-indent)))

;; xml
(add-hook 'nxml-mode-hook
          (lambda ()
            (define-key nxml-mode-map (kbd "M-h") nil)
            (set-tab-style t 4)))

;; yaml
(add-hook 'yaml-mode-hook
          (lambda ()
            (yafolding-mode)
            (visual-line-mode)
            (whitespace-mode)
            (highlight-indentation-current-column-mode)
            (set-tab-style nil 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MINOR MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; avy
(define-key global-map (kbd "M-C") 'avy-goto-word-or-subword-1)
(define-key global-map (kbd "M-L") 'avy-goto-char)
(define-key isearch-mode-map (kbd "<tab>") 'avy-isearch)
(define-key isearch-mode-map (kbd " ") 'avy-isearch)
(define-key isearch-mode-map (kbd "C-i") 'avy-isearch)
(eval-after-load "avy"
  '(progn
     (setq avy-keys (append (number-sequence ?a ?z) (number-sequence ?A ?Z)))
     ;; (set-face 'avy-lead-face "black" "yellow" 'bold)
     ;; (set-face 'avy-lead-face-0 "black" "yellow")
     ;; (set-face 'avy-lead-face-1 "magenta" "yellow")
     ;; (set-face 'avy-lead-face-2 "red" "yellow")))
     ))

;; avy-zap
(define-key global-map (kbd "M-Z") 'avy-zap-to-char-dwim)

;; expand-region
(define-key global-map (kbd "M-h") 'er/expand-region)
(define-key global-map (kbd "M-H") 'er/contract-region)
(eval-after-load "expand-region"
  '(progn
     (setq expand-region-fast-keys-enabled nil)))

;; grep
(eval-after-load "grep"
  '(progn
     (define-key grep-mode-map (kbd "W") 'wgrep-change-to-wgrep-mode)))

;; helm
;; Invoking these things will load helm, which will activate helm mode; this means a pause of a few
;; seconds the first time they're pressed. However for something simple like a git commit message,
;; it'll be quick.
(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
(define-key global-map (kbd "C-x b") 'helm-mini)
(define-key global-map (kbd "S-<f11>") 'helm-mini)
(define-key global-map (kbd "M-<f12>") 'helm-resume)
(define-key global-map (kbd "M-S-<f12>") 'helm-semantic-or-imenu)
(define-key global-map (kbd "M-p") 'helm-pages)

(eval-after-load "helm"
  '(progn
     (helm-mode)

     ;; rebind tab to run persistent action
     (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
     ;; make TAB works in terminal
     (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
     ;; list actions using C-z
     (define-key helm-map (kbd "C-z")  'helm-select-action)

     ;; Here's something a bit silly in helm: it has two distinct variables for idle waiting before
     ;; updating the list, one for slow ("delayed") sources and one for non-delayed. Since slow
     ;; sources shouldn't be kicked off too often, you want to increase the delay, right? But the
     ;; docs for helm-input-idle-delay - the delay for non-delayed sources - says it needs to be
     ;; greater than the idle delay for delayed sources!
     ;;
     ;; That's right: exactly the kind of operation you don't want to delay (e.g. M-x function list)
     ;; needs to respond slower than expensive background greps etc!

     ;; Duplicates do turn up in practice. So we need to suffer laggy input on non-delayed inputs.
     (setq helm-idle-delay 0.25)
     ;; even for non-delayed sources, helm is laggy when typing quickly
     (setq helm-input-idle-delay 0.25)

     (when (executable-find "curl")
       (setq helm-google-suggest-use-curl-p t))

     (setq
      ;; open helm buffer inside current window, not occupy whole other window
      helm-split-window-in-side-p t
      ;; move to end or beginning of source when reaching top or bottom of source.
      ;; helm-move-to-line-cycle-in-source t
      ;; search for library in `require' and `declare-function' sexp.
      helm-ff-search-library-in-sexp t
      ;; scroll 8 lines other window using M-<next>/M-<prior>
      helm-scroll-amount 8
      helm-ff-file-name-history-use-recentf t)))

;; highlight-indentation-current-column-mode
(define-key global-map (kbd "C-c TAB") 'highlight-indentation-current-column-mode)

;; iedit; iedit-mode defines a bunch of key bindings while active; C-h b to see them
(define-key global-map (kbd "M-i") 'iedit-mode)

;; multiple-cursors
(define-key global-map (kbd "M-n") 'mc/mark-next-like-this)
(define-key global-map (kbd "M-N") 'mc/unmark-next-like-this)
(define-key global-map (kbd "C-M-n") 'mc/skip-to-next-like-this)
;; disable inserting a | for all the fake cursors, throwing off alignments
;; the first one in an emacs session will appear, but no more
(add-hook 'multiple-cursors-mode-hook
          (lambda ()
            (defun mc/cursor-is-bar nil)))

;; projectile

;; projectile has appallingly bad performance on remote filesystems because it uses a dynamic
;; modeline; projectile-project-name searches the file system tree on every modeline update, but
;; guess what - the project a file belongs to almost never changes, certainly not as often as the
;; modeline is updated, which is synchronously after every keystroke at a minimum.

;; I'm fixing this here in the two ways: only enable projectile if it looks like the file is part of
;; a project, and memoize the projectile-project-name function. If there are operations in the
;; future that are likely to invalidate this cache, the variable can be set to nil and it will be
;; recalculated.

;; currently "projectness" is strictly determined by presence of .git directory
(add-hook 'find-file-hook
          (lambda ()
            (if (locate-dominating-file default-directory ".git")
                (progn
                  (projectile-mode)
                  (define-key global-map (kbd "<f11>") 'helm-projectile-find-file-dwim)
                  ;; piggy-back magit on this hook
                  (define-key global-map (kbd "M-M") 'magit-status)))))
(eval-after-load "projectile"
  '(progn
     (defvar-local bk/projectile-project-name-cache nil
       "Cached value of projectile-project-name")

     (defadvice projectile-project-name (around bk/projectile-project-name activate)
       (if (not bk/projectile-project-name-cache)
           (setq bk/projectile-project-name-cache ad-do-it))
       (setq ad-return-value bk/projectile-project-name-cache))

     (setq projectile-completion-system 'helm-comp-read)

     (define-key projectile-mode-map (kbd "<f12>") 'projectile-find-file)
     (define-key projectile-mode-map (kbd "S-<f12>") 'projectile-switch-to-buffer)
     (define-key projectile-mode-map (kbd "<f22>") 'projectile-switch-to-buffer)))

;; whitespace
;; global whitespace mode can't be easily undone; in particular display faces remain even when
;; whitespace is disabled for a buffer; thus we enable whitespace per mode, as desired
(define-key global-map (kbd "C-c SPC") 'whitespace-mode)
(setq whitespace-style '(face tabs trailing lines space-before-tab newline
                              space-after-tab tab-mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUFFER SWITCHING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; skip special buffers when cycling through with C-PgUp/PgDn

(setq special-buffer-blacklist '("\*Compile-Log\*" "\*helm.*" "\*Backtrace\*"
                                 "\*Messages\*" "\*magit.*"))
(autoload '-any-p "dash")
(defun next-code-buffer ()
  (interactive)
  (let (( bread-crumb (buffer-name) ))
    (next-buffer)
    (while
        (and
         (-any-p (lambda (re) (string-match-p re (buffer-name))) special-buffer-blacklist)
         (not (equal bread-crumb (buffer-name))))
      (next-buffer))))
(defun previous-code-buffer ()
  (interactive)
  (let ((bread-crumb (buffer-name)))
    (previous-buffer)
    (while
        (and
         (-any-p (lambda (re) (string-match-p re (buffer-name))) special-buffer-blacklist)
         (not (equal bread-crumb (buffer-name))))
      (previous-buffer))))
(define-key global-map (kbd "C-<next>") 'next-code-buffer)
(define-key global-map (kbd "C-<prior>") 'previous-code-buffer)

;; better buffer list with bulk ops
(define-key global-map (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SMARTREP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; move-text with C-c m -> p n
(smartrep-define-key global-map "C-c m"
  '(("n" . move-text-down)
    ("p" . move-text-up)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-block-pair (left right)
  (let ((indent (get-current-line-indent)))
    (insert left)
    (newline)
    (insert indent)
    (save-excursion
      (newline)
      (insert indent)
      (insert right)))
  (indent-for-tab-command))

(defun insert-braces-macro ()
  (interactive)
  (insert-block-pair "{" "}"))
(defun insert-parens-macro ()
  (interactive)
  (insert-block-pair "(" ")"))
(defun insert-brackets-macro ()
  (interactive)
  (insert-block-pair "[" "]"))
(defun insert-do-macro ()
  (interactive)
  (insert-block-pair "do" "end"))

(define-key global-map (kbd "C-j") nil)
(define-key global-map (kbd "C-j b r") 'insert-braces-macro)
(define-key global-map (kbd "C-j b {") 'insert-braces-macro)
(define-key global-map (kbd "C-j b (") 'insert-parens-macro)
(define-key global-map (kbd "C-j b [") 'insert-brackets-macro)
(define-key global-map (kbd "C-j b d") 'insert-do-macro)
(define-key global-map (kbd "C-j C-j") 'newline-and-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RANDOM ELISP BINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        (yank)))
    (next-line)))
(define-key global-map (kbd "M-c") 'duplicate-line-or-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-point-text ()
  "Get 'interesting' text at point; either word, or region"
  (if mark-active
      (buffer-substring (mark) (point))
    (thing-at-point 'symbol)))

(defun get-point-regex ()
  "Get 'interesting' text at point as a regex, with word boundaries if symbol"
  (if mark-active
      (regexp-quote (buffer-substring (mark) (point)))
    ;; note the \< and \> - this means we can't simply quote get-point-text
    (concat "\\<" (regexp-quote (thing-at-point 'symbol)) "\\>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local current-highlight-word nil
  "Current word for toggle-word-highlight if any")
(defun toggle-word-highlight ()
  "Toggle highlight of word-at-point"
  (interactive)
  (let ((new-word (get-point-regex)))
    (unhighlight-regexp current-highlight-word)
    (if (equal new-word current-highlight-word)
        (setq-local current-highlight-word nil)
      (highlight-regexp new-word)
      (setq-local current-highlight-word new-word))))
(define-key global-map (kbd "M-m") 'toggle-word-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'helm-occur-init-source "helm")
(defun helm-occur-1 (initial-value)
  "Preconfigured helm for Occur with initial input (helm-occur with mods)."
  (interactive)
  (helm-occur-init-source)
  (let ((bufs (list (buffer-name (current-buffer)))))
    (helm-attrset 'moccur-buffers bufs helm-source-occur)
    (helm-set-local-variable 'helm-multi-occur-buffer-list bufs)
    (helm-set-local-variable
     'helm-multi-occur-buffer-tick
     (cl-loop for b in bufs
              collect (buffer-chars-modified-tick (get-buffer b)))))
  (helm :sources 'helm-source-occur
        :buffer "*helm occur*"
        :history 'helm-grep-history
        :preselect (and (memq 'helm-source-occur helm-sources-using-default-as-input)
                        (format "%s:%d:" (buffer-name) (line-number-at-pos (point))))
        :truncate-lines t
        :input initial-value))
(defun bk/helm-occur ()
  "Invoke helm-occur with initial input configured from text at point"
  (interactive)
  (helm-occur-1 (get-point-text)))
(define-key global-map (kbd "M-o") 'bk/helm-occur)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'helm-git-grep-1 "helm-git-grep")
(defun git-grep-selected-text ()
  "Git grep for selected text, if any"
  (interactive)
  (if mark-active
      (helm-git-grep-1 (buffer-substring (mark) (point)))
    (helm-git-grep)))
(define-key global-map (kbd "M-G") 'git-grep-selected-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; simple indent / unindent block with M-, and M-.
;; code stolen from coffee-mode

(defun barrkel-indent-shift-amount (start end dir)
  "Compute distance to the closest increment of `tab-width'."
  (let ((min most-positive-fixnum))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((current (current-indentation)))
          (when (< current min)
            (setq min current)))
        (forward-line))
      (let ((rem (% min tab-width)))
        (if (zerop rem)
            tab-width
          (cond ((eq dir 'left) rem)
                ((eq dir 'right) (- tab-width rem))
                (t 0)))))))

(defun barrkel-indent-shift-left (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the left.
If COUNT is not given, indents to the closest increment of
`tab-width'. If region isn't active, the current line is
shifted. The shifted region includes the lines in which START and
END lie. An error is signaled if any lines in the region are
indented less than COUNT columns."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let ((amount (if count (prefix-numeric-value count)
                  (barrkel-indent-shift-amount start end 'left))))
    (when (> amount 0)
      (let (deactivate-mark)
        (save-excursion
          (goto-char start)
          ;; Check that all lines can be shifted enough
          (while (< (point) end)
            (if (and (< (current-indentation) amount)
                     (not (looking-at "[ \t]*$")))
                (error "Can't shift all lines enough"))
            (forward-line))
          (indent-rigidly start end (- amount)))))))

(defun barrkel-indent-shift-right (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the right.
if COUNT is not given, indents to the closest increment of
`tab-width'. If region isn't active, the current line is
shifted. The shifted region includes the lines in which START and
END lie."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let (deactivate-mark
        (amount (if count (prefix-numeric-value count)
                  (barrkel-indent-shift-amount start end 'right))))
    (indent-rigidly start end amount)))

(add-to-list 'debug-ignored-errors "^Can't shift all lines enough")

(define-key global-map (kbd "M-,") 'barrkel-indent-shift-left)
(define-key global-map (kbd "M-.") 'barrkel-indent-shift-right)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; better shell piping: pipe selection through shell command if any, otherwise output

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
      ;; Active region
      (if (eq arg nil)
          (shell-command-on-region p m command t t)
        (shell-command-on-region p m command)))))
(define-key global-map (kbd "M-|") 'generalized-shell-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; clipboard reconfiguration with a delete line that doesn't go into kill ring

;; map C-k to delete whole line without killing
(defun delete-line-command ()
  "Deletes current line and don't put in kill ring"
  (interactive)
  (beginning-of-line)
  (let ((start (point)))
    (end-of-line)
    (forward-char) ;; eat newline too, but don't fail on last line of file
    (delete-region start (point))))
(define-key global-map (kbd "C-y") 'delete-line-command)
;; I don't use these keys for paging; remap to paste
(define-key global-map (kbd "C-v") 'yank)
(define-key global-map (kbd "M-<insert>") 'yank)
(define-key global-map (kbd "M-v") 'helm-show-kill-ring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TABS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use tab for completion
(setq-default tab-always-indent 'complete)

(setq-default tab-width 4)

;; indent-relative has never done what I want
(defun indent-relative ()
  (indent-tab))

;; I think there was some emacs guy 20 years ago who thought tabs were a text compression feature.
;; Emacs is desperate to insert tabs, yet pretends that it didn't. If you want to use tab
;; indentation in a language, you basically have to set the indentation step in spaces, then
;; configure tab width so that it correctly divides into the indentation step, and finally configure
;; tab compression to convert the spaces into tabs.
;;
;; So if you like a display tab width of 8, then you should set both tab-width and basic offset /
;; indent variables to 8, and enable indent-tabs-mode. But if you prefer a display tab width of 4 -
;; even if just temporarily - you need to change two variables to achieve a single effect.

;; stop introducing tabs (change if necessary in mode hook)
(setq-default indent-tabs-mode nil)

;; and stop pretending tabs are made up of spaces
(setq backward-delete-char-untabify-method nil)

;; let's leave the 1980's US typing school program
(setq sentence-end-double-space nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TWEAKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; X paste bug
(setq x-selection-timeout 10)

;; line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-width 4)
;; (set-face 'linum "magenta")

;; number columns too
(column-number-mode)

;; paren match highlighting
(show-paren-mode)

;; save place when reloading; we must load this package now to make it work for command line args
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs-saved-places")

;; overwrite selection
(delete-selection-mode)

;; kill cruft
(menu-bar-mode -1)
(tool-bar-mode -1)

;; wordy prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; default back-end is really verbose, including whole branch
(defadvice vc-mode-line (after strip-backend () activate)
  (when (stringp vc-mode)
    (setq vc-mode "")))

;; pull in changes from other programs when files change on disk
(global-auto-revert-mode)

;; completion
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; scrolling
(setq scroll-step 1 scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

;; don't break hard links when editing; edit the backing file
(setq backup-by-copying-when-linked t)

;; nicer startup
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; slightly more preferable (to me) window placement; I far far prefer vertical splits to horizontal
(setq split-height-threshold 150)

;; make minibuffer text read-only to avoid edge case bugginess
(setq minibuffer-prompt-properties
      `(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;; GUI emacs
(if window-system
    (progn
      (setq cursor-type '(bar . 2))
      (blink-cursor-mode t)
      (setq frame-title-format "Barry's Emacs")
      (setq mouse-wheel-progressive-speed nil)
      ;; console emacs has no fringe and margin mode interferes with linum; only use in UI
      (require 'diff-hl)
      (global-diff-hl-mode)))

;; semantic navigation
(setq imenu-max-item-length 120)

;; whitespace-mode
(setq whitespace-line-column 120)

;; enable disabled things
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; don't lose region for some commands
(defmacro keep-region (command)
  "Wrap command in code that saves and restores the region"
  (letrec ((command-name (symbol-name command))
           (advice-name (concat command-name "-keep-region")))
    `(progn
       (defadvice ,command (around ,(intern advice-name))
         (let ((deactivate-mark nil)
               (transient-mark-mode transient-mark-mode))
           (save-excursion
             ad-do-it)))
       (ad-activate (quote ,command)))))
(keep-region replace-string)
(keep-region replace-regexp)
(keep-region anzu-query-replace-regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC KEY REMAPS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "C-z") 'undo) ; C-x C-z still backgrounds
(define-key global-map (kbd "M-z") 'zop-to-char)
(define-key global-map (kbd "M-?") 'hippie-expand)
(define-key global-map (kbd "M-W") 'fixup-whitespace)
(define-key global-map (kbd "C-c w") 'write-region)
(define-key global-map (kbd "C-c i") 'string-inflection-all-cycle)
(define-key global-map (kbd "M-<delete>") 'kill-word)
(define-key global-map (kbd "M-A") 'align-regexp)

;; outsource job of finding something to compile to a shell script
(define-key global-map (kbd "<f9>") '(lambda ()
                                       (interactive)
                                       (compile "make-parent")))

;; window movement
(define-key global-map (kbd "M-<left>") 'windmove-left)
(define-key global-map (kbd "M-<right>") 'windmove-right)
(define-key global-map (kbd "M-<up>") 'windmove-up)
(define-key global-map (kbd "M-<down>") 'windmove-down)

;; number inc / dec
(define-key global-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key global-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; scrolling without affecting cursor
(defun scroll-up-line-other-window ()
  "Scroll other window up one line"
  (interactive)
  (scroll-other-window 1))
(defun scroll-down-line-other-window ()
  "Scroll other window down one line"
  (interactive)
  (scroll-other-window-down 1))
(define-key global-map (kbd "C-<up>") 'scroll-down-line)
(define-key global-map (kbd "C-<down>") 'scroll-up-line)
(define-key global-map (kbd "M-S-<up>") 'scroll-down-line-other-window)
(define-key global-map (kbd "M-S-<down>") 'scroll-up-line-other-window)

;; search and replace
(define-key global-map (kbd "M-R") 'anzu-query-replace-regexp)
(define-key global-map (kbd "C-s") 'isearch-forward-regexp)
(define-key global-map (kbd "C-r") 'isearch-backward-regexp)
;; would like to have a non-query replace...
(define-key global-map (kbd "M-r") 'replace-regexp)

;; quicker access to rgrep; rgrep + wgrep => sweet stuff
(define-key global-map (kbd "M-s g") 'rgrep)

;; pop global mark - go back; we'll see how well it works in practice
(define-key global-map (kbd "C-,") 'pop-global-mark)
;; C-. should be mapped to find-funtion-at-point or something based on ctags if possible

;; preemptively stop buffer buildup; quoted insert is not needed very often
(define-key global-map (kbd "C-q") 'kill-this-buffer)
(define-key global-map (kbd "M-Q") 'quoted-insert)

;; number manipulation
(define-key global-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key global-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(blink-cursor-mode t)
 '(cursor-type (quote (bar . 2)))
 '(custom-enabled-themes (quote (barrkel-4)))
 '(custom-safe-themes
   (quote
    ("789d365625c25564557d728a2eb0a0141f8bc2c250c786deddf55e912d3628e3" "552d3e01a9742e7635c4a46388185ca70e3d08063cc6678212381d6fe27f6bbb" "dabaad95869f1c29ee5f78895a05f219ce28847fc33ea783342fe6478622a09c" "26ca642ed44744c966c1aa136a7f8996f43d2669aa8e4e77c3c260e4c8bad7f7" "819eee0a99068671570dc0db2a49e79194e7402c5f431cf9e5fe445f74f3a8a8" "0304f6773d997609f818be75eef73ae557351c9c284cedcdcd55f0747f6ed586" "558410d7803ff018a67c30659ac7eb48daf1a8f2f88513c57b3e3ebac71aa3fb" "c7e0422d3b032d66fd1666da6099182689a815d078f03c3db4c3288e66ba6a26" "b18119d24b0b4cd9998b2ba21654ada087b7c5f7a7d2fcbdc15102c305375c65" "3afe4800dfb9d048efe2f759894424b91b0a773b0abb63973fb33cd056f96d34" "30f083d649543e3568a1547aaf903e10a59a2b45d0363e070b67acc2df8d4eb4" default)))
 '(dired-listing-switches "-alh")
 '(magit-push-current-set-remote-if-missing nil)
 '(package-selected-packages
   (quote
    (company-lsp company ksp-cfg-mode kubel kubernetes kubernetes-helm kubernetes-tramp racer realgud-byebug realgud-pry realgud k8s-mode rjsx-mode lsp-javascript-typescript treemacs lsp-java lsp-ruby helm-lsp powershell zop-to-char yaml-mode yafolding wgrep web-mode volatile-highlights string-inflection smartrep slim-mode scss-mode rspec-mode puppet-mode ox-reveal operate-on-number multiple-cursors move-text markdown-mode magit julia-mode js2-mode inf-ruby iedit htmlize highlight-indentation highlight-indent-guides helm-projectile helm-git-grep haskell-mode haml-mode god-mode go-mode git-timemachine format-sql flycheck fireplace expand-region evil-numbers esqlite-helm enh-ruby-mode edbi-sqlite discover-my-major diff-hl csv-mode csharp-mode color-theme coffee-mode cargo avy-zap anzu)))
 '(parens-require-spaces nil)
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values
   (quote
    ((dockerfile-image-name . "hadoop-in-a-box")
     (docker-image-name . "duco-encvol")
     (docker-image-name . "ldap-for-hadoop")
     (docker-image-name . "hadoop-in-a-box")
     (docker-image-name . "cdh5-duco")
     (docker-image-name . "impala-builder")
     (docker-image-name . "hadoop-kitchen-sink")
     (docker-image-name . "hadoop-single")
     (docker-image-name . "hadoop-base")
     (docker-image-name . debian7-jdk8)
     (docker-image-name . "cloudera-host")
     (docker-image-name . "cloudera-path-a")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(match ((t (:background "#73d216" :foreground "black"))))
 '(wgrep-done-face ((t (:foreground "LightSkyBlue")))))
