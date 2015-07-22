;;;; -*- Mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode: nil -*-

;; My all-purpose, singing and dancing Emacs init file! [kmcorbett@gmail.com]
;; 1) General environment
;; 2) Editing basics: enable commands and bind keys
;; 3) Load paths and optional packages
;; 4) Programming: Common Lisp, Clojure, Git, etc
;; 5) Startup and global flags
;; 6) Customization forms

(eval-when-compile (require 'cl))

(let ((path "/Applications/Emacs.app"))
  (when (and (file-exists-p path)
             (zerop (or (string-match-p (concat "^" path) invocation-directory) -1)))
    (cd (getenv "HOME"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 1) General environment

(setenv "PAGER" "cat")
(setenv "GIT_PAGER" "cat")
(setenv "EDITOR" "emacsclient")

;;; Bash profile for shell-mode
(when (file-exists-p "~/.bash_profile")
  (setq explicit-bash-args '("--login" "--init-file" "~/.bash_profile" "-i")))

;;; PATH setup - some for Mac OS X - emulating PATH in Terminal app
;;; Emacs 24 sets PATH from environment, so this is not needed
(let ((my-path-directories
       `("~/bin" "/Developer/usr/bin" "/usr/local/git/bin" "/usr/local/bin")))
  (setenv "PATH"
          (mapconcat
           'identity
           (delete-dups
            (append
             (mapcar (lambda (path)
                       (if (string-match "^~" path)
                           (replace-match (getenv "HOME") nil nil path)
                         path))
                     my-path-directories)
             (split-string (getenv "PATH") ":")))
           ":"))
  (mapc (lambda (path) (push path exec-path))
        my-path-directories))

;; Always ALWAYS use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Fonts
(when (eq system-type 'darwin)
  ;; disable tool bar, menu bar, full screen
  (tool-bar-mode -1)
  ;(menu-bar-mode -1)
  (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :family "Consolas")
  ;; default font size (point * 10)
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly. 
  (set-face-attribute 'default nil :height 165)
  ;; if you want to use different font size for specific charset,
  ;; add :size POINT-SIZE in the font-spec.
  (set-fontset-font t 'ascii (font-spec :size 12 :name "Andale Mono"))
  ;; you may want to add different for other charset in this way
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 2) Editing basics: enable commands and bind keys

;;; Enable commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'scroll-left 'disabled nil)

;;; Keys
(define-key global-map "\M-[" 'backward-paragraph)
(define-key global-map "\M-]" 'forward-paragraph)
(define-key global-map "\M-j" 'fill-region-as-paragraph)
(define-key global-map "\C-xg" 'goto-line)
(define-key global-map "\C-x\C-g" 'keyboard-quit)
(define-key global-map "\C-x "     'set-mark-command)
(define-key global-map "\M-s" 'center-line)
(define-key global-map "\M-(" 'insert-parentheses)
(define-key global-map "\M-+" 'tags-loop-continue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 3) Load paths and optional packages

;;; Local scripts
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;; Package manager
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;; Packages I use

(defvar my-packages-list 
  '(cl-lib
    auto-complete
    color-theme
    markdown-mode pandoc-mode doc-view w3m
    git-commit-mode git-rebase-mode gitconfig-mode magit
    dash-at-point slime paredit
    clojure-mode cider
    haskell-mode scala-mode go-mode))

;; Use Melpa package repo 
(setq package-user-dir "~/.emacs.d/elpa/")
(package-initialize)

(defvar package-refresh-p t)

(defun install-my-packages ()
  (interactive)
  (when package-refresh-p
    (package-refresh-contents))
  ;; Maybe install each package
  (mapc #'(lambda (name)
            (unless (package-installed-p name)
              (package-install name)))
        my-packages-list))

(install-my-packages)

;;; Color theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
    (color-theme-initialize)
    ;;(color-theme-hober)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4) Programming:
;;;    Auto-Complete
;;;    VC for Git, Subversion, darcs, etc
;;;    Dash
;;;    Java
;;;    Common Lisp / Slime, Clojure / Cider
;;;    Markdown mode

(setq lisp-indent-function 'common-lisp-indent-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SLIME setup

;; Prefer to get Slime from Elpa, but if skipped or failed try Quicklisp
(unless (package-installed-p 'slime)
  (let ((slime-helper (expand-file-name "~/quicklisp/slime-helper.el")))
    (if (file-exists-p slime-helper)
        (load slime-helper)
        (warn "Quicklisp Slime helper not found, Slime may not work"))))

(require 'slime-autoloads)

;; Slime REPL? fancy REPL? Tramp for editing remote files?
;(slime-setup '(slime-repl))
;(slime-setup '(slime-fancy slime-tramp))
(slime-setup '(slime-fancy))

(setq slime-lisp-implementations
      `((lw-console ("lw-console") :directory ,(or (getenv "DT_HOME")(getenv "HOME")))
        (lispworks ("lispworks-6-1-0-amd64-linux"))
        (ccl ("ccl" "-K" "utf-8") :coding-system utf-8-unix)))

;; Use UTF-8 character encoding
(set-language-environment "utf-8")
(setq slime-net-coding-system 'utf-8-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO setup

(require 'go-mode-autoloads)
(add-to-list 'magic-mode-alist '("\\.go" .go-mode))
;;(add-hook 'go-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-Complete

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;; Dirty fix for having Auto Complete everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
    auto-complete-mode (lambda ()
                         (if (not (minibufferp (current-buffer)))
                             (auto-complete-mode 1))
                         ))
(real-global-auto-complete-mode t)

;;; Dash
(autoload 'dash-at-point "dash-at-point"
          "Search the word at point with Dash." t nil)
(global-set-key "\C-cd" 'dash-at-point)
(global-set-key "\C-ce" 'dash-at-point-with-docset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modes and Hooks

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Clojure
(require 'clojure-mode)
(require 'cider)

(add-to-list 'magic-mode-alist '("\\.clj" . clojure-mode))
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'auto-fill-mode)

(setq cider-repl-wrap-history t)
(setq cider-repl-history-file "~/.emacs.d/nrepl-history")

;;; Haskell - for great good
(require 'haskell-mode)
(add-to-list 'magic-mode-alist '("\\.hs" . haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;; Java
(add-hook 'java-mode
          (lambda ()
            "Fix java mode tabs"
            (setq c-basic-offset 4 tab-width 4 indent-tabs-mode nil)
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;;; Lisp
(require 'lisp-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)

;;; Verisk CIF files
(add-to-list 'magic-mode-alist '(".*:properties.*:cif-version" . text-mode))

;;; Markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(setq markdown-command "~/bin/Markdown.pl")

;;; Pandoc for translating Markdown to HTML etc
(require 'pandoc-mode)
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

;;; Git
(require 'git-commit-mode)
(require 'git-rebase-mode)

(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

;;; w3m Web browser

;;change default browser for 'browse-url'  to w3m
(setq browse-url-browser-function 'w3m-goto-url-new-session)
 
;;change w3m user-agent to android
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")
 
;;quick access hacker news
;; (defun hn ()
;;   (interactive)
;;   (browse-url "http://news.ycombinator.com"))
 
;; ;;quick access reddit
;; (defun reddit (reddit)
;;   "Opens the REDDIT in w3m-new-session"
;;   (interactive (list
;;                 (read-string "Enter the reddit (default: psycology): " nil nil "psychology" nil)))
;;   (browse-url (format "http://m.reddit.com/r/%s" reddit))
;;   )
 
;; TODO: Do I need this?
(defun wikipedia-search (search-term)
  "Search for SEARCH-TERM on wikipedia"
  (interactive
   (let ((term (if mark-active
                   (buffer-substring (region-beginning) (region-end))
                 (word-at-point))))
     (list
      (read-string
       (format "Wikipedia (%s):" term) nil nil term)))
   )
  (browse-url
   (concat
    "http://en.m.wikipedia.org/w/index.php?search="
    search-term
    ))
  )
 
;;when I want to enter the web address all by hand
(defvar my-w3m-last-site "google.com")
(defun w3m-open-site (site)
  "Opens site in new w3m session with 'http://' appended"
  (interactive
   (list
    (setq my-w3m-last-site
          (read-string
           (format "Enter website address(default: %s):" my-w3m-last-site)
           nil nil my-w3m-last-site nil ))))
  (w3m-goto-url-new-session
   (concat "http://" site)))

(defun w3m-browse-current-buffer ()
  (interactive)
  (let ((filename (concat (make-temp-file "w3m-") ".html")))
    (unwind-protect
         (progn
           (write-region (point-min) (point-max) filename)
           (w3m-find-file filename))
      (delete-file filename))))

(load "eval-in-repl-init.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5) startup and global flags

(setq inhibit-startup-message t)
(setq visible-bell t)
(display-time)

(ignore-errors (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 6) Customization

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-delay 0.2)
 '(blink-cursor-interval 0.2)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(gud-gdb-command-name "gdb --annotate=1")
 '(indent-tabs-mode nil)
 '(large-file-warning-threshold nil)
 '(gc-cons-threshold 500000000)
 '(mode-require-final-newline (quote ask))
 '(require-final-newline (quote ask))
 '(safe-local-variable-values (quote ((indent-tabs) (Package . YB) (Package . GUI) (indent-tabs-mode) (Package . utils) (Package . HUNCHENTOOT) (Syntax . COMMON-LISP) (Package . CL-USER) (Package . CCL) (Syntax . ANSI-Common-Lisp) (Base . 10))))
 '(slime-repl-shortcut-dispatch-char 44)
 '(standard-indent 2)
 '(tab-width 2)
 '(undo-limit 800000)
 '(undo-outer-limit 120000000)
 '(undo-strong-limit 1200000)
 '(visible-cursor t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
; '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal))))
; '(font-lock-type-face ((((class color) (min-colors 88) (background "White")) (:foreground "DarkGreen" :underline nil))))
 '(font-lock-variable-name-face ((t (:foreground "selectedMenuItemColor" :underline t :weight bold))))
 '(slime-repl-output-face ((t (:inherit font-lock-string-face :foreground "#440044"))))
 '(slime-repl-prompt-face ((t (:inherit font-lock-keyword-face :foreground "#0022AA"))))
 '(slime-repl-result-face ((t (:foreground "darkgreen")))))

;; Override custom settings found in local.el
;; TODO: Test this idea as it may not work as intended
(when (file-exists-p "~/.emacs.d/local.el")
  (load-file "~/.emacs.d/local.el"))

;;; init.el ends here

;; Local Variables:
;; time-stamp-start: "Updated: +"
;; time-stamp-end: "$"
;; End:
