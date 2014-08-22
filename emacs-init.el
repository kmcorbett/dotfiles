;;;; -*- Mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode: nil -*-

;;; My all-purpose, singing and dancing Emacs init file! [kmcorbett@gmail.com]
;;; 1) Environment variables for shells
;;; 2) Editing basics: enable commands and bind keys
;;; 3) Load paths and optional modules
;;; 4) Modules for programming, writing, editing:
;;;    Common Lisp (editing)
;;;    Slime
;;;    ECB 
;;;    Auto-Complete
;;;    VC for Subversion, darcs, etc
;;;    Markdown mode
;;; 5) Startup and global flags
;;; 6) Customization forms

(eval-when-compile 
  (require 'cl))

(let ((path "/Applications/Emacs.app"))
  (when (and (file-exists-p path)
             (zerop (or (string-match-p (concat "^" path) invocation-directory) -1)))
    (cd (getenv "HOME"))))

;;;; 1) Environment variables for shells

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
    color-theme auto-complete markdown-mode pandoc-mode
    git-commit-mode git-rebase-mode gitconfig-mode magit
    dash-at-point slime paredit clojure-mode haskell-mode))

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

;;; 4) Modules for programming:
;;;    Slime
;;;    Auto-Complete
;;;    ECB 
;;;    VC for Subversion, darcs, etc
;;;    Modes for PHP, ...

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
;;;

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Clojure
;;; TODO: load cider
(require 'clojure-mode)
(add-to-list 'magic-mode-alist '("\\.clj" . clojure-mode))
(add-hook 'clojure-mode-hook 'paredit-mode)

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
(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

;;; Git
(require 'git-commit-mode)
(require 'git-rebase-mode)
(require 'magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5) startup and global flags

(setq inhibit-startup-message t)
(setq visible-bell t)
(display-time)

(ignore-errors (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 6) Customization

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

(when (file-exists-p "~/.emacs.d/local.el")
  (load-file "~/.emacs.d/local.el"))

;;; init.el ends here

;; Local Variables:
;; time-stamp-start: "Updated: +"
;; time-stamp-end: "$"
;; End:
