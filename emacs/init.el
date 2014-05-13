;;;; -*- Mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode: nil -*-

;;; kmcorbett's all-purpose, singing and dancing Emacs init file!
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

(require 'cl)
(cd (getenv "HOME")) 

;;;; 1) Environment variables for shells

(setenv "PAGER" "")
(setenv "GIT_PAGER" "")
(setenv "EDITOR" "emacsclient")

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

;;;; 3) Load paths and optional modules

(add-to-list 'load-path "~/.emacs.d/")

;;; Color theme
(when (file-exists-p "~/.emacs.d/color-theme-6.6.0/color-theme.el")
  (add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/")
  (require 'color-theme)
  (eval-after-load "color-theme"
    '(progn
       (color-theme-initialize)
       ;;(color-theme-hober)
       )))

;;; 4) Modules for programming:
;;;    Slime
;;;    Auto-Complete
;;;    ECB 
;;;    VC for Subversion, darcs, etc
;;;    Modes for PHP, ...

(setq lisp-indent-function 'common-lisp-indent-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slime

(setq inferior-lisp-program
      (cond
        ;; Leave current value alone
        (inferior-lisp-program)
        ;; LispWorks console image
        ((file-exists-p "~/bin/lw-console")
         "~/bin/lw-console")
        ((file-exists-p "/usr/local/bin/ccl")
         "ccl -K utf-8")
        ((getenv "LISP"))
        (t "lisp")))

;; Use UTF-8 character encoding
(set-language-environment "utf-8")
(setq slime-net-coding-system 'utf-8-unix)

;; Autoloading? or loading when Emacs starts?
;(require 'slime-autoloads)
;(require 'slime)

;; Slime from Quicklisp
(let ((slime-helper (expand-file-name "~/quicklisp/slime-helper.el")))
  (when (file-exists-p slime-helper)
    (load slime-helper)))

;; Slime REPL? fancy REPL? Tramp for editing remote files?
;(slime-setup '(slime-repl))
;(slime-setup '(slime-fancy slime-tramp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-Complete

(when (file-exists-p "~/.emacs.d/auto-complete-1.3.1/auto-complete-config.el")
  (add-to-list 'load-path "~/.emacs.d/auto-complete-1.3.1")
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  ;; dirty fix for having Auto Complete everywhere
  (define-globalized-minor-mode real-global-auto-complete-mode
      auto-complete-mode (lambda ()
                           (if (not (minibufferp (current-buffer)))
                               (auto-complete-mode 1))
                           ))
  (real-global-auto-complete-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hooks
;;;

;;; PHP
;(load-file "~/.emacs.d/php-mode-1.5.0/php-mode.el")

(setq text-mode-hook 'turn-on-auto-fill)
;;(setq emacs-lisp-mode-hook 'turn-on-auto-fill)
;;(setq lisp-mode-hook 'turn-on-auto-fill)

;; Verisk CIF files
(add-to-list 'magic-mode-alist '(".*:properties.*:cif-version" . text-mode))

;;; Darcs
;(load-file "~/.emacs.d/vc-darcs.el")
;(add-to-list 'vc-handled-backends 'DARCS)
;(autoload 'vc-darcs-find-file-hook "vc-darcs")
;(add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

(when (file-exists-p "~/.emacs.d/markdown-mode.el")
  (autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text" . markdown-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5) startup and global flags

(setq inhibit-startup-message t)
(setq visible-bell t)
(display-time)

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
 '(markdown-command "~/bin/Markdown.pl")
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
