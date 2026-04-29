;;;; -*- Mode: Emacs-Lisp; tab-width: 2; indent-tabs-mode: nil -*-

;; My all-purpose, singing and dancing Emacs init file! [kmcorbett@gmail.com]
;; 1) General environment
;; 2) Editing basics: enable commands and bind keys
;; 3) Load paths and optional packages
;; 4) Programming: Common Lisp, Clojure, Git, etc
;; 5) Startup and global flags
;; 6) Customization forms

;;(eval-when-compile (require 'cl))

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
;; (when (file-exists-p "~/.bash_profile")
;;   (setq explicit-bash-args '("--login" "--init-file" "~/.bash_profile" "-i")))

;;; PATH setup - don't count on Mac OS X to get the same PATH as terminal apps
(let ((my-bin (expand-file-name "~/bin")))
  (unless (member my-bin exec-path)
    (setq exec-path (cons my-bin exec-path)))
  (setenv "PATH"
          (concat my-bin path-separator (or (getenv "PATH") ""))))

;; Always ALWAYS use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Fonts
(when (eq system-type 'darwin)
  ;; disable tool bar, menu bar, full screen
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :family "Hack")
  ;; default font size (point * 10)
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly.
  (set-face-attribute 'default nil :height 165)
  ;; if you want to use different font size for specific charset,
  ;; add :size POINT-SIZE in the font-spec.
  (set-fontset-font t 'ascii (font-spec :size 12 :name "Hack"))
  ;; you may want to add different for other charset in this way
  ;; also for a problem with svg image type
  (setq image-types (cons 'svg image-types)))



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
(global-set-key (kbd "C-c r") #'recompile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 3) Load paths and optional packages

;;; Local scripts
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;; Package manager
(require 'package)
(setq package-check-signatures nil)

;;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpaother" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
         '("melpa-stable" . "https://stable.melpa.org/packages/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;; Packages I use

(defvar my-packages-list
  '( ;;cl-lib
    ;; auto-complete
    ;;color-theme
    markdown-mode
    ;; pandoc-mode doc-view w3m
    git-modes ; git-commit-mode git-rebase-mode gitconfig-mode
    magit
    ;;dash-at-point
    ;;slime
    paredit
    ;;clojure-mode cider
    ;;haskell-mode
    scala-mode
    ;;go-mode
    yaml-mode
))

;; Assumes package.el is already initialized and use-package is installed.

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

;; use-package handles :mode and :ensure for us, so we can just declare the packages we want to use with it
(use-package markdown-mode :ensure t
             :mode (("\\.md\\'" . markdown-mode) ("\\.MD\\'" . markdown-mode) ("\\.markdown\\'" . markdown-mode) ("\\.MARKDOWN\\'" . markdown-mode)))
(use-package ttl-mode :ensure t :mode (("\\.ttl\\'" . ttl-mode) ("\\.trig\\'" . ttl-mode)))
;;(use-package n3-mode :ensure t :mode (("\\.n3\\'" . n3-mode) ("\\.nt\\'" . n3-mode)))
(use-package sparql-mode
  :ensure t
  :mode (("\\.rq\\'"      . sparql-mode)
         ("\\.sparql\\'"  . sparql-mode)
         ("\\.ru\\'"      . sparql-mode)))
(use-package scala-mode :ensure t :mode (("\\.scala\\'" . scala-mode)))
(use-package yaml-mode :ensure t :mode (("\\.yml\\'" . yaml-mode) ("\\.yaml\\'" . yaml-mode)))
;; (use-package copilot
;;   :ensure t
;;   :hook ((prog-mode . copilot-mode)
;;          (yaml-mode . copilot-mode)
;;          (yaml-ts-mode . copilot-mode)
;;          (ttl-mode . copilot-mode)
;;          (n3-mode . copilot-mode)
;;          (sparql-mode . copilot-mode))
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . copilot-accept-completion)
;;               ("TAB"   . copilot-accept-completion)
;;               ("C-TAB" . copilot-accept-completion-by-word)
;;               ("C-<tab>" . copilot-accept-completion-by-word))
;;   :config
;;   (setq copilot-indent-offset-warning-disable t)
;;   (add-to-list 'copilot-indentation-alist '(yaml-mode yaml-indent-offset))
;;   (add-to-list 'copilot-indentation-alist '(yaml-ts-mode yaml-indent-offset))
;;   (add-to-list 'copilot-indentation-alist '(ttl-mode tab-width))
;;   (add-to-list 'copilot-indentation-alist '(n3-mode tab-width))
;;   (add-to-list 'copilot-indentation-alist '(sparql-mode tab-width)))

;;; Color theme
;; (require 'color-theme)
;; (eval-after-load "color-theme"
;;   '(progn
;;     (color-theme-initialize)
;;     ;;(color-theme-hober)
;;     ))

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
;; (unless (package-installed-p 'slime)
;;   (let ((slime-helper (expand-file-name "~/quicklisp/slime-helper.el")))
;;     (if (file-exists-p slime-helper)
;;         (load slime-helper)
;;         (warn "Quicklisp Slime helper not found, Slime may not work"))))

;;(require 'slime-autoloads)

;; Slime REPL? fancy REPL? Tramp for editing remote files?
;(slime-setup '(slime-repl))
;(slime-setup '(slime-fancy slime-tramp))
;;(slime-setup '(slime-fancy))

;; (setq slime-lisp-implementations
;;       `((lw-console ("lw-console") :directory ,(or (getenv "DT_HOME")(getenv "HOME")))
;;         (lispworks ("lispworks-6-1-0-amd64-linux"))
;;         (ccl ("ccl" "-K" "utf-8") :coding-system utf-8-unix)))

;; Use UTF-8 character encoding
(set-language-environment "utf-8")
;;(setq slime-net-coding-system 'utf-8-unix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GO setup

;; (require 'go-mode-autoloads)
;; (add-to-list 'magic-mode-alist '("\\.go" .go-mode))
;;(add-hook 'go-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-Complete

;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)
;; ;; Dirty fix for having Auto Complete everywhere
;; (define-globalized-minor-mode real-global-auto-complete-mode
;;     auto-complete-mode (lambda ()
;;                          (if (not (minibufferp (current-buffer)))
;;                              (auto-complete-mode 1))
;;                          ))
;; (real-global-auto-complete-mode t)

;;; Dash
;; (autoload 'dash-at-point "dash-at-point"
;;           "Search the word at point with Dash." t nil)
;; (global-set-key [(C c) C d] 'dash-at-point)
;; (global-set-key [(C u) (C c) (C d) d] 'dash-at-point-with-docset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modes and Hooks

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; Python
(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (concat "python3 " buffer-file-name))))

;;; Clojure
;; (require 'clojure-mode)
;; (require 'cider)

;; (add-to-list 'magic-mode-alist '("\\.clj" . clojure-mode))
;; (add-hook 'clojure-mode-hook 'paredit-mode)
;; (add-hook 'clojure-mode-hook 'auto-fill-mode)

;; (setq cider-repl-wrap-history t)
;; (setq cider-repl-history-file "~/.emacs.d/nrepl-history")

;;; Haskell - for great good
;; (require 'haskell-mode)
;; (add-to-list 'magic-mode-alist '("\\.hs" . haskell-mode))
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;; Java
;; (add-hook 'java-mode
;;           (lambda ()
;;             "Fix java mode tabs"
;;             (setq c-basic-offset 4 tab-width 4 indent-tabs-mode nil)
;;             (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
;;             (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;;; Lisp
;; (require 'lisp-mode)
;; (add-hook 'lisp-mode-hook 'paredit-mode)

;;; Markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
;;;; Pandoc-based Markdown preview (HTML + PDF) with
;;;; persistent error capture and auto-refresh
;;;; Tested with Emacs 29.x

;; ------------------------------------------------------------
;; Core runner
;; ------------------------------------------------------------

(defun kmc--pandoc-run (output-ext pandoc-args open-fn)
  "Run pandoc on the current Markdown buffer.

OUTPUT-EXT is a string like \"html\" or \"pdf\".
PANDOC-ARGS is a list of arguments passed to pandoc.
OPEN-FN is called with the output file path on success.

All pandoc stdout/stderr is captured in *pandoc-output*."
  (let* ((in buffer-file-name)
         (out (make-temp-file "pandoc-preview-" nil
                              (concat "." output-ext)))
         (buf (get-buffer-create "*pandoc-output*")))
    (unless in
      (error "Current buffer is not visiting a file"))
    (with-current-buffer buf
      (erase-buffer))
    (let ((exit-code
           (apply #'call-process
                  "pandoc"
                  nil          ; no stdin
                  buf          ; stdout
                  t            ; stderr -> same buffer
                  (append pandoc-args
                          (list "-o" out in)))))
      (display-buffer buf)
      (if (eq exit-code 0)
          (funcall open-fn out)
        (message "Pandoc failed with exit code %s" exit-code)))))

;; ------------------------------------------------------------
;; HTML preview
;; ------------------------------------------------------------

(defun markdown-preview-pandoc-html ()
  "Preview current Markdown buffer as HTML using pandoc.
All output is captured in *pandoc-output*."
  (interactive)
  (kmc--pandoc-run
   "html"
   '("-f" "gfm" "-s" "--resource-path=.")
   (lambda (out)
     (when (eq system-type 'darwin)
       (call-process "open" nil 0 nil out)))))

;; ------------------------------------------------------------
;; PDF preview (requires tectonic or another TeX engine)
;; ------------------------------------------------------------

(defun markdown-preview-pandoc-pdf ()
  "Preview current Markdown buffer as PDF using pandoc + tectonic.
All output is captured in *pandoc-output*."
  (interactive)
  (kmc--pandoc-run
   "pdf"
   '("-f" "gfm" "--pdf-engine=tectonic")
   (lambda (out)
     (when (eq system-type 'darwin)
       (call-process "open" nil 0 nil out)))))

;; ------------------------------------------------------------
;; Auto-refresh on save (HTML)
;; ------------------------------------------------------------

(defvar-local kmc-pandoc-auto-refresh nil
  "If non-nil, auto-refresh HTML preview on save.")

(defun kmc-pandoc--after-save ()
  (when kmc-pandoc-auto-refresh
    (markdown-preview-pandoc-html)))

(defun markdown-pandoc-auto-refresh-enable ()
  "Enable automatic HTML preview refresh on save."
  (interactive)
  (setq kmc-pandoc-auto-refresh t)
  (add-hook 'after-save-hook #'kmc-pandoc--after-save nil t)
  (message "Pandoc auto-refresh enabled"))

(defun markdown-pandoc-auto-refresh-disable ()
  "Disable automatic HTML preview refresh on save."
  (interactive)
  (setq kmc-pandoc-auto-refresh nil)
  (remove-hook 'after-save-hook #'kmc-pandoc--after-save t)
  (message "Pandoc auto-refresh disabled"))

;; ------------------------------------------------------------
;; Key bindings (Markdown mode only)
;; ------------------------------------------------------------

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c m h")
              #'markdown-preview-pandoc-html)
  (define-key markdown-mode-map (kbd "C-c m p")
              #'markdown-preview-pandoc-pdf)
  (define-key markdown-mode-map (kbd "C-c m a")
              #'markdown-pandoc-auto-refresh-enable)
  (define-key markdown-mode-map (kbd "C-c m A")
              #'markdown-pandoc-auto-refresh-disable))
;;; Git
;; (require 'git-commit-mode)
;; (require 'git-rebase-mode)

(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

;;; w3m Web browser

;;change default browser for 'browse-url'  to w3m
;; (setq browse-url-browser-function 'w3m-goto-url-new-session)

;;change w3m user-agent to android
;; (setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")

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
;; (defun wikipedia-search (search-term)
;;   "Search for SEARCH-TERM on wikipedia"
;;   (interactive
;;    (let ((term (if mark-active
;;                    (buffer-substring (region-beginning) (region-end))
;;                  (word-at-point))))
;;      (list
;;       (read-string
;;        (format "Wikipedia (%s):" term) nil nil term)))
;;    )
;;   (browse-url
;;    (concat
;;     "http://en.m.wikipedia.org/w/index.php?search="
;;     search-term
;;     ))
;;   )

;;when I want to enter the web address all by hand
;; (defvar my-w3m-last-site "google.com")
;; (defun w3m-open-site (site)
;;   "Opens site in new w3m session with 'http://' appended"
;;   (interactive
;;    (list
;;     (setq my-w3m-last-site
;;           (read-string
;;            (format "Enter website address(default: %s):" my-w3m-last-site)
;;            nil nil my-w3m-last-site nil ))))
;;   (w3m-goto-url-new-session
;;    (concat "http://" site)))

;; (defun w3m-browse-current-buffer ()
;;   (interactive)
;;   (let ((filename (concat (make-temp-file "w3m-") ".html")))
;;     (unwind-protect
;;          (progn
;;            (write-region (point-min) (point-max) filename)
;;            (w3m-find-file filename))
;;       (delete-file filename))))

;; (load "eval-in-repl-init.el")

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
 '(confirm-kill-emacs 'y-or-n-p)
 '(gc-cons-threshold 500000000)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(indent-tabs-mode nil)
 '(large-file-warning-threshold nil)
 '(mode-require-final-newline 'ask)
 '(package-selected-packages
   '(git-modes markdown-mode paredit scala-mode sparql-mode
     treemacs-evil treemacs-icons-dired treemacs-magit treemacs-persp
     treemacs-projectile treemacs-tab-bar ttl-mode use-package
     yaml-mode))
 '(require-final-newline 'ask)
 '(safe-local-variable-values
   '((eval setq tab-width 2) (eval setq indent-tabs-mode nil)
     (eval local-set-key (kbd "TAB") 'tab-to-tab-stop) (indent-tabs)
     (Package . YB) (Package . GUI) (indent-tabs-mode)
     (Package . utils) (Package . HUNCHENTOOT) (Syntax . COMMON-LISP)
     (Package . CL-USER) (Package . CCL) (Syntax . ANSI-Common-Lisp)
     (Base . 10)))
 '(slime-repl-shortcut-dispatch-char 44)
 '(standard-indent 2)
 '(tab-width 2)
 '(undo-limit 800000)
 '(undo-outer-limit 120000000)
 '(undo-strong-limit 1200000)
 '(visible-cursor t))

;;;; 7) Treemacs

(require 'treemacs)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        t
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;;(treemacs-start-on-boot)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; 8) Copilot

;; (use-package copilot
;;   :hook (prog-mode . copilot-mode)
;;   :bind (("C-c c" . copilot-complete)
;;          :map copilot-completion-map
;;               ("<tab>"   . copilot-accept-completion)
;;               ("TAB"     . copilot-accept-completion)
;;               ("C-<tab>" . copilot-accept-completion-by-word)
;;               ("C-TAB"   . copilot-accept-completion-by-word)
;;               ("M-n"   . copilot-next-completion)
;;               ("M-p"   . copilot-previous-completion)))

;; ;;; Copilot Chat

;; (use-package copilot-chat
;;   :after copilot
;;   :bind (("C-c p p" . copilot-chat-display)
;;          ("C-c p e" . copilot-chat-explain)
;;          ("C-c p r" . copilot-chat-review)
;;          ("C-c p f" . copilot-chat-fix)
;;          ("C-c p o" . copilot-chat-optimize)
;;          ("C-c p d" . copilot-chat-doc)
;;          ("C-c p b" . copilot-chat-add-current-buffer)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
