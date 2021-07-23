;;; init.el -- Init file
;;;
;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;; Variables and Options

(column-number-mode t)          ;; Enable column information in the modeline.
(save-place-mode t)             ;; Save our place in each file.
(show-paren-mode t)             ;; Highlight matching braces.
(size-indication-mode t)        ;; Show the size of the buffer in the modeline.
(tool-bar-mode -1)              ;; Disable the tool bar in the GUI.
(global-hl-line-mode t)         ;; Highlight the entire line ("cursorline" in Vim).
(xterm-mouse-mode)              ;; Enable mouse mode in terminals that support it.
(which-function-mode 1)         ;; Display the current function name in the mode line.

(setq
  vc-follow-symlinks t          ;; Always follow symlinks.
  scroll-margin 3               ;; Make sure there are at least 3 lines above or below the current line on-screen.
  scroll-conservatively 5       ;; Don't recenter point unless moving more than five lines outside of the frame.
  inhibit-startup-screen t      ;; Don't show the welcome screen.
  make-backup-files nil         ;; stop creating backup~ files
  auto-save-default nil         ;; stop creating #autosave# files
  load-prefer-newer t           ;; Prefer newest version of a file.
  ;gc-cons-threshold 100000000
  garbage-collection-messages t)
(fset 'yes-or-no-p 'y-or-n-p)   ;; Use 'y' instead of 'yes', etc.

(when (version<= "26.0.50" emacs-version)
  ;; Enable line numbers
  (global-display-line-numbers-mode))


;; NOTE: (almost) every invocation of eval-{when,and}-compile in this file is for no other reason
;; than to satisfy flycheck.

;; Automatically break long lines at the fill-column.
(setq-default auto-fill-function 'do-auto-fill)


;; Smooth scrolling...sorta.
(require 'pixel-scroll)
(setq pixel-resolution-fine-flag t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(pixel-scroll-mode t)


(eval-and-compile (require 'sh-script nil t))
(add-hook 'sh-mode-hook
  (lambda ()
    (setq-local sh-basic-offset 4)))


(require 'cc-vars)
(setq c-default-style
  '((java-mode . "java")
     (awk-mode . "awk")
     (other . "bsd")))


;; Tell Emacs where "Custom" configuration can go, and then load it.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)


;; Always start the server so that emacsclient can connect.
(require 'server)
(unless (server-running-p)
  (server-start))


;; For packages that aren't in [M]ELPA, copy or git clone them into ~/.emacs.d/lisp.
(let ((default-directory "~/.emacs.d/lisp"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Add MELPA to the package archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Work around a problem with ELPA and TLS 1.3 on Emacs < 27
;; See https://www.reddit.com/r/emacs/comments/cdf48c/failed_to_download_gnu_archive/
(require 'gnutls)
(when (version<= emacs-version "26.99.0")
  (defvar gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Not needed with use-package.
;; (package-initialize)

;; Use the "use-package" package to manage packages.
;; Make sure it's loaded *after* package.el but *before* anything else.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (package-initialize)
  (require 'use-package))

(require 'bind-key)

;; If a package "used" below doesn't exist, install it.
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package esup
  :ensure t
  ;; To use MELPA Stable use ":pin melpa-stable",
  :pin melpa)


;; GCMH - the Garbage Collector Magic Hack
;; https://github.com/emacsmirror/gcmh
(use-package gcmh
  :config
  (setq gcmh-high-cons-threshold 262144000) ; 250MiB
  (gcmh-mode 1))


;; Make Emacs use the $PATH set up by the user's shell
;; https://github.com/purcell/exec-path-from-shell
(eval-and-compile (require 'exec-path-from-shell nil t))
(use-package exec-path-from-shell
  :if (memq system-type '(usg-unix-v darwin gnu/linux))
  :init
  (setq exec-path-from-shell-arguments '("-l"))

  :config
  (exec-path-from-shell-initialize))

(use-package shfmt
  :hook (sh-mode . shfmt-on-save-mode)
  :custom (shfmt-arguments '("-i" "4")))


;; Project Interaction Library for Emacs
;; https://github.com/bbatsov/projectile
(eval-and-compile (require 'projectile nil t))
(use-package projectile
  :config
  (projectile-mode 1)
  (setq projectile-require-project-root t)
  (setq projectile-dynamic-mode-line nil)
  (setq projectile-indexing-method 'native)
  (setq projectile-globally-ignored-directories
    (append projectile-globally-ignored-directories '("target" "build")))

  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-register-project-type 'platformio '("platformio.ini"))
  (setq projectile-project-root-files-bottom-up
    '(".projectile" "platformio.ini" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs")))


;; The extensible vi layer for Emacs.
;; https://github.com/emacs-evil/evil
(use-package evil
  :init
  ;; Set up for evil-collection.
  (setq evil-want-integration t
    evil-want-keybinding nil
    evil-undo-system 'undo-tree
    ;; Take C-u back for scrolling a half-page up.
    evil-want-C-u-scroll t)
  (setq-default evil-symbol-word-search t)

  :config
  (add-hook 'after-init-hook 'evil-normalize-keymaps)

  (global-set-key (kbd "M-u") 'universal-argument)
  ;; (evil-set-initial-state 'cider-repl-mode 'emacs)
  ;; (evil-set-initial-state 'sly-mrepl-mode 'emacs)
  ;; (evil-set-initial-state 'sly-db-mode 'emacs)
  ;; (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)

  (evil-mode 1))

(defun my/cursor-state-change ()
  "Change the cursor to a bar in insert mode, and a box otherwise."
  (if (string= evil-state "insert")
    (setq cursor-type 'bar)
    (setq cursor-type 'box)))

(add-hook 'evil-insert-state-entry-hook #'my/cursor-state-change)
(add-hook 'evil-insert-state-exit-hook #'my/cursor-state-change)


;; A set of keybindings for evil-mode
;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :after evil
  :config (evil-collection-init))


;; Displays a visual hint on evil edit operations
;; https://github.com/edkolev/evil-goggles
(eval-and-compile (require 'evil-goggles nil t))
(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))


(defun my/package-file-p (&optional filename _noerror)
  "Return t if FILENAME resides under ~/.emacs.d/elpa."
  (interactive "GEnter filename: ")
  (let* ((elpa (expand-file-name "~/.emacs.d/elpa/"))
         (fname (if filename filename buffer-file-name))
         (expanded (expand-file-name fname)))
    (if (string-prefix-p elpa expanded)
      (progn
        (message "file under %s" elpa)
        t)
      nil)))

;; undo-tree, required for evil `C-r` redo functionality
;; https://www.emacswiki.org/emacs/UndoTree
(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (setq undo-limit (* 1024 1024)) ;; 10MB.
  (setq undo-strong-limit undo-limit)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (advice-add 'undo-tree-make-history-save-file-name :filter-return #'(lambda (FILENAME) (concat FILENAME ".gz")))
  (advice-add 'undo-tree-load-history :before-until #'my/package-file-p)
  (advice-add 'undo-tree-save-history :before-until #'my/package-file-p)
  (global-undo-tree-mode 1))


;; Modular in-buffer completion framework for Emacs
;; https://github.com/company-mode/company-mode
(use-package company
  :config
  (setq company-global-modes '(not comint-mode
                                eshell-mode
                                help-mode
                                message-mode)
    company-idle-delay 0.1
    company-minimum-prefix-length 1
    company-tooltip-align-annotations t)

  :hook (after-init . global-company-mode))


;; A company front-end with icons
;; https://github.com/sebastiencs/company-box
(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))


;; Editorconfig reads .editorconfig files and configures settings accordingly.
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :config (editorconfig-mode 1))


;; A minor-mode menu for the mode line
;; https://github.com/tarsius/minions
(use-package minions
  :config (minions-mode 1))


;; Emacs incremental completion and selection narrowing framework
;; https://github.com/emacs-helm/helm
(use-package helm
  :config
  (helm-mode 1)
  ;; Rebind M-x to use Helm mode.
  (global-set-key (kbd "M-x") 'helm-M-x)
  ;; Remap various functions to the Helm equivalent
  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
  (define-key global-map [remap execute-extended-command] 'helm-M-x)
  (unless (boundp 'completion-in-region-function)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

  :custom
  ;; Use 'rg' instead of 'ag' in Helm
  (helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
  (helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))
  (helm-mode-fuzzy-match t)
  (helm-completion-in-region-fuzzy-match t))


;; Package for highlighting uncommitted changes
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :config (global-diff-hl-mode))


;; On-the-fly spell checking
;; https://www.emacswiki.org/emacs/FlySpell
;; Requires either ispell, aspell, or hunspell
;; (Preferring aspell right now)
;; $ pkgin in aspell aspell-en
;;
;; Oh btw, this isn't an actual package so we're abusing use-package here (hence, ":ensure nil").
(use-package flyspell
  :ensure nil
  :defer 1

  :custom
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil))


(use-package ispell
  :ensure nil
  :defer 1
  :custom (ispell-silently-savep t)
  :init (cond
          ((executable-find "hunspell")
            (setq ispell-program-name "hunspell"
              ispell-local-dictionary "en_US"
              ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))

          ((executable-find "aspell")
            (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))))


;; A replacement for the emacs' built-in command `comment-dwim'.
;; https://github.com/remyferre/comment-dwim-2
(use-package comment-dwim-2
  :config
  (global-set-key (kbd "M-;") 'comment-dwim-2)
  (setq comment-dwim-2--inline-comment-behavior 'reindent-comment))

(eval-and-compile
  (require 'f nil t)
  (require 's nil t))
(defun my/rust-src-path ()
  "Find Rust's source path."
  (or
    ;(getenv "RUST_SRC_PATH")
    (when (executable-find "rustc")
      (let* ((sysroot (s-trim-right
                        (shell-command-to-string
                          (format "%s --print sysroot" (executable-find "rustc")))))
              (library-path (f-join sysroot "lib/rustlib/src/rust/library"))
              (lib-path (f-join sysroot "lib/rustlib/src/rust/src"))
              (src-path (cond
                          ((file-exists-p library-path) library-path)
                          ((file-exists-p lib-path) lib-path))))
        (when (file-exists-p src-path)
          src-path)
        src-path))
    "/usr/local/src/rust/src"))

;; Rust support
;; https://github.com/rust-lang/rust-mode
;; (use-package rust-mode
;;   :custom (rust-format-on-save t))
;;
;; https://github.com/brotzeit/rustic
(use-package rustic
  :custom
  (rustic-format-on-save nil)
  (rustic-lsp-format t)
  (rustic-lsp-server 'rust-analyzer)
  (rustic-racer-src-path (my/rust-src-path)))

;; Better Rust/Cargo support for Flycheck
;; https://github.com/flycheck/flycheck-rust
(use-package flycheck-rust
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))


(defun my/lsp-format ()
  "Enable LSP formatting for Rust."
  (when (member major-mode '(rust-mode rustic-mode))
    (lsp-format-buffer)))

; Client/library for the Language Server Protocol
;; https://emacs-lsp.github.io/lsp-mode/
;;
;; Required things
;; $ gem install solargraph
;; $ python3 -m pip install cmake-language-server
;; $ python3 -m pip install python-language-server 'python-language-server[all]' pyls-mypy future pyls-isort pyls-black
;; # pkgin in clang-tools-extra
;;
;; Clojure: https://github.com/clojure-lsp/clojure-lsp looks promising
(use-package lsp-mode
  :custom
  (lsp-auto-guess-root t)
  ;(lsp-enable-snippet nil)
  ;(lsp-idle-delay 1.0)
  (lsp-keep-workspace-alive nil)
  (read-process-output-max (* 2 1024 1024))

  (lsp-pyls-configuration-sources ["flake8" "pycodestyle"])
  (lsp-rust-server 'rust-analyzer)
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-diagnostics-enable-experimental nil)

  :bind ("<f2>" . lsp-rename)

  :hook
  ((c-mode c++-mode rust-mode rustic-mode go-mode python-mode ruby-mode) . lsp)
  (before-save . my/lsp-format))

;; UI integrations for lsp-mode
;; https://emacs-lsp.github.io/lsp-ui/
(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-delay 0.75)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-symbol nil)

  :hook (lsp-mode . lsp-ui-mode))

;; Java LSP integration
;; https://emacs-lsp.github.io/lsp-java/
(use-package lsp-java
  :config (add-hook 'java-mode-hook #'lsp))


;; A tree layout file explorer for Emacs
;; https://github.com/Alexander-Miller/treemacs/
(use-package treemacs
  :defer t
  :custom
  (treemacs-project-follow-cleanup t)
  :bind
  (:map global-map
    ("<f9>" . treemacs-select-window)))

;; Evil mode integration for treemacs
;; https://github.com/Alexander-Miller/treemacs/blob/master/src/extra/treemacs-evil.el
(eval-and-compile (require 'treemacs-interface nil t))
(use-package treemacs-evil
  :config
  (define-key evil-treemacs-state-map (kbd "TAB") #'treemacs-TAB-action))


;; Integration for treemode and lsp-mode
;; https://github.com/emacs-lsp/lsp-treemacs
(use-package lsp-treemacs
  :after (treemacs lsp-mode)
  :config
  (lsp-treemacs-sync-mode 1)
  :bind
  (:map global-map
    ("<f11>" . lsp-treemacs-errors-list)))


;; A modern, on-the-fly syntac checking extension.
;; https://www.flycheck.org/en/latest/
;;
;; flycheck can be used in place of, or in tandem with, emacs-lsp.
;; That said, if there is already an LSP server installed and useable for a
;; certain file type, it may already have flycheck integration and/or you may
;; just not want flycheck checking in addition to the LSP.
;;
;; A list of useful commands to have installed to enable flycheck:
;; - C/C++: http://cppcheck.sourceforge.net/
;; - Dockerfiles: https://github.com/hadolint/hadolint
;; - Markdown: https://github.com/markdownlint/markdownlint/
;; - Prose (text, Markdown): https://github.com/amperser/proselint/
;; - RPM spec files: https://sourceforge.net/projects/rpmlint/
;; - Shell scripts:
;;   - shfmt: https://github.com/mvdan/sh
;;   - https://github.com/koalaman/shellcheck/
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


;; A groovy major mode, grails minor mode, and a groovy inferior mode.
;; https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes
(use-package groovy-mode)


;; Extends the builtin js-mode to add better syntax highlighting for JSON
;; https://github.com/joshwnj/json-mode
(use-package json-mode)


;; Mode for the Go programming language
;; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :mode ("\\.go\\'")
  :config (setq gofmt-command "goimports")
  :hook (before-save . gofmt-before-save))


;; Syntax highlighting for .vimrc/_vimrc files
;; https://github.com/mcandre/vimrc-mode
(use-package vimrc-mode)


;; Highlight TODO keywords
;; https://github.com/tarsius/hl-todo
(use-package hl-todo
  :config (global-hl-todo-mode))


;; Advanced, type aware, highlight support for CMake
;; https://github.com/Lindydancer/cmake-font-lock/
(use-package cmake-font-lock
  :hook (cmake-mode .))


;; PlatformIO integration
;; https://github.com/ZachMassia/PlatformIO-Mode/
(use-package platformio-mode
  :requires projectile
  :hook ((c-mode c++-mode) . platformio-conditionally-enable))


;; Arduino project files are just C++, really
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))


;; Adds LatexMk support to AUCTeX.
;; https://github.com/tom-tan/auctex-latexmk
(use-package auctex-latexmk
  :requires auctex)


;; Only required here so that "reftex-plug-into-AUCTex" isn't seen as a free variable down below
(eval-and-compile (require 'reftex))

;; LaTeX support
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/TeX-Mode.html
(use-package latex
  :ensure nil
  :defer 1
  :requires (auctex auctex-latexmk reftex)

  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)

  :config
  (auctex-latexmk-setup)

  :hook
  (LaTeX-mode . company-auctex-init)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . turn-on-reftex)
  (TeX-mode . (lambda () (setq TeX-command-default "latexmk"))))

(add-hook 'LaTeX-mode-hook
  (lambda ()
    (push
      '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
         :help "Run latexmk on file")
      TeX-command-list)))


;; ParEdit helps keep parentheses balanced.
;; https://www.emacswiki.org/emacs/ParEdit
;; also: http://danmidwood.com/content/2014/11/21/animated-paredit.html
(use-package paredit
  :hook ((clojure-mode lisp-mode emacs-lisp-mode) . paredit-mode))

(use-package evil-paredit
  :hook ((clojure-mode lisp-mode emacs-lisp-mode) . evil-paredit-mode))

;; Highlights delimiters such as parentheses, brackets or braces according to their depth.
;; https://github.com/Fanael/rainbow-delimiters/
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; CIDER is the Clojure(Script) Interactive Development Environment that Rocks
;; https://docs.cider.mx
(use-package cider
  :config (setq cider-repl-display-help-banner nil)
  :requires clojure-mode)


;; Support for the Clojure(Script) programming language
;; https://github.com/clojure-emacs/clojure-mode
(use-package clojure-mode
  :requires rainbox-delimiters)


;; Mode for handling Dockerfiles
;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :mode "Dockerfile.*\\'")


;; Discover key bindings and their meaning for the current Emacs major mode
;; https://github.com/jguenther/discover-my-major
(use-package discover-my-major)


;; Mode for editing Markdown-formatted text
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode)))


;; Major mode for editing YAML files
;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode)


(require 'hl-line)
(set-face-background hl-line-face "#262626")


(use-package monokai-theme)
(load-theme 'monokai t)


;; Put icons in various places to spruce this place up a bit.
;; https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons)

;; doom-modeline is a modeline taken from the Doom Emacs project.
;; https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :after (all-the-icons)
  :custom
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-indent-info t)
  :hook (after-init . doom-modeline-mode))

(defun enable-doom-icons ()
  "Enable icons in the modeline only for graphical frames."
  (setq doom-modeline-icon (display-graphic-p)))

(defun setup-frame-doom (frame)
  "Enable icons in the modeline only if FRAME is a graphical frame."
  (with-selected-frame frame
    (enable-doom-icons)))

(add-hook 'after-make-frame-functions #'setup-frame-doom)


;; A Common Lisp REPL
;; https://github.com/joaotavora/sly
(use-package sly
  :config
  (setq sly-lisp-implementations
    `((sbcl ("/usr/local/bin/sbcl" "--noinform" "--no-linedit") :coding-system utf-8-unix)
       (ccl ,(expand-file-name "~/bin/ccl")))))


;; Quicklisp support for SLY
;; https://github.com/joaotavora/sly-quicklisp
(use-package sly-quicklisp :after sly)


;; ASDF contrib for SLY
;; https://github.com/mmgeorge/sly-asdf
(use-package sly-asdf :after sly)


;; A better Emacs *help* buffer.
;; https://github.com/Wilfred/helpful
(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-c C-d" . helpful-at-point))


(defvar electrify-return-match
  "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an \"electric\" return.")

(defun electrify-return-if-match (arg)
  "TODO ARG."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at electrify-return-match)
      (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(global-set-key (kbd "RET") 'electrify-return-if-match)


;; Make the Emacs GUI frame arguably prettier on macOS
(when (string-equal system-type "darwin")
  (add-to-list 'default-frame-alist'(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist'(ns-appearance . light)))


;; trackpad / mouse wheel scrolls buffer
(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key [mouse-5] 'scroll-up-line)


(defun my/compile-after-save ()
  "Byte-Compiles the Emacs Lisp file in the current buffer, iff a compiled version already exists."
  (add-hook 'after-save-hook
    (lambda ()
      (if (file-exists-p (concat buffer-file-name "c"))
        (emacs-lisp-byte-compile)))

  nil
  t))

(add-hook 'emacs-lisp-mode-hook 'my/compile-after-save)


(provide 'init)
;;; init.el ends here
