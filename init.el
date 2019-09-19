;;; GENERAL CONFIGURATION

(setq custom-file (concat user-emacs-directory "custom"))
(load custom-file)

(column-number-mode t)          ;; Enable column information in the modeline.
(save-place-mode t)             ;; Save our place in each file.
(show-paren-mode t)             ;; Highlight matching braces.
(size-indication-mode t)        ;; Show the size of the buffer in the modeline.
(tool-bar-mode -1)              ;; Disable the tool bar in the GUI.
(global-hl-line-mode t)         ;; Highlight the entire line ("cursorline" in Vim).
(xterm-mouse-mode)              ;; Enable mouse mode in terminals that support it.
(which-function-mode 1)         ;; Display the current function name in the mode line.
(auto-fill-mode t)              ;; Automatically break long lines at the fill-column.
(setq
  vc-follow-symlinks t          ;; Always follow symlinks.
  scroll-margin 3               ;; Make sure there are at least 3 lines above or below the current line on-screen.
  scroll-conservatively 5       ;; Don't recenter point unless moving more than five lines outside of the frame.
  inhibit-startup-screen t      ;; Don't show the welcome screen.
  make-backup-files nil         ;; stop creating backup~ files
  auto-save-default nil         ;; stop creating #autosave# files
  load-prefer-newer t)          ;; Prefer newest version of a file.
(fset 'yes-or-no-p 'y-or-n-p)   ;; Use 'y' instead of 'yes', etc.
(setq c-default-style
  '((java-mode . "java")
     (awk-mode . "awk")
     (other . "bsd")))

(require 'hl-line)
(set-face-background hl-line-face "#262626")

;; Enable line numbers for Emacs >= 26.1
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

(require 'server)
(unless (server-running-p)
  (server-start))

;;; PACKAGES

;; For packages that aren't in [M]ELPA, copy or git clone them into ~/.emacs.d/lisp.
(let ((default-directory "~/.emacs.d/lisp"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Add MELPA to the package archives
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)

;; Work around a problem with ELPA and TLS 1.3 on Emacs < 27
;; See https://www.reddit.com/r/emacs/comments/cdf48c/failed_to_download_gnu_archive/
(when (version<= emacs-version "26.99.0")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(package-initialize)

;; Use the "use-package" package to manage packages.
;; Make sure it's loaded *after* package.el but *before* anything else.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;; If a package "used" below doesn't exist, install it.
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Help OSX figure out what my PATH is.
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Evil-mode, for vi-like emulation and keybindings.
(use-package evil
  :init (setq evil-want-C-u-scroll t) ;; Take C-u back for scrolling a half-page up.
  :config
  (evil-mode 1)
  (global-set-key (kbd "M-u") 'universal-argument)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs))

(defun my/cursor-state-change ()
  "Change the cursor to a bar in insert mode, and a box otherwise."
  (if (string= evil-state "insert")
    (setq cursor-type 'bar)
    (setq cursor-type 'box)))

(add-hook 'evil-insert-state-entry-hook #'my/cursor-state-change)
(add-hook 'evil-insert-state-exit-hook #'my/cursor-state-change)

(use-package evil-numbers
  :requires evil)

;; Company mode is an in-buffer text-completion framework.
(use-package company
  :hook (after-init . global-company-mode))

;; Editorconfig reads .editorconfig files and configures settings accordingly.
(use-package editorconfig
  :config (editorconfig-mode 1))

;; Minions is a minor mode helper.
(use-package minions
  :config (minions-mode 1))


;; Helm is an "incremental completion and selection-narrowing framework"
(use-package helm
    :config
    (helm-mode 1)
    (setq helm-mode-fuzzy-match t)
    (setq helm-completion-in-region-fuzzy-match t))

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

;; Use 'rg' instead of 'ag' in Helm
(setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
(setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))

(use-package diff-hl
  :config (global-diff-hl-mode))

;;(global-set-key (kbd "<f2">) 'flyspell-mode)

(use-package comment-dwim-2             ;; https://github.com/remyferre/comment-dwim-2
  :config (global-set-key (kbd "M-;") 'comment-dwim-2))

(use-package rust-mode                  ;; https://github.com/rust-lang/rust-mode
  :defer t
  :custom (rust-format-on-save t))

(use-package cargo                      ;; https://github.com/kwrooijen/cargo.el
  :requires (rust-mode)
  :hook (rust-mode . cargo-minor-mode))

(use-package lsp-mode
  :defer t
  :config (require 'lsp-clients)
  :custom
  (lsp-enable-snippet nil)
  (lsp-auto-guess-root t)
  :hook ((c-mode c++-mode rust-mode go-mode python-mode) . lsp))

(use-package lsp-ui
  :defer t
  :custom
  (lsp-ui-sideline-delay 1.5)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-symbol nil))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package company-lsp :defer t)

(use-package groovy-mode :defer t)

(use-package go-mode
  :mode ("\\.go\\'")
  :config (setq gofmt-command "goimports")
  :hook (before-save . gofmt-before-save))

(use-package hl-todo
  :config (global-hl-todo-mode))

(use-package cmake-font-lock
  :hook (cmake-mode .))

;;; THEMES AND UI

(use-package monokai-theme
  :config (load-theme 'monokai))

(use-package all-the-icons)

(use-package projectile
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; doom-modeline is a modeline taken from the Doom Emacs project.
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

(defconst sbcl-bin "/usr/local/bin/sbcl")
(defconst ccl-bin "~/bin/ccl")
(use-package slime-company)
(use-package slime
  :requires (slime-company)
  :init
  (setq slime-contribs '(slime-fancy slime-company))
  (when (file-exists-p ccl-bin)
    (setq inferior-lisp-program ccl-bin)))

;; A port of the Vim airline themes to Emacs.
;;(use-package airline-themes
;;  :config (load-theme 'airline-cool))

;; powerline-evil expands Powerline with Evil-mode information
;;(use-package powerline-evil)

;; Powerline is the venerable status line configurator from Vim.
;;(use-package powerline
;;  :after (powerline-evil)
;;  :config (powerline-default-theme))
