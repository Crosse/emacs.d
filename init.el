;; -*- lexical-binding: t; -*-

;;; init.el -- Init file
;;;
;;; Commentary:

;;; Code:


;; taken from daviwil's Emacs config
(add-hook 'emacs-startup-hook
  (lambda ()
    (message "*** Emacs loaded in %s seconds with %d garbage collections."
      (emacs-init-time "%.2f")
      gcs-done)))

;; set a higher GC during startup
(setq gc-cons-threshold (* 50 1000 1000))
;; ...but set it much lower afterwards so that GC pauses aren't as significant.
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 16 1000 1000))))

;; For packages that aren't in [M]ELPA, copy or git clone them into ~/.emacs.d/site-lisp.
(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path)
  (delete-dups load-path))


;; Add MELPA to the package archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Consider built-in packages when installing/upgrading
;; This became a thing in Emacs 29.1
(unless (version< emacs-version "29.1")
  (customize-set-variable 'package-install-upgrade-built-in t))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; If a package "used" below doesn't exist, install it.
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
;; https://github.com/daviwil/dotfiles/blob/fb83c040258391bbb0cb467278bc709cf995d0ac/.emacs.d/modules/dw-core.el#L25-L27
(require 'url-history)
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
  url-history-file (expand-file-name "url/history" user-emacs-directory))

(use-package recentf
  :ensure nil
  :init (recentf-mode))

(use-package no-littering
  :config
  (add-to-list 'recentf-exclude
    (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
    (recentf-expand-file-name no-littering-etc-directory))
  (setq auto-save-file-name-transforms
    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Keep customization settings in a temporary file
;; https://github.com/daviwil/dotfiles/blob/fb83c040258391bbb0cb467278bc709cf995d0ac/.emacs.d/modules/dw-core.el#L33-L38
(setq custom-file
  (if (boundp 'server-socket-dir)
    (expand-file-name "custom.el" server-socket-dir)
    (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; Always start the server so that emacsclient can connect.
;; (do this before setting the custom-file below)
(when (fboundp 'server-running-p)
  (unless (server-running-p)
    (server-start)))


;;; --- Keybindings

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Quicker way to kill a buffer instead of "C-x k <enter>"
(defun my/quick-kill-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-c C-k") #'my/quick-kill-buffer)

;; Mimic Visual Studio
(global-unset-key (kbd "C-<return>"))
(global-set-key (kbd "C-<return>") 'evil-open-above)

;; fix for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=59081
;; can be removed after upgrading to Emacs 29.
(add-to-list 'image-types 'svg)

;;; --- Interface

(column-number-mode t)          ;; Enable column information in the modeline.
(save-place-mode t)             ;; Save our place in each file.
(show-paren-mode t)             ;; Highlight matching braces.
(size-indication-mode t)        ;; Show the size of the buffer in the modeline.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))           ;; Disable the tool bar in the GUI.
(xterm-mouse-mode)              ;; Enable mouse mode in terminals that support it.

(customize-set-variable 'vc-follow-symlinks t)     ;; Always follow symlinks.
(customize-set-variable 'scroll-margin 3)          ;; Make sure there are at least 3 lines above or below the current line on-screen.
(customize-set-variable 'scroll-conservatively 5)  ;; Don't recenter point unless moving more than five lines outside of the frame.
(customize-set-variable 'inhibit-startup-screen t) ;; Don't show the welcome screen
(customize-set-variable 'make-backup-files nil)    ;; stop creating backup~ files
(customize-set-variable 'auto-save-default nil)    ;; stop creating #autosave# files
(customize-set-variable 'garbage-collection-messages t)
(customize-set-variable 'display-line-numbers-width-start t)

(setq-default truncate-lines t)

;; https://github.com/nex3/perspective-el#some-musings-on-emacs-window-layouts
;; ...but customized from there
(customize-set-variable 'display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-at-bottom display-buffer-same-window)
     (reusable-frames . t)))
(customize-set-variable 'even-window-sizes nil)


(fset 'yes-or-no-p 'y-or-n-p)   ;; Use 'y' instead of 'yes', etc.

;; Display line numbers and highlight the current line for many modes
;; Use `hl-line-mode` instead of `global-hl-line-mode`; see
;; https://emacsredux.com/blog/2020/11/21/disable-global-hl-line-mode-for-specific-modes/
(dolist (mode '(text-mode-hook
                 prog-mode-hook
                 conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1)))
  (add-hook mode #'hl-line-mode))


;; Automatically break long lines at the fill-column.
(setq-default auto-fill-function 'do-auto-fill)

(use-package which-func
  :ensure nil
  :custom (which-func-non-auto-modes '(treemacs-mode))
  :config (which-function-mode 1))

(defun my/frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (modify-frame-parameters
      frame '((user-position . t) (top . 0.5) (left . 0.5)))))

(defun my/gui-setup ()
  "Set up things in FRAME that only make sense for graphical displays."
  (when (display-graphic-p)
    ;; (set-face-attribute 'default nil :font "SauceCodePro NF-12")
    ;; also move the frame into the center of the screen and make it larger.
    (require 'cl-lib)
    (let* ((candidates (cl-case system-type
                                (gnu/linux '("BlexMono Nerd Font Mono" "SauceCodePro Nerd Font Mono"))
                                (darwin '("BlexMono NF"))))
           (fonts (cl-remove-if-not #'x-list-fonts candidates))
            (height (cl-case system-type
                      (gnu/linux 100)
                      (darwin 140))))
      (when fonts
        (set-face-attribute 'default nil :font (car fonts) :height height)))
    (set-frame-size (selected-frame) 140 62)
    (my/frame-recenter)))


;; This hook will not run for the initial frame created when starting Emacs.
;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
;(add-hook 'after-make-frame-functions #'my/gui-setup)

;; ...so to get around that, just unconditionally call the function when this file is read.

(if (display-graphic-p)
  (my/gui-setup)
  (tooltip-mode nil))

;; Smooth scrolling...sorta.
(use-package pixel-scroll
  :ensure nil
  :if (display-graphic-p)
  :config
  (setq pixel-resolution-fine-flag t)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (pixel-scroll-mode t))

;; Make Emacs use the $PATH set up by the user's shell
;; https://github.com/purcell/exec-path-from-shell
(eval-and-compile (require 'exec-path-from-shell nil t))
(use-package exec-path-from-shell
  :if (memq system-type '(usg-unix-v darwin gnu/linux))
  ;; :init
  ;; (setq exec-path-from-shell-arguments '("-l"))

  :config
  (dolist (var '("SSH_AUTH_SOCK" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package treesit
  :ensure nil
  :config
  (setq treesit-extra-load-path "/opt/pkg/lib")) ;; for pkgsrc on macOS

;; Project Interaction Library for Emacs
;; https://github.com/bbatsov/projectile
(eval-and-compile (require 'projectile nil t))
(use-package projectile
  :commands projectile-run-vterm
  :config
  (projectile-mode 1)
  (setq
    projectile-require-project-root t
    projectile-dynamic-mode-line nil
    projectile-indexing-method 'hybrid
    projectile-sort-order 'recently-active)
  (setq projectile-globally-ignored-directories
    (append projectile-globally-ignored-directories '("target" "build" ".elixir_ls" "vendor")))
  (setq projectile-project-search-path
    '(("~/code/mine" . 1) ("~/code/mine/lisp . 1") ("~/code/dotfiles" . 0) ("~/code/work" . 1)))

  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-register-project-type 'platformio '("platformio.ini") :project-file "platformio.ini")
  ;; XXX do I need this if I have the line above?
  (add-to-list 'projectile-project-root-files-bottom-up "platformio.ini"))

(with-eval-after-load 'consult
  (require 'projectile)
  (when (fboundp 'consult-ripgrep)
    (define-key projectile-mode-map [remap projectile-ripgrep] 'consult-ripgrep)))


;; https://github.com/nlamirault/ripgrep.el
(use-package ripgrep)
(use-package projectile-ripgrep
  :requires (ripgrep projectile))


;; The extensible vi layer for Emacs.
;; https://github.com/emacs-evil/evil
(use-package evil
  ;; TODO: unbind <TAB> from evil-jump-forward
  :init
  (setq
    evil-default-cursor 'box
    evil-insert-state-cursor 'bar
    evil-move-beyond-eol t
    evil-respect-visual-line-mode t
    evil-undo-system 'undo-tree
    evil-want-C-u-scroll t ; Take C-u back for scrolling a half-page up.
    evil-want-integration t
    evil-want-keybinding nil)
  (setq-default evil-symbol-word-search t)

  :functions evil-set-initial-state

  :config
  (add-hook 'after-init-hook 'evil-normalize-keymaps)
  (global-set-key (kbd "M-u") 'universal-argument)
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'comint-mode 'emacs)
    (evil-set-initial-state 'vterm-mode 'emacs)))

(evil-mode 1)


(defun my/quit-window (&optional kill window)
  "Quit WINDOW and kill its buffer when KILL is nil."
  (interactive "P")
  (with-current-buffer (window-buffer (window-normalize-window window))
      (quit-window (or kill "kill") window)))

(defun my/remap-quit-window (keymap)
  "Remap \"q\" when it is bound to #'quit-window in KEYMAP."
  (when (fboundp 'evil-collection-define-key)
    (evil-collection-define-key 'normal keymap "q" #'my/quit-window)))

;; A set of keybindings for evil-mode
;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :requires evil
  :functions evil-collection-set-readonly-bindings
  :config
  (require 'subr-x)
  (advice-add 'evil-collection-set-readonly-bindings :after #'my/remap-quit-window) ; must be before init!
  (evil-collection-init))

(with-eval-after-load 'evil-collection
  (evil-collection-define-key 'normal 'view-mode-map
    "+" nil
    "=" nil
    "0" nil
    "-" nil))


;; Displays a visual hint on evil edit operations
;; https://github.com/edkolev/evil-goggles
(eval-and-compile (require 'evil-goggles nil t))
(use-package evil-goggles
  :requires evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))


;;; --- Terminals

(use-package term
  :ensure nil
  :config
  (setq explicit-shell-file-name "bash")
  (setq term-prompt-regexp "^[^#$%>\\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :after projectile ; we have :bind and :commands so this is okay
  :bind ("C-`" . projectile-run-vterm)
  :commands vterm
  :config
  ;; (setq term-prompt-regexp "^[^#$%>\\n]*[#$%>] *")
  (push (list "find-file-below"
          (lambda (path)
            (if-let* ((buf (find-file-noselect path))
                       (window (display-buffer-below-selected buf nil)))
              (select-window window)
              (message "Failed to open file: %s" path))))
    vterm-eval-cmds)
  (setq vterm-max-scrollback 10000))


(defun my/enable-undo-p (&optional filename _noerror)
  "Return t if FILENAME resides under ~/.emacs.d/elpa."
  (interactive "GEnter filename: ")
  (let* ((elpa (expand-file-name "~/.emacs.d/elpa/"))
          (fname (if filename filename buffer-file-name))
          (expanded (expand-file-name fname)))
    (or
      (string-prefix-p elpa expanded)
      (not (buffer-name (buffer-base-buffer))))))

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
  (advice-add 'undo-tree-load-history :before-until #'my/enable-undo-p)
  (advice-add 'undo-tree-save-history :before-until #'my/enable-undo-p)
  (global-undo-tree-mode 1))


;; Modular in-buffer completion framework for Emacs
;; https://github.com/company-mode/company-mode
(use-package company
  :config
  (setq company-global-modes '(not comint-mode
                                eshell-mode
                                help-mode
                                message-mode
                                fundamental-mode
                                conf-mode)
    company-idle-delay 0.1
    company-minimum-prefix-length 1
    company-tooltip-align-annotations t)
  :bind (
          :map company-active-map
          ("<return>" . nil)
          ("RET" . nil)
          ("<tab>" . company-complete-selection)
          ("TAB" . company-complete-selection) )
  :hook (after-init . global-company-mode))


;; A company front-end with icons
;; https://github.com/sebastiencs/company-box
(use-package company-box
  :after company ; We :hook so this is okay
  :hook (company-mode . company-box-mode))


;; Editorconfig reads .editorconfig files and configures settings accordingly.
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :config (editorconfig-mode 1))


;; A minor-mode menu for the mode line
;; https://github.com/tarsius/minions
(use-package minions
  :config (minions-mode 1))


;; VERTical Interactive COmpletion
;; https://github.com/minad/vertico
(use-package vertico
  :init
  (vertico-mode)
  (vertico-mouse-mode)
  :custom
  (vertico-cycle t)
  :bind (:map minibuffer-local-map
          ("C-l" . vertico-directory-up))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Emacs completion style that matches multiple regexps in any order
;; https://github.com/oantolin/orderless
(use-package orderless
  :init
  (setq
    completion-category-defaults nil
    completion-ignore-case t)
  :custom
  (completion-styles '(substring orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t))

(use-package savehist
  :ensure nil
  :init (savehist-mode))

(use-package crm
  :ensure nil
  :config
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
            (replace-regexp-in-string
              "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
              crm-separator)
            (car args))
      (cdr args)))
  (advice-add #'completing-read-multiple :filter-args 'crm-indicator))

(use-package emacs
  :ensure nil
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
    '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
    #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Enable rich annotations using the Marginalia package
;; https://github.com/minad/marginalia/
(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
          :map minibuffer-local-map
          ("M-A" . marginalia-cycle))

  :init (marginalia-mode))

(use-package xref
  :ensure nil)

;; Consulting completing-read
;; https://github.com/minad/consult/
;; TODO: check out consult-line{,-multi}, consult-xref, consult-{,rip}grep
(use-package consult
  :requires (xref recentf)
  :demand t ; TODO: this might not be necessary since we :hook?
  :bind (:map minibuffer-local-map ("C-r" . consult-history))
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-project-function (lambda (_) (projectile-project-root)))

  :config
  (define-key global-map [remap switch-to-buffer] 'consult-buffer)
  (define-key global-map [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  (define-key global-map [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (define-key global-map [remap project-switch-to-buffer] 'consult-project-buffer)

  :hook (completion-list-mode . consult-preview-at-point-mode))


;; Emacs Mini-Buffer Actions Rooted in Keymaps
;; https://github.com/oantolin/embark
(use-package embark
  :bind
  (("C-." . embark-act)
    ("C-;" . embark-dwim)
    ("C-h B" . embark-bindings))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
    '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
       nil
       (window-parameters (mode-line-format . none)))))


;; Embark-Consult integration
;; Also in https://github.com/oantolin/embark
(use-package embark-consult
  :requires (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))


;; Displays available keybindings in popup
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :config
  (which-key-mode 1))


;; On-the-fly spell checking
;; https://www.emacswiki.org/emacs/FlySpell
;; Requires either ispell, aspell, or hunspell
;; (Preferring aspell right now)
;; $ pkgin in aspell aspell-en
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


;; A tree layout file explorer for Emacs
;; https://github.com/Alexander-Miller/treemacs/
(use-package treemacs
  :defer t
  :custom
  (treemacs-project-follow-cleanup t)
  (treemacs-project-follow-mode t)
  :config
  (treemacs-resize-icons 16))

(use-package treemacs-projectile
  :after (treemacs projectile))
(use-package treemacs-perspective
  :after (treemacs perspective))
(use-package treemacs-magit
  :after (treemacs magic))

;; Evil mode integration for treemacs
;; https://github.com/Alexander-Miller/treemacs/blob/master/src/extra/treemacs-evil.el
(eval-and-compile (require 'treemacs-interface nil t))
(use-package treemacs-evil
  :config
  :bind (:map evil-treemacs-state-map ("TAB" . #'treemacs-TAB-action)))


(defun my/lsp-treemacs-errors-list-toggle ()
  "Toggle the LSP errors list in treemacs."
  (interactive)
  (let* ((lsp-buffer-name "*LSP Error List*")
          (lsp-buffer (get-buffer-window lsp-buffer-name)))
    (if (eq lsp-buffer (selected-window))
      (kill-buffer lsp-buffer-name)
      (progn
        (lsp-treemacs-errors-list)
        (solaire-mode)))))

;; Integration for treemode and lsp-mode
;; https://github.com/emacs-lsp/lsp-treemacs
(use-package lsp-treemacs
  :after (treemacs lsp-mode)            ; we :bind so this is okay
  :config
  (lsp-treemacs-sync-mode 1)
  :bind
  (:map global-map
    ("<f11>" . my/lsp-treemacs-errors-list-toggle)))


;; LaTeX support
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/TeX-Mode.html
(use-package latex
  :ensure nil
  :requires (auctex reftex)

  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (when (boundp 'reftex-plug-into-AUCTeX)
    (setq reftex-plug-into-AUCTeX t))
  (setq TeX-PDF-mode t)

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

;; Adds LatexMk support to AUCTeX.
;; https://github.com/tom-tan/auctex-latexmk
(use-package auctex-latexmk
  :requires latex
  :config (auctex-latexmk-setup))

;; Perspective
(use-package perspective
  :requires (projectile consult)
  :demand t
  :config
  (setq persp-state-default-file (expand-file-name "perspective.state" user-emacs-directory))
  (when (fboundp 'consult--customize-put) ; shut up, flycheck (consult-customize, below, is a macro)
    (consult-customize consult--source-buffer :hidden t :default nil))
  (add-to-list 'consult-buffer-sources persp-consult-source)
  :custom (persp-mode-prefix-key (kbd "C-c C-p"))
  :hook (kill-emacs . persp-state-save)
  :init
  (persp-mode))

(use-package persp-projectile
  :requires (perspective projectile)
  :bind (:map projectile-mode-map ("C-c p p" . 'projectile-persp-switch-project)))

;; Put icons in various places to spruce this place up a bit.
;; https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons
  :if (display-graphic-p))

;; https://github.com/jtbm37/all-the-icons-dired
(use-package all-the-icons-dired
  :requires all-the-icons
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))


;; The theming came from https://zzamboni.org/post/beautifying-org-mode-in-emacs/
(defun my/org-graphical-setup ()
  "Set up Org Mode fonts and whatnot."
    (let* ((serif-tuple
             (cond
               ((x-list-fonts "Crimson Pro") '(:font "Crimson Pro"))
               ((x-list-fonts "Big Caslon") '(:font "Big Caslon"))
               ((x-list-fonts "Georgia") '(:font "Georgia"))
               ((x-list-fonts "Serif") '(:font "Serif"))
               (nil (warn "Cannot find a serif font. Install something!"))))
            (sans-serif-tuple
              (cond
                ((x-list-fonts "Quattrocento Sans") '(:font "Quattrocento Sans"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande") '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana") '(:font "Verdana"))
                ((x-list-fonts "Sans Serif") '(:font "Sans Serif"))
                (nil (warn "Cannot find a sans-serif font. Install something!"))))
            (base-font-color (face-foreground 'default nil 'default))
            (headline `(:inherit default :weight bold :foreground ,base-font-color)))
      (custom-theme-set-faces
        'user
        `(org-level-8 ((t (,@headline ,@serif-tuple))))
        `(org-level-7 ((t (,@headline ,@serif-tuple))))
        `(org-level-6 ((t (,@headline ,@serif-tuple))))
        `(org-level-5 ((t (,@headline ,@serif-tuple))))
        `(org-level-4 ((t (,@headline ,@serif-tuple :height 1.1))))
        `(org-level-3 ((t (,@headline ,@serif-tuple :height 1.25))))
        `(org-level-2 ((t (,@headline ,@serif-tuple :height 1.4))))
        `(org-level-1 ((t (,@headline ,@serif-tuple :height 1.6))))
        `(org-document-title ((t (,@headline ,@serif-tuple :height 2.0 :underline nil)))))

      (custom-theme-set-faces
        'user
        `(variable-pitch ((t :family ,(plist-get sans-serif-tuple :font) :height 140 :weight thin)))
        `(fixed-pitch ((t :family "BlexMono NF" :height 120 :weight medium)))))

    (custom-theme-set-faces
      'user
      '(org-block ((t (:inherit fixed-pitch))))
      '(org-code ((t (:inherit (shadow fixed-pitch)))))
      '(org-document-info ((t (:foreground "dark orange"))))
      '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
      '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
      '(org-link ((t (:foreground "royal blue" :underline t))))
      '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
      '(org-property-value ((t (:inherit fixed-pitch))) t)
      '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
      '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
      '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
      '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
      '(line-number ((t (:inherit (shadow fixed-pitch)))))
      '(line-number-current-line ((t (:inherit (shadow fixed-pitch)))))))

(use-package org
  :custom
  (org-hide-emphasis-markers t)
  :config
  (font-lock-add-keywords 'org-mode
    '(("^ *\\([-]\\) "
        (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (when (display-graphic-p)
    (my/org-graphical-setup)
    (add-hook 'org-mode-hook #'variable-pitch-mode))

  :hook
  (org-mode . visual-line-mode))

;; Org mode
(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package org-bullets
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold nil)
  (doom-themes-enable-italic nil)
  (doom-themes-treemacs-theme "doom-colors")

  (doom-gruvbox-dark-variant "hard")
  (doom-gruvbox-padded-modeline nil)
  (doom-solarized-light-padded-modeline t)
  (doom-earl-grey-brighter-comments t)

  :config
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(let
  ((frame-background-mode 'light)
    (frame-set-background-mode nil))
  (setq doom-oksolar-light-brighter-comments t)
  (load-theme 'doom-oksolar-light t))

;;(use-package command-log-mode)

;; doom-modeline is a modeline taken from the Doom Emacs project.
;; https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :after all-the-icons ; we :hook so this is okay
  :custom
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-indent-info t)
  (doom-modeline-minor-modes t)
  :hook (after-init . doom-modeline-mode))

(defun setup-frame-doom (frame)
  "Enable icons in the modeline only if FRAME is a graphical frame."
  (with-selected-frame frame
    (setq doom-modeline-icon (display-graphic-p))))

(add-hook 'after-make-frame-functions #'setup-frame-doom)

;; (require 'tab-line)
;; (defun sorted-buffers-list ()
;;   "Return buffer list sorted by name."
;;   (sort (tab-line-tabs-buffer-list)
;;     #'(lambda (first second)
;;         (string<
;;           (buffer-name first)
;;           (buffer-name second)))))

;; (defun my/tab-line-tabs-function ()
;;   "My tabs line function."
;;   (seq-filter
;;     (apply-partially
;;       (lambda (buffer)
;;         (let ((bufname (buffer-name buffer)))
;;           (cond
;;             ((string-equal bufname (buffer-name (current-buffer))) buffer)
;;             ((string-prefix-p "*vterm" bufname) buffer)
;;             ((string-equal "*scratch*" bufname) buffer)
;;             ((string-equal "*Messages*" bufname) buffer)
;;             ((not (or
;;                     (string-search ":" bufname)
;;                     (string-prefix-p "*" bufname))) buffer)))))
;;     (sorted-buffers-list)))

;; (use-package tab-line
;;   :ensure nil
;;   :demand t                          ; required because we :bind, but we don't want to defer loading
;;   :custom
;;   (tab-line-new-button-show nil)
;;   (tab-line-close-button-show t)
;;   :config
;;   (setq tab-line-separator " ")
;;   (setq tab-line-tabs-function #'tab-line-tabs-window-buffers)

;;   :bind
;;   ("s-}" . tab-line-switch-to-next-tab)
;;   ("s-{" . tab-line-switch-to-prev-tab)

;;   :hook
;;   (after-init . global-tab-line-mode))

;; A better Emacs *help* buffer.
;; https://github.com/Wilfred/helpful
(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h K" . helpful-kill-buffers)
  ("C-c C-d" . helpful-at-point))


;; Wakatime - automatic time tracking and metrics generated from your
;; programming activity
(use-package wakatime-mode
  :config
  (setq
    wakatime-cli-path (expand-file-name "~/.wakatime/wakatime-cli")
    wakatime-api-key (auth-source-pick-first-password :host "wakatime.com"))
  :hook (after-init . global-wakatime-mode))

(use-package rainbow-mode
  :hook (after-init . rainbow-mode))

(defun my/solaire-mode-real-buffer-p ()
  "Return t if the current buffer is a real (file-visiting) buffer, or a terminal."
  (or
    (solaire-mode-real-buffer-p)
    (eq major-mode #'vterm-mode)))

;; "If only certain buffers could be so grossly incandescent."
;; https://github.com/hlissner/emacs-solaire-mode
(use-package solaire-mode
  :config
  (setq solaire-mode-real-buffer-fn #'my/solaire-mode-real-buffer-p)
  :init
  (solaire-global-mode +1))

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


;;; --- Development

;; Shell scripting
(eval-and-compile (require 'sh-script nil t))
(add-hook 'sh-mode-hook
  (lambda ()
    (setq-local sh-basic-offset 4)))

(use-package shfmt
  :hook (sh-mode . shfmt-on-save-mode)
  :custom (shfmt-arguments '("-i" "4")))

(require 'cc-vars)
(setq c-default-style
  '((java-mode . "java")
     (awk-mode . "awk")
     (other . "bsd")))

(use-package c-mode
  :ensure nil
  :hook (c-mode . lsp-deferred))

(use-package c++-mode
  :ensure nil
  :hook (c++-mode . lsp-deferred))

;; A replacement for the emacs' built-in command `comment-dwim'.
;; https://github.com/remyferre/comment-dwim-2
(use-package comment-dwim-2
  :config
  (global-set-key (kbd "M-;") 'comment-dwim-2)
  (setq comment-dwim-2--inline-comment-behavior 'reindent-comment))

;; `checkdoc` thinks my Emacs init files should conform to the format of a "regular" Emacs lisp
;; and I'm just tired of it telling me that the files don't start "correctly".
(defun my/disable-checkdoc-in-init-files ()
  "Disable Flycheck's checkdoc checker the current buffer is an Emacs init file."
  (when-let* ((bufname (buffer-file-name (current-buffer)))
               (filename (expand-file-name bufname))
               (dir (file-name-directory filename))
               (ext (file-name-extension filename)))
    (when (and
            (or
              (string-equal dir (expand-file-name "~/.emacs.d/"))
              (string-equal dir (expand-file-name "~/.emacs.d/modules/")))
            (string-equal ext "el"))
      (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))))

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
  :hook (flycheck-mode . my/disable-checkdoc-in-init-files)
  :init (global-flycheck-mode))

;; Client/library for the Language Server Protocol
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
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0.6)
  (lsp-keep-workspace-alive nil)
  (lsp-response-timeout 20)
  (lsp-signature-render-documentation nil)

  (read-process-output-max (* 2 1024 1024))

  (lsp-pyls-configuration-sources ["flake8" "pycodestyle"])

  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-diagnostics-enable-experimental nil)
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-import-enforce-granularity t)
  (lsp-rust-analyzer-import-granularity "module")
  (lsp-rust-analyzer-import-group t)
  (lsp-rust-server 'rust-analyzer)

  :bind
  ("<f2>" . lsp-rename)
  ("<f12>" . lsp-ui-peek-find-definitions)
  ("M-<f12>" . lsp-ui-peek-find-references)
  ("s-<f12>" . lsp-find-definition)

  :commands (lsp lsp-deferred)

  :hook
  (lsp-mode . lsp-enable-which-key-integration))

;; UI integrations for lsp-mode
;; https://emacs-lsp.github.io/lsp-ui/
(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable nil)

  (lsp-ui-peek-always-show t)

  (lsp-ui-sideline-delay 0.75)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-symbol nil)

  :bind ("<f9>" . lsp-ui-doc-toggle)

  :hook (lsp-mode . lsp-ui-mode))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor\\'"))

;; Stops the incessant beeping that is caused by mousing over parts of the window in lsp-enabled buffers
(let ((areas '("nil" "mode-line" "left-margin" "left-fringe" "right-fringe" "header-line"
                "vertical-scroll-bar" "tab-line" "menu-bar"))
       loc)
  (while areas
    (setq loc (pop areas))
    (global-set-key
      (kbd (concat "<" loc "> <mouse-movement>")) #'ignore)))

;; Rust support
;; https://github.com/brotzeit/rustic
(use-package rustic
  :requires lsp-mode
  :config
  (setq rustic-format-on-save nil
    rustic-lsp-server 'rust-analyzer)
  :hook ((rust-mode rustic-mode) . lsp-deferred))

;; Better Rust/Cargo support for Flycheck
;; https://github.com/flycheck/flycheck-rust
(use-package flycheck-rust
  :after (flycheck rustic-mode) ; we :hook so this is okay
  :hook (flycheck-mode . flycheck-rust-setup))

;; Java LSP integration
;; https://emacs-lsp.github.io/lsp-java/
(use-package lsp-java
  :requires lsp-mode
  :hook java . 'lsp)

(use-package flycheck-golangci-lint
  :config
  (add-to-list 'flycheck-checkers 'golangci-lint))

(use-package ruby-mode
  :ensure nil
  :custom (ruby-indent-level 2)
  :hook (ruby-mode . lsp-deferred))

;; (use-package lsp-pyright
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp))))

(use-package python-mode
  :ensure nil
  :hook (python-mode . lsp-deferred))

(use-package python-isort
  :hook (python-mode . python-isort-on-save-mode))

(use-package blacken
  :hook (python-mode . blacken-mode))

(use-package poetry
  :hook (python-mode . poetry-tracking-mode))

;; A groovy major mode, grails minor mode, and a groovy inferior mode.
;; https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes
(use-package groovy-mode
  :hook (groovy-mode . lsp-deferred))

;; Extends the builtin js-mode to add better syntax highlighting for JSON
;; https://github.com/joshwnj/json-mode
(use-package json-mode)

;; Mode for the Go programming language
;; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :mode ("\\.go\\'")
  :config
  (setq gofmt-command "golines")
  ;; (setq gofmt-args '("--tab-len=8" "--reformat-tags" "--max-len=9999"))
  (setq gofmt-args '("--tab-len=8" "--max-len=9999"))
  :hook
  ((go-mode . lsp-deferred)
    (before-save . gofmt-before-save)))

;; Syntax highlighting for .vimrc/_vimrc files
;; https://github.com/mcandre/vimrc-mode
(use-package vimrc-mode)

;; https://melpa.org/#/protobuf-mode
(use-package protobuf-mode)

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

;; ParEdit helps keep parentheses balanced.
;; https://www.emacswiki.org/emacs/ParEdit
;; also: http://danmidwood.com/content/2014/11/21/animated-paredit.html
(use-package paredit
  :hook ((clojure-mode lisp-mode emacs-lisp-mode) . paredit-mode))

;; https://github.com/Fuco1/smartparens
;; TODO: configure this for more modes, maybe: double-quote would be nice to have, for instance
(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook (paredit-mode . smartparens-mode))

;; https://github.com/emacs-evil/evil-cleverparens
(use-package evil-cleverparens
  :requires (smartparens paredit)
  :config (setq evil-cleverparens-complete-parens-in-yanked-region t)
  :hook (paredit-mode . evil-cleverparens-mode))

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
  :hook ((clojure-mode clojurec-mode clojurescript-mode) . lsp-deferred))

;; Mode for handling Dockerfiles
;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :mode "Dockerfile.*\\'")

;; Major mode of Terraform configuration file
;; (requires hcl-mode, so we get that one for free)
;; https://github.com/emacsorphanage/terraform-mode
(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode))

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

;; It's Magit! A Git Porcelain inside Emacs.
;; https://github.com/magit/magit
(use-package magit
  :bind ("C-c g" . magit-file-dispatch))

;; A Common Lisp REPL
;; https://github.com/joaotavora/sly
(use-package sly
  :config
  (setq sly-lisp-implementations
    `((roswell ("ros" "-Q" "run"))
       (sbcl ("/usr/local/bin/sbcl" "--noinform" "--no-linedit") :coding-system utf-8-unix)
       (ccl ,(expand-file-name "~/bin/ccl"))))
  (setq sly-default-lisp 'roswell))

(defvar *roswell-init-file* (expand-file-name "~/.roswell/helper.el"))
(if (file-exists-p *roswell-init-file*)
  (progn
    (load *roswell-init-file*)
    (setq inferior-lisp-program "ros -Q run"))
  (message "Roswell helper not found; is it installed?"))

;; Quicklisp support for SLY
;; https://github.com/joaotavora/sly-quicklisp
(use-package sly-quicklisp
  :requires sly)

;; ASDF contrib for SLY
;; https://github.com/mmgeorge/sly-asdf
(use-package sly-asdf :requires sly)

(use-package web-mode
  :mode
  ("\\.htm[l]?\\'" . web-mode)
  ("\\.css\\'" . web-mode)
  ("\\.ts[x]?\\'" . web-mode)
  :custom
  (web-mode-enable-auto-quoting nil)
  (web-mode-css-indent 2)
  (web-mode-attr-indent 2)
  (web-mode-code-indent 2)
  (web-mode-markup-indent 2)
  (web-mode-attr-value-indent 2)
  (web-mode-markup-comment-indent 2))

(use-package rjsx-mode
  :mode ("\\.js[x]?\\'" . rjsx-mode)
  :custom
  (js2-strict-missing-semi-warning nil)
  :hook (rjsx-mode . lsp-deferred))

;; Emacs port of GitGutter which is Sublime Text Plugin
;; https://github.com/emacsorphanage/git-gutter
;; git-gutter and git-gutter-fringe config taken from https://ianyepan.github.io/posts/emacs-git-gutter/
(use-package git-gutter
  :config
  (setq git-gutter:update-interval 0.1)
  (global-git-gutter-mode +1))

;; Fringe version of git-gutter.el
;; https://github.com/emacsorphanage/git-gutter-fringe
(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; An Emacs mode for editing and running Microsoft PowerShell code.
;; https://github.com/jschaf/powershell.el
(use-package powershell)

;; required dependencies for copilot.el (also 'editorconfig', but we load that up above)
(use-package dash)
(use-package s)

;; jsonrpc is a built-in package, and use-package really doesn't want to upgrade it to the version
;; in ELPA. The hack here is to specify a minimum version requirement, and then force-install if
;; it's not met.
(unless (package-installed-p 'jsonrpc '(1 0 24))
  (package-install 'jsonrpc))
(use-package jsonrpc)

;; An unofficial Copilot plugin for Emacs.
;; https://github.com/copilot-emacs/copilot.el
(use-package copilot
  :if (not (version-list-< (pkg-info-library-version 'jsonrpc) '(1 0 24)))
  :ensure nil
  :requires (editorconfig dash s jsonrpc)
  :config
  (add-to-list 'copilot-indentation-alist '(elisp-interactive-mode lisp-indent-offset))
  :custom
  (copilot-indent-offset-warning-disable t)
  (copilot-idle-delay 2)
  :bind (:map copilot-completion-map
          ("<tab>" . copilot-accept-completion)
          ("TAB" . copilot-accept-completion))
  :hook (prog-mode . copilot-mode))

(provide 'init)
;;; init.el ends here
