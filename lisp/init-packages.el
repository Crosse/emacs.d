;;; init-packages.el --- Package Configuration and Installation
;;; Commentary:

;;; Code:

(use-package projectile
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-project-root-files-bottom-up
    '(".projectile" "platformio.ini" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs")))

(projectile-register-project-type 'platformio '("platformio.ini"))

;; Evil-mode, for vi-like emulation and keybindings.
(use-package evil
  :init (setq evil-want-C-u-scroll t) ;; Take C-u back for scrolling a half-page up.
  :config
  (evil-mode 1)
  (global-set-key (kbd "M-u") 'universal-argument)
  (evil-set-initial-state 'cider-repl-mode 'emacs)
  (evil-set-initial-state 'sly-mrepl-mode 'emacs)
  (evil-set-initial-state 'sly-db-mode 'emacs)
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

(use-package platformio-mode
  :requires (projectile)
  :hook ((c-mode c++-mode) . platformio-conditionally-enable))

(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

(use-package auctex-latexmk
  :requires (auctex))

(use-package latex
  :ensure auctex
  :config (auctex-latexmk-setup)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-save-query nil)
  ;(TeX-master nil)
  :hook (latex-mode . company-auctex-init))

(use-package paredit
  :hook ((clojure-mode lisp-mode emacs-lisp-mode) . paredit-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package cider
  :config (setq cider-repl-display-help-banner nil)
  :requires (clojure-mode))

(use-package clojure-mode
  :requires (rainbox-delimiters))

(provide 'init-packages)
;;; init-packages.el ends here
