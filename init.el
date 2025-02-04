;; -*- lexical-binding: t; -*-

;;; init.el -- Init file

;;;---------- Startup ----------;;;

(defconst --init-disposition
  (let ((ext (file-name-extension (or  load-file-name ""))))
    (cond
     ((equal ext "el") "interpreted")
     ((equal ext "elc") "byte-compiled")
     ((equal ext "eln") "native-compiled") ;; doesn't seem to happen...
     (t (format "unknown: %s" ext)))))

;; taken from daviwil's Emacs config
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections (%s)."
                     (emacs-init-time "%.2f")
                     gcs-done
                     --init-disposition)))

;; GC threshold is set to a larger value in early-init.el,
;; but we'll set it much lower afterwards so that GC pauses aren't as significant.
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

(eval-and-compile
  (require 'host-config (expand-file-name "~/.emacs.d/init.d/host-config.el") t))


;;;---------- Package Management ----------;;;

;; For packages that aren't in [M]ELPA, copy or git clone them into ~/.emacs.d/site-lisp.
(let ((default-directory (expand-file-name "~/.emacs.d/site-lisp")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path)
  (delete-dups load-path))

;; Add MELPA to the package archives
(require 'package)
(setq package-quickstart t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Consider built-in packages when installing/upgrading
;; This became a thing in Emacs 29.1
(unless (version< emacs-version "29.1")
  (customize-set-variable 'package-install-upgrade-built-in t))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;; (setq use-package-compute-statistics t)

;; If a package "used" below doesn't exist, install it.
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(require 'bind-key)


;;;---------- General Configuration ----------;;;

(use-package auth-source
  :ensure nil
  :config
  (when (string-equal system-type "darwin")
    (setq auth-sources
          '(macos-keychain-generic macos-keychain-internet "~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))))

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
;; https://github.com/daviwil/dotfiles/blob/fb83c040258391bbb0cb467278bc709cf995d0ac/.emacs.d/modules/dw-core.el#L25-L27
(require 'url-history)
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

(use-package recentf
  :ensure nil
  :demand t
  :commands recentf-expand-file-name
  :config (recentf-mode))

(use-package no-littering
  :demand t
  :after recentf
  :config
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))
(eval-when-compile (require 'no-littering))

;; Always start the server so that emacsclient can connect.
(eval-when-compile (require 'server))
(use-package server
  :ensure nil
  :functions server-running-p
  :config
  (unless (server-running-p)
    (server-start)))

;; Keep customization settings in a temporary file
;; https://github.com/daviwil/dotfiles/blob/fb83c040258391bbb0cb467278bc709cf995d0ac/.emacs.d/modules/dw-core.el#L33-L38
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(use-package f
  :functions f-read)

(eval-and-compile
  (defun my/env-from-path (filename)
    "Read environment variables from FILENAME and set them in Emacs\\=' environment."
    (interactive "filename:")
    (let ((contents (f-read filename))
          (trim-chars "[ \\t\\n\\r\"]+"))
      (dolist (line (string-lines contents t))
        (let* ((pos (seq-position line ?=))
               (key (substring line 0 pos))
               (val (string-trim
                     (substring line (1+ pos)) trim-chars trim-chars)))
          (setenv key val)
          (when (string-equal "PATH" key)
            (setq exec-path
                  (append (parse-colon-path val) (list exec-directory)))
            (setq-default eshell-path-env val)))))))

(let ((env-file (expand-file-name "~/.rc/editor_vars")))
  (when (file-exists-p env-file)
    (my/env-from-path env-file)))


;;; --- Keybindings

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Mimic Visual Studio
(global-unset-key (kbd "C-<return>"))
(global-set-key (kbd "C-<return>") 'evil-open-above)

;;;---------- User Interface ----------;;;

(use-package emacs
  :ensure nil
  :config
  (column-number-mode t)   ;; Enable column information in the modeline.
  (save-place-mode t)      ;; Save our place in each file.
  (show-paren-mode t)      ;; Highlight matching braces.
  (size-indication-mode t) ;; Show the size of the buffer in the modeline.
  (xterm-mouse-mode)       ;; Enable mouse mode in terminals that support it.
  (context-menu-mode)
  (setq-default truncate-lines t)
  (fset 'yes-or-no-p 'y-or-n-p)                   ;; Use 'y' instead of 'yes', etc.
  (setq-default auto-fill-function 'do-auto-fill) ;; Automatically break long lines at the fill-column.


  :custom
  (tab-always-indent 'complete)
  (c-tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p)

  (vc-follow-symlinks t) ;; Always follow symlinks.
  (scroll-margin 3) ;; Make sure there are at least 3 lines above or below the current line on-screen.
  (scroll-conservatively 5) ;; Don't recenter point unless moving more than five lines outside of the frame.
  (inhibit-startup-screen t) ;; Don't show the welcome screen
  (make-backup-files nil)    ;; stop creating backup~ files
  (auto-save-default nil)    ;; stop creating #autosave# files
  (garbage-collection-messages t)
  (display-line-numbers-width-start t))


;; Display line numbers and highlight the current line for many modes
;; Use `hl-line-mode` instead of `global-hl-line-mode`; see
;; https://emacsredux.com/blog/2020/11/21/disable-global-hl-line-mode-for-specific-modes/
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1)))
  (add-hook mode #'hl-line-mode))


(use-package ace-window
  :init
  (ace-window-display-mode)
  :bind
  ("C-s-o" . ace-window))

(use-package avy
  :bind
  ("C-s-;" . avy-goto-char-timer))

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
  "Remap \"q\" when it is bound to #\\='quit-window in KEYMAP."
  (when (fboundp 'evil-collection-define-key)
    (evil-collection-define-key 'normal keymap "q" #'my/quit-window)))

;; A set of keybindings for evil-mode
;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :commands
  evil-collection-set-readonly-bindings
  evil-collection-define-key
  :config
  (require 'subr-x)
  (advice-add 'evil-collection-set-readonly-bindings :after #'my/remap-quit-window) ; must be before init!
  (evil-collection-init))

(eval-when-compile (require 'evil-collection nil t))
;; (with-eval-after-load 'evil-collection)
(evil-collection-define-key 'normal 'view-mode-map
  "+" nil
  "=" nil
  "0" nil
  "-" nil)

;; Displays a visual hint on evil edit operations
;; https://github.com/edkolev/evil-goggles
(eval-when-compile (require 'evil-goggles nil t))
(use-package evil-goggles
  :functions evil-goggles-use-diff-faces
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

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
  ;; corfu recommendations
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))

  (completion-styles '(substring orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t))

(with-eval-after-load 'orderless
  (orderless-define-completion-style orderless-literal-only
    (orderless-style-dispatchers nil)
    (orderless-matching-styles '(orderless-literal))))

;; corfu.el - COmpletion in Region FUnction
;; https://github.com/minad/corfu
(use-package corfu
  :after orderless
  :commands corfu-send
  :custom
  (corfu-cycle-t)
  (corfu-auto t)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode)
  :config
  (corfu-echo-mode 1)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)

  (keymap-set corfu-map "RET" `( menu-item "" nil :filter
                                 ,(lambda (&optional _)
                                    (and (derived-mode-p 'eshell-mode 'comint-mode)
                                         #'corfu-send))))
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :hook
  (prog-mode . corfu-mode)
  (shell-mode . corfu-mode)
  (eshell-mode . corfu-mode))

(add-hook 'corfu-mode-hook
          (lambda ()
            (setq-local completion-styles '(orderless-literal-only basic)
                        completion-category-overrides nil
                        completion-category-defaults nil)))

(add-hook 'lsp-completion-mode-hook
          (lambda ()
            (setq-local completion-category-defaults (assoc-delete-all 'lsp-capf completion-category-defaults)
                        completion-styles '(orderless-literal lsp-passthrough))))

(use-package cape
  :after corfu
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-blend-background t)
  (kind-icon-default-face 'corfu-default)

  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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

;; Consulting completing-read
;; https://github.com/minad/consult/
;; TODO: check out consult-line{,-multi}, consult-xref, consult-{,rip}grep
(use-package consult
  :demand t ; TODO: this might not be necessary since we :hook?
  :functions consult-customize consult-ripgrep consult-customize
  :commands consult-preview-at-point-mode
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
  (("s-." . embark-act)
   ("s-;" . embark-dwim)
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
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Displays available keybindings in popup
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :config
  (which-key-mode 1))


;; A tree layout file explorer for Emacs
;; https://github.com/Alexander-Miller/treemacs/
(use-package treemacs
  ;; :defer t
  :commands treemacs treemacs-indent-guide-mode treemacs-TAB-action treemacs-set-scope-type
  :custom
  ;; (treemacs-collapse-dirs 0)
  ;; (treemacs-is-never-other-window nil)
  (treemacs-project-follow-cleanup t)
  (treemacs-project-follow-mode t)
  :config
  ;; (treemacs-resize-icons 44) ; for HiDPI except it doesn't work on my Mac
  (treemacs-indent-guide-mode t)
  (treemacs-git-commit-diff-mode t)
  :bind
  (:map treemacs-mode-map ("<mouse-1>" . treemacs-single-click-expand-action)))

;; Evil mode integration for treemacs
;; https://github.com/Alexander-Miller/treemacs/blob/master/src/extra/treemacs-evil.el
(use-package treemacs-evil
  ;; :defer t
  :after (treemacs evil)
  :config
  :bind
  (:map evil-treemacs-state-map
        ("TAB" . #'treemacs-TAB-action)
        ("<mouse-1>" . treemacs-single-click-expand-action)))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  ;; :defer t
  :after (treemacs magit))

(use-package treemacs-perspective
  ;; :defer t
  :after (treemacs perspective)
  :config
  (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-projectile
  ;; :defer t
  :after (treemacs projectile)
  :commands treemacs-projectile)

;; (defun my/lsp-treemacs-errors-list-toggle ()
;;   "Toggle the LSP errors list in treemacs."
;;   (interactive)
;;   (let* ((lsp-buffer-name "*LSP Error List*")
;;           (lsp-buffer (get-buffer-window lsp-buffer-name)))
;;     (if (eq lsp-buffer (selected-window))
;;       (kill-buffer lsp-buffer-name)
;;       (progn
;;         (lsp-treemacs-errors-list)
;;         (solaire-mode)))))

;; Integration for treemode and lsp-mode
;; https://github.com/emacs-lsp/lsp-treemacs
(use-package lsp-treemacs
  ;; :defer t
  :after (treemacs lsp-mode)            ; we :bind so this is okay
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1)
  ;; :bind
  ;; (:map global-map
  ;;   ("<f11>" . my/lsp-treemacs-errors-list-toggle))
  )

;; Perspective
(use-package perspective
  :config
  (setq persp-state-default-file (expand-file-name "perspective.state" user-emacs-directory))
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
  :custom
  (persp-mode-prefix-key (kbd "C-c C-p"))
  (persp-frame-global-perspective-include-scratch-buffer t)
  (persp-modestring-short t)
  :hook
  (kill-emacs . persp-state-save)
  (after-init . persp-mode))

(with-eval-after-load 'perspective
  (persp-mode))

(use-package persp-projectile
  :bind (:map projectile-mode-map ("C-c p p" . 'projectile-persp-switch-project)))

;; Put icons in various places to spruce this place up a bit.
;; https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons
  :if (display-graphic-p))

;; https://github.com/jtbm37/all-the-icons-dired
(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

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

(eval-when-compile (require 'doom-oksolar-light-theme))
(let
    ((frame-background-mode 'light))
  (frame-set-background-mode nil)
  (setq doom-oksolar-light-brighter-comments t)
  (load-theme (get-config 'ui-theme) t))

;; doom-modeline is a modeline taken from the Doom Emacs project.
;; https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
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

;;(use-package command-log-mode)

;; A better Emacs *help* buffer.
;; https://github.com/Wilfred/helpful
(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h K" . helpful-kill-buffers)
  ("C-c C-d" . helpful-at-point))

;;;---------- Window Layout and Management ----------;;;

(defun my/toggle-window-dedication ()
  "Toggles window dedication in the selected window."
  (interactive)
  (let ((status (window-dedicated-p (selected-window))))
    (if status
        (message "disabling window dedication")
      (message "enabling window dedication"))
    (set-window-dedicated-p (selected-window) (not status))))

(use-package emacs
  :ensure nil

  :config
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))           ;; Disable the tool bar in the GUI.

  :custom
  (switch-to-buffer-obey-display-actions t)
  (switch-to-buffer-in-dedicated-window pop)

  ;; left, top, right, bottom
  (window-sides-slots '(1 1 1 1))

  (display-buffer-alist
   '(((or (major-mode . helpful-mode)
          (major-mode . help-mode)
          (major-mode . Info-mode))
      display-buffer-reuse-mode-window
      (inhibit-same-window . nil))

     `(,(rx (| "*xref*"
               "*grep*"
               "*Occur*"))
       display-buffer-reuse-window
       (inhibit-same-window . nil))

     ((major-mode . flycheck-error-list-mode)
      display-buffer-in-side-window
      (side . bottom)
      (slot . 0)
      (window-parameters . ((no-delete-other-windows . t)))
      (window-height . 0.3))

     ((and (major-mode . treemacs-mode)
           ("Treemacs"))
      display-buffer-in-side-window
      (side . left)
      (slot . 0)
      (window-parameters . ((no-delete-other-windows . t)))
      (window-width . 0.25))

     ((and (major-mode . treemacs-mode)
           ("Errors List"))
      display-buffer-in-side-window
      (side . bottom)
      (slot . 0)
      (window-parameters . ((no-delete-other-windows . t)))
      (window-width . 0.25))

     ;; doesn't work...vterm is wonky.
     ((or (major-mode . vterm-mode)
          (major-mode . vterm-copy-mode))
      display-buffer-in-side-window
      (side . bottom)
      (slot . 0)
      (window-height . 0.3))))

  :bind
  ("C-s-'" . my/toggle-window-dedication))

;; winner-mode
;; Use C-c <left> and C-c <right> to undo/redo window configuration changes
(use-package winner
  :ensure nil
  :init
  (winner-mode +1))

;; https://github.com/nex3/perspective-el#some-musings-on-emacs-window-layouts
;; ...but customized from there
;; (customize-set-variable 'display-buffer-base-action
;;   '((display-buffer-reuse-window display-buffer-in-side-window display-buffer-same-window)
;;      (reusable-frames . t)))
;; (customize-set-variable 'even-window-sizes nil)

(defun my/frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (modify-frame-parameters
     frame '((user-position . t) (top . 0.5) (left . 0.5)))))

(defun my/gui-setup (&optional frame)
  "Set up things in FRAME that only make sense for graphical displays."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      ;; (set-face-attribute 'default nil :font "SauceCodePro NF-12")
      ;; also move the frame into the center of the screen and make it larger.
      ;; (require 'cl-lib)
      (let ((fonts (get-config 'ui-font)))
        (when fonts
          (set-face-attribute 'default nil :font (car fonts) :height (get-config 'ui-height))))
      (set-frame-size (selected-frame) 140 62)
      (my/frame-recenter))))


;; This hook will not run for the initial frame created when starting Emacs.
;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions #'my/gui-setup)
(add-hook 'after-init-hook #'my/gui-setup)

;; Smooth scrolling...sorta.
(use-package pixel-scroll
  :ensure nil
  :if (display-graphic-p)
  :config
  (setq pixel-resolution-fine-flag t)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (pixel-scroll-mode t))

;; Stops the incessant beeping that is caused by mousing over parts of the window in lsp-enabled buffers
(let ((areas '("nil" "mode-line" "left-margin" "left-fringe" "right-fringe" "header-line"
               "vertical-scroll-bar" "tab-line" "menu-bar"))
      loc)
  (while areas
    (setq loc (pop areas))
    (global-set-key
     (kbd (concat "<" loc "> <mouse-movement>")) #'ignore)))


(defun my/solaire-mode-real-buffer-p ()
  "Return t if the current buffer is a real (file-visiting) buffer, or a terminal."
  (or
   (solaire-mode-real-buffer-p)
   (when (fboundp 'vterm-mode)
     (eq major-mode #'vterm-mode))))

;; "If only certain buffers could be so grossly incandescent."
;; https://github.com/hlissner/emacs-solaire-mode
(use-package solaire-mode
  :functions solaire-mode-real-buffer-p
  :config
  (setq solaire-mode-real-buffer-fn #'my/solaire-mode-real-buffer-p)
  :init
  (solaire-global-mode +1))

;; Make the Emacs GUI frame arguably prettier on macOS
(when (string-equal system-type "darwin")
  (add-to-list 'default-frame-alist'(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist'(ns-appearance . light)))


;; trackpad / mouse wheel scrolls buffer
(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key [mouse-5] 'scroll-up-line)

;;;---------- Project Management ----------;;;

(use-package envrc
  :hook (after-init . envrc-global-mode))

;; Project Interaction Library for Emacs
;; https://github.com/bbatsov/projectile
(eval-when-compile (require 'projectile nil t))
(use-package projectile
  :commands projectile-run-vterm
  :functions projectile-register-project-type projectile-project-root
  :config
  (projectile-mode 1)
  (setq
   projectile-require-project-root t
   projectile-dynamic-mode-line nil
   projectile-indexing-method 'alien
   ;; projectile-sort-order 'recently-active
   )
  (setq projectile-globally-ignored-directories
        (append projectile-globally-ignored-directories '("target" "build" ".elixir_ls" "vendor")))
  (setq projectile-project-search-path
        '(("~/code/mine" . 1) ("~/code/mine/lisp . 1") ("~/code/dotfiles" . 0) ("~/code/work" . 1)))

  ;; XXX do I need this if I have the line above?
  (projectile-register-project-type 'platformio '("platformio.ini") :project-file "platformio.ini")
  (add-to-list 'projectile-project-root-files-bottom-up "platformio.ini")

  (define-key projectile-mode-map [remap projectile-ripgrep] 'consult-ripgrep)
  :bind (
         :map projectile-mode-map
         ("s-p" . projectile-command-map)
         ("C-c p" . projectile-command-map)))

;; https://github.com/nlamirault/ripgrep.el
(use-package ripgrep)
(use-package projectile-ripgrep)


;;;---------- Terminals ----------;;;

(use-package term
  :ensure nil
  :commands term-mode
  :config
  (setq explicit-shell-file-name "bash")
  (setq term-prompt-regexp "^[^#$%>\\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :bind ("C-`" . projectile-run-vterm-other-window)
  :functions vterm-mode
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

(use-package dash
  :ensure nil
  :functions -non-nil)

(defun my/undo-allowed-history (&optional _filename _noerror)
  "Return t if we should save and load undo history for the current file."
  (let* ((ignored-paths `("~/.emacs.d/elpa/"
                          ,no-littering-etc-directory
                          ,no-littering-var-directory
                          ,temporary-file-directory
                          "/tmp"
                          "/dev/shm"
                          ,(getenv "TMPDIR")))
         (expanded-paths (seq-map #'expand-file-name (-non-nil ignored-paths))))
    (and
     (not buffer-read-only)
     (stringp buffer-file-name)
     (when epa-file-handler (not (string-match-p (car epa-file-handler) buffer-file-name)))
     (not (seq-some (lambda (elt) (string-prefix-p elt (expand-file-name buffer-file-name))) expanded-paths))
     (not (seq-some (lambda (elt) (string-prefix-p elt (expand-file-name buffer-file-name))) load-path)))))

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
  (advice-add 'undo-tree-load-history :before-while #'my/undo-allowed-history)
  (advice-add 'undo-tree-save-history :before-while #'my/undo-allowed-history)

  :init
  (global-undo-tree-mode 1))

;; A minor-mode menu for the mode line
;; https://github.com/tarsius/minions
(use-package minions
  :config (minions-mode 1))


(use-package savehist
  :ensure nil
  :init (savehist-mode))

(use-package xref
  :ensure nil
  :functions
  xref-show-xrefs-function
  xref-show-definitions-function)

;; On-the-fly spell checking
;; https://www.emacswiki.org/emacs/FlySpell
;; Requires either ispell, aspell, or hunspell
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


;;;---------- Org Mode ----------;;;

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
  :ensure nil ;; use the Org bundled with Emacs
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

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package org-bullets
  :defer t
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))


;;;---------- Development Support ----------;;;

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

;; Highlight TODO keywords
;; https://github.com/tarsius/hl-todo
(use-package hl-todo
  :config (global-hl-todo-mode))

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
  :config (setq evil-cleverparens-complete-parens-in-yanked-region t)
  :hook (paredit-mode . evil-cleverparens-mode))

;; Highlights delimiters such as parentheses, brackets or braces according to their depth.
;; https://github.com/Fanael/rainbow-delimiters/
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; It's Magit! A Git Porcelain inside Emacs.
;; https://github.com/magit/magit
(use-package magit
  :config (add-to-list 'magit-status-headers-hook 'magit-insert-user-header)
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :bind ("C-c g" . magit-file-dispatch))

;; Editorconfig reads .editorconfig files and configures settings accordingly.
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :demand t
  :config (editorconfig-mode 1))

;; Wakatime - automatic time tracking and metrics generated from your
;; programming activity
(use-package wakatime-mode
  :config
  (setq
   wakatime-cli-path (expand-file-name "~/.wakatime/wakatime-cli")
   wakatime-api-key (auth-source-pick-first-password :host "wakatime.com"))
  :hook (after-init . global-wakatime-mode))

;; Print current function in modeline
;; (use-package which-func
;;   :ensure nil
;;   :custom
;;   (which-func-non-auto-modes '(treemacs-mode magit-mode magit-status-mode))
;;   :config (which-function-mode 1))

;; Colorize the names of colors
(use-package rainbow-mode
  :hook (after-init . rainbow-mode))

;; A replacement for the emacs' built-in command `comment-dwim'.
;; https://github.com/remyferre/comment-dwim-2
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2)
  :config
  (setq comment-dwim-2-inline-comment-behavior 'reindent-comment))

(use-package apheleia
  :init
  (apheleia-global-mode +1)

  :config
  (setf (alist-get 'golines apheleia-formatters)
        '("golines"
          "--base-formatter=goimports -local github.com/getlantern,github.com/Crosse" "--tab-len=8" "--reformat-tags" "--max-len=9999"))
  (setf (alist-get 'go-mode apheleia-mode-alist) 'golines)
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'golines))

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

(defun my/toggle-flycheck-list-errors-buffer ()
  "Toggles Flycheck's list of errors."
  (interactive)
  (let* ((flycheck-buffer-name "*Flycheck errors*")
         (flycheck-buffer (get-buffer-window flycheck-buffer-name)))
    (if (eq flycheck-buffer (selected-window))
        (delete-window flycheck-buffer)
      (progn
        (flycheck-list-errors)
        (select-window (get-buffer-window flycheck-buffer-name))))))

;; A modern, on-the-fly syntax checking extension.
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
  :functions flycheck-list-errors
  :hook
  (prog-mode . flycheck-mode)
  (flycheck-mode . my/disable-checkdoc-in-init-files)
  :init (global-flycheck-mode)
  :bind
  (:map global-map
        ("<f8>" . my/toggle-flycheck-list-errors-buffer)))

;; uncomment languages as I test them
(defconst my/treesit-candidates
  '(
    ;; "c"
    ;; "c-sharp"
    ;; "cmake"
    ;; "cpp" ;; actually c++-mode
    ;; "css"
    ;; "dockerfile"
    ;; "elixir"
    "go"
    "go-mod"
    ;; "html"
    ;; "java"
    ;; "javascript"
    ;; "json"
    ;; "lua"
    ;; "markdown"
    ;; "php"
    "python"
    ;; "ruby"
    ;; "rust"
    ;; "toml"
    ;; "tsx"
    ;; "typescript"
    ;; "yaml"
    ))

(use-package treesit
  :ensure nil
  :init
  ;; (add-to-list 'treesit-load-name-override-list '(go-mod "libtree-sitter-go-mod" "tree_sitter_gomod"))
  (add-to-list 'treesit-load-name-override-list '(gomod "libtree-sitter-go-mod" "tree_sitter_gomod"))
  :config
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))))

  (dolist (lang my/treesit-candidates)
    (let ((legacy-mode (intern (format "%s-mode" lang)))
          (ts-mode (intern (format "%s-ts-mode" lang))))
      (when (and (fboundp legacy-mode)
                 (treesit-language-available-p (intern lang)))
        (cl-pushnew `(,legacy-mode . ,ts-mode) major-mode-remap-alist :test 'equal)))))

(with-eval-after-load 'treesit
  (defun my/install-all-treesit-grammars ()
    "Installs all treesitter grammars."
    (interactive)
    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))))

(defconst treesit-grammars
  (cl-loop for lang in my/treesit-candidates
	   collect (let ((legacy-mode (intern (format "%s-mode" lang)))
			 (ts-mode (intern (format "%s-ts-mode" lang))))
		     (when (and (fboundp legacy-mode)
				(treesit-language-available-p (intern lang)))
		       ts-mode))))

(defconst lsp-enabled-modes
  (append treesit-grammars
          '(
            c-mode
            c++-mode
            rust-mode rustic-mode
            ruby-mode
            python-mode
            groovy-mode
            go-mode
            clojure-mode clojurec-mode clojurescript-mode
            dockerfile-mode
            terraform-mode
            rjsx-mode))
  "Modes for which lsp-mode should be enabled")

;; Client/library for the Language Server Protocol
;; https://emacs-lsp.github.io/lsp-mode/
(use-package lsp-mode
  :custom
  (lsp-auto-guess-root t)
  (lsp-copilot-enabled nil)
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0.6)
  (lsp-keep-workspace-alive nil)
  (lsp-response-timeout 20)
  (lsp-signature-render-documentation nil)
  (lsp-completion-provider :none)

  (read-process-output-max (* 2 1024 1024))

  (lsp-go-analyses '((shadow . t) (unusedvariable . t) (useany . t)))

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
  ("s-<f12>" . lsp-find-definition)

  :commands (lsp lsp-deferred)

  :hook
  (lsp-mode . lsp-enable-which-key-integration))

(dolist (mode lsp-enabled-modes)
  (add-hook (intern (format "%s-hook" mode)) #'lsp-deferred))

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

  :bind
  ("<f9>" . lsp-ui-doc-toggle)
  ("<f12>" . lsp-ui-peek-find-definitions)
  ("M-<f12>" . lsp-ui-peek-find-references)

  :hook (lsp-mode . lsp-ui-mode))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor\\'"))

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

;; Free, ultrafast Copilot alternative for Emacs
;; https://github.com/Exafunction/codeium.el
(eval-when-compile (require 'codeium nil t))
(use-package codeium
  :ensure nil ;; not in MELPA; clone repo into ~/.emacs.d/site-lisp
  :commands codeium-init

  :defines
  codeium-mode-line-enable
  codeium-api-enabled
  codeium/document/text
  codeium/document/cursor_offset
  codeium/metadata/api_key

  :functions
  codeium-completion-at-point
  codeium-utf8-byte-length

  :init
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)

  :config
  (setq use-dialog-box nil)
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AutoCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  (defun my-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq codeium/document/text 'my-codeium/document/text)
  (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)
  (setq codeium/metadata/api_key (auth-source-pick-first-password :host "codeium.com"))

  :hook (emacs-startup . (lambda () (run-with-timer 0.1 nil #'codeium-init))))


;;;---------- Language Support ----------;;;


;;---------- C/C++ ----------;;
;; PlatformIO integration
;; https://github.com/ZachMassia/PlatformIO-Mode/
(use-package platformio-mode
  :hook ((c-mode c-ts-mode c++-mode c++-ts-mode) . platformio-conditionally-enable))

;; Arduino project files are just C++, really
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))


;;---------- Clojure ----------;;
;; CIDER is the Clojure(Script) Interactive Development Environment that Rocks
;; https://docs.cider.mx
(use-package cider
  :config (setq cider-repl-display-help-banner nil)
  :hook (clojure-mode . cider-mode))

;; Support for the Clojure(Script) programming language
;; https://github.com/clojure-emacs/clojure-mode
(use-package clojure-mode
  :defer t)


;;---------- CMake ----------;;
;; Advanced, type aware, highlight support for CMake
;; https://github.com/Lindydancer/cmake-font-lock/
(unless (treesit-language-available-p 'cmake) ;; only load if we don't have cmake-ts-mode
  (use-package cmake-font-lock
    :hook (cmake-mode .)))


;;---------- Common Lisp ----------;;
(defvar *roswell-init-file* (expand-file-name "~/.roswell/helper.el"))

;; A Common Lisp REPL
;; https://github.com/joaotavora/sly
(use-package sly
  :commands sly
  :defines sly-contribs
  :config
  (setq sly-lisp-implementations
        `((roswell ("ros" "-Q" "run"))
          (sbcl ("/usr/local/bin/sbcl" "--noinform" "--no-linedit") :coding-system utf-8-unix)
          (ccl ,(expand-file-name "~/bin/ccl"))))
  (setq sly-default-lisp 'roswell)
  (if (file-exists-p *roswell-init-file*)
      (progn
        (load *roswell-init-file*)
        (setq inferior-lisp-program "ros -Q run"))
    (message "Roswell helper not found; is it installed?"))
  :hook (common-lisp-mode . sly-mode))

;; Quicklisp support for SLY
;; https://github.com/joaotavora/sly-quicklisp
(use-package sly-quicklisp
  :defer t
  :config
  (add-to-list 'sly-contribs 'sly-quicklip))

;; ASDF contrib for SLY
;; https://github.com/mmgeorge/sly-asdf
(use-package sly-asdf
  :defer t
  :config
  (add-to-list 'sly-contribs 'sly-asdf))


;;---------- Docker ----------;;
;; Mode for handling Dockerfiles
;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode)


;;---------- Go ----------;;
;; Mode for the Go programming language
;; https://github.com/dominikh/go-mode.el
(use-package go-mode)

(use-package flycheck-golangci-lint
  :config
  (setf (flycheck-checker-get 'golangci-lint 'modes) '(go-mode go-ts-mode))
  :hook ((go-mode go-ts-mode) . flycheck-golangci-lint-setup))


;;---------- Groovy ----------;;
;; A groovy major mode, grails minor mode, and a groovy inferior mode.
;; https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes
(use-package groovy-mode)


;;---------- HCL ----------;;
;; Emacs major mode of HCL(Hashicorp Configuration Language)
;; https://github.com/hcl-emacs/hcl-mode
(use-package hcl-mode)


;;---------- Java ----------;;
;; Java LSP integration
;; https://emacs-lsp.github.io/lsp-java/
(use-package lsp-java
  :hook java . 'lsp)


;;---------- JSON ----------;;
;; Extends the builtin js-mode to add better syntax highlighting for JSON
;; https://github.com/joshwnj/json-mode
(unless (treesit-language-available-p 'json)
  (use-package json-mode))


;;---------- LaTeX ----------;;
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/TeX-Mode.html
(use-package latex
  :ensure nil

  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (when (boundp 'reftex-plug-into-AUCTeX)
    (setq reftex-plug-into-AUCTeX t))
  (setq TeX-PDF-mode t)

  :hook
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
  ;; :config (auctex-latexmk-setup)
  :hook ((LaTeX-mode TeX-mode) . auctex-latexmk-setup))


;;---------- Markdown ----------;;
;; Mode for editing Markdown-formatted text
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))


;;---------- PowerShell ----------;;
;; An Emacs mode for editing and running Microsoft PowerShell code.
;; https://github.com/jschaf/powershell.el
(use-package powershell)


;;---------- Protobuf ----------;;
;; https://melpa.org/#/protobuf-mode
(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))


;;---------- Python ----------;;
(use-package lsp-pyright
  :mode
  ("\\.py\\'" . python-mode)
  :hook ((python-mode python-ts-mode) . (lambda () (require 'lsp-pyright) (lsp))))

(use-package poetry
  :hook ((python-mode python-ts-mode) . poetry-tracking-mode))


;;---------- RJSX ----------;;
(use-package rjsx-mode
  :mode ("\\.js[x]?\\'" . rjsx-mode)
  :custom
  (js2-strict-missing-semi-warning nil))


;;---------- Ruby ----------;;
(use-package ruby-mode
  :ensure nil
  :custom (ruby-indent-level 2))

(use-package ruby-ts-mode
  :ensure nil
  :custom (ruby-indent-level 2))


;;---------- Rust ----------;;
;; https://github.com/emacs-rustic/rustic
;; (use-package rustic
;;   :config
;;   (setq rustic-format-on-save nil
;;     rustic-lsp-server 'rust-analyzer))

;; Better Rust/Cargo support for Flycheck
;; https://github.com/flycheck/flycheck-rust
;; (use-package flycheck-rust
;;   :hook (flycheck-mode . flycheck-rust-setup))


;;---------- Shell scripting ----------;;
(eval-when-compile (require 'sh-script nil t))
(add-hook 'sh-mode-hook
          (lambda ()
            (setq-local sh-basic-offset 4)))

(use-package cc-vars
  :ensure nil
  :custom
  (c-default-style
   '((java-mode . "java")
     (java-ts-mode . "java")
     (awk-mode . "awk")
     (other . "bsd"))))


;;---------- systemd unit files ----------;;
(use-package systemd)


;;---------- Terraform ----------;;
;; Major mode of Terraform configuration file
;; https://github.com/emacsorphanage/terraform-mode
(use-package terraform-mode)
;; :hook (terraform-mode . terraform-format-on-save-mode))


;;---------- YAML ----------;;
;; Major mode for editing YAML files
;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode)


;;---------- VIM ----------;;
;; Syntax highlighting for .vimrc/_vimrc files
;; https://github.com/mcandre/vimrc-mode
(use-package vimrc-mode)


;;---------- Web (HTML/CSS/JS/TS) ----------;;
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

;;; End of Language Support

;; (garbage-collect)
(provide 'init)
