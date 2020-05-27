;;; init.el -- Init file
;;;
;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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

; Always start the server so that emacsclient can connect.
(require 'server)
(unless (server-running-p)
  (server-start))

; Tell Emacs where "Custom" configuration can go, and then load it.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'init-package-setup)

(cond
  ((string-equal system-type "darwin") (require 'init-os-darwin nil 'noerror))
  ((string-equal system-type "gnu/linux") (require 'init-os-linux nil 'noerror)))

(require 'init-packages)
(require 'init-ui)
(require 'init-sly)
(require 'init-local nil 'noerror)

(cond
  ((executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    (setq ispell-local-dictionary "en_US")
    (setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
  ((executable-find "aspell")
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

(add-hook 'text-mode 'flyspell-mode)
(add-hook 'latex-mode 'flyspell-mode)

; I WANT AUTO-WRAPPING, geez
(auto-fill-mode)

(provide 'init)
;;; init.el ends here
