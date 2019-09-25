;;; init-ui --- Configuration related to the general look-and-feel of Emacs
;;; Commentary:

;;; Code:

(require 'hl-line)
(set-face-background hl-line-face "#262626")

;; Enable line numbers for Emacs >= 26.1
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

(use-package monokai-theme
  :config (load-theme 'monokai))

(use-package all-the-icons)

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

(provide 'init-ui)
;;; init-ui.el ends here
