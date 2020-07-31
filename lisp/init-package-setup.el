;;; init-package-setup.el --- Package Management Configuration
;;;
;;; Commentary:
;;;
;;; This file differs from init-packages.el in that this is included quite early in intialization so
;;; that everything else can work.

;;; Code:

;; For packages that aren't in [M]ELPA, copy or git clone them into ~/.emacs.d/lisp.
(let ((default-directory "~/.emacs.d/lisp"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Add MELPA to the package archives
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)

;; Work around a problem with ELPA and TLS 1.3 on Emacs < 27
;; See https://www.reddit.com/r/emacs/comments/cdf48c/failed_to_download_gnu_archive/
(require 'gnutls)
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

(provide 'init-package-setup)
;;; init-package-setup.el ends here
