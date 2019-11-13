;;; init-sly.el -- Init file
;;;
;;; Commentary:

;;; Code:

;(use-package slime-company)
;(use-package slime
;  :requires (slime-company)
;  :init
;  (when (file-exists-p ql-helper)
;    (load ql-helper))
;  :config
;  (setq slime-lisp-implementations
;    `((sbcl ("/usr/local/bin/sbcl"
;              "--userinit" ,(expand-file-name "~/.sbclrc")
;              "--no-linedit"
;              "--dynamic-space-size" "2GB")
;        :coding-system utf-8-unix)
;       (ccl ,(expand-file-name "~/bin/ccl")))
;    slime-net-coding-system 'utf-8-unix
;    slime-export-save-file t
;    slime-contribs '(slime-fancy slime-scratch slime-trace-dialog quicklisp-quickload)
;    lisp-simple-loop-indentation 1
;    lisp-loop-keyword-indentation 6
;    lisp-loop-forms-indentation 6)
;  ;(global-set-key "\C-z" 'slime-selector)
;  (autoload 'paredit-mode "paredit" "Minor mode for structurally editing Lisp code." t)
;  (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
;  (add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
;  (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
;  (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
;  (add-hook 'slime-load-hook (lambda () (require 'slime-fancy)))
;  (add-hook 'enable-paredit-mode (lambda () (paredit-mode +1)))
;  (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
;  (show-paren-mode 1))

(use-package sly-quicklisp
  :after sly)

(use-package sly
  :config
  (setq sly-lisp-implementations
    `((sbcl ("/usr/local/bin/sbcl" "--noinform" "--no-linedit") :coding-system utf-8-unix)
       (ccl ,(expand-file-name "~/bin/ccl")))))

(provide 'init-sly)
;;; init-sly.el ends here
