;;; init-sly.el -- Init file
;;;
;;; Commentary:

;;; Code:

(use-package sly-quicklisp :after sly)
(use-package sly-asdf :after sly)

(use-package sly
  :config
  (setq sly-lisp-implementations
    `((sbcl ("/usr/local/bin/sbcl" "--noinform" "--no-linedit") :coding-system utf-8-unix)
       (ccl ,(expand-file-name "~/bin/ccl")))))

(provide 'init-sly)
;;; init-sly.el ends here
