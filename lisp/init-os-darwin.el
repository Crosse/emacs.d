;;; init-os-darwin --- OSX-specific configuration and packages.
;;; Commentary:

;;; Code:

;; Help OSX figure out what my PATH is.
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize)))

(provide 'init-os-darwin)
;;; init-os-darwin.el ends here
