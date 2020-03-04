;;; init-os-linux --- Linux-specific configuration and packages.
;;; Commentary:

;;; Code:

; For some reason Emacs in a tmux pane doesn't know what these actions mean.
(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key [mouse-5] 'scroll-up-line)

(provide 'init-os-linux)
;;; init-os-linux.el ends here
