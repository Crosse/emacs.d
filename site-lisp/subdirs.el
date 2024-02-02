;;; subdirs.el -- subdirs
;;;; Commentary:

;;; Code:
(normal-top-level-add-to-load-path '("." "copilot.el"))
(normal-top-level-add-subdirs-to-load-path)
(delete-dups load-path)

(provide 'subdirs)
;;; subdirs.el ends here
