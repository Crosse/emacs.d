;; -*- lexical-binding: t; -*-

(setq load-prefer-newer t) ; Prefer newest version of a file, whether it be compiled or source.

(when (featurep 'native-compile)
  ;; Silence compiler warnings.
  (customize-set-value 'native-comp-async-report-warnings-errors nil)
  (customize-set-value 'package-native-compile t)

  ;; Put the Emacs native compilation cache in no-littering's `var` directory.
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
      (convert-standard-filename
        (expand-file-name  "var/eln-cache/" user-emacs-directory)))))
