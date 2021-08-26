;; -*- mode: lisp -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-auto-save t)
 '(TeX-parse-self t)
 '(TeX-save-query nil)
 '(helm-minibuffer-history-key "M-p")
 '(lsp-solargraph-library-directories
   '("~/.rbenv/" "/usr/lib/ruby/" "~/.rvm/" "~/.gem/" "/opt/pkg/lib/ruby"))
 '(package-selected-packages
   '(python-isort lsp-pyright blacken json-mode shfmt evil-paredit jenkinsfile-mode airline-themes all-the-icons auctex auctex-latexmk auto-compile cargo cider clojure-mode cmake-font-lock cmake-mode comment-dwim-2 company company-auctex company-box company-lsp crystal-mode diff-hl discover-my-major dockerfile-mode doom-modeline editorconfig esup evil evil-collection evil-goggles exec-path-from-shell flycheck flycheck-clangcheck flycheck-rust gcmh gnutls go-mode groovy-mode helm helm-company helpful hl-todo latex lsp-java lsp-mode lsp-ui magit markdown-mode minions monokai-theme paredit platformio-mode powerline-evil powershell projectile rainbow-delimiters rustic slime-repl-ansi-color sly sly-asdf sly-quicklisp treemacs treemacs-evil terraform-mode undo-tree use-package vimrc-mode yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight semi-bold :height 120 :width normal :foundry "adobe" :family "SauceCodePro Nerd Font"))))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
