;; host-config.el -- host-specific configuration
;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'eieio)

(defclass host-config ()
  ((name
     :initarg :name
     :accessor name)
    (system-type
      :initarg :system-type
      :reader system-type)
    (setup
      :initarg :setup
      :initform nil
      :reader setup)
    (ui-font
      :initarg :ui-font
      :initform "monospace"
      :reader ui-font)
    (ui-height
      :initarg :ui-height
      :initform 100
      :reader ui-height)
    (ui-theme
      :initarg :ui-theme
      :initform 'doom-oksolar-light
      :reader ui-theme)))

(cl-defmethod ui-font ((conf host-config))
  "Gets the UI font to use from CONF."
  (cl-remove-if-not #'x-list-fonts (slot-value conf 'ui-font)))

(cl-defmethod setup ((conf host-config))
  "Run the setup for CONF."
  (let ((setup (slot-value conf 'setup)))
    (when setup
      (funcall setup))))

(defclass host-config-linux (host-config)
  ((name
     :initarg :name
     :initform 'linux
     :reader name)
    (system-type
      :initarg :system-type
      :initform "linux"
      :reader system-type)
    (setup
      :initarg :setup
      :initform #'host-config--setup-linux)
    (ui-font
      :initarg :ui-font
      :initform '("BlexMono Nerd Font Mono" "SauceCodePro Nerd Font Mono" "monospace"))
    (ui-height
      :initarg :ui-height
      :iniftorm 100
      :reader ui-height)))

(defclass host-config-darwin (host-config)
  ((name
     :initarg :name
     :initform 'darwin
     :reader name)
    (system-type
      :initarg :system-type
      :initform "darwin"
      :reader system-type)
    (setup
      :initarg :setup
      :initform #'host-config--setup-darwin)
    (ui-font
      :initarg :ui-font
      :initform '("BlexMono NF" "Menlo"))
    (ui-height
      :initarg :ui-height
      :initform 140
      :reader ui-height)))

(defun host-config--setup-linux ()
  "Set up Linux.")

(defun host-config--setup-darwin ()
  "Set up Darwin."
  (with-eval-after-load 'treesit
    (setq treesit-extra-load-path "/opt/pkg/lib")))

(defconst host-configs
  `("mouse" ,(make-instance 'host-config-darwin :name "mouse")
     "nuc" ,(make-instance 'host-config-linux :name "nuc")))

(defun get-host-config ()
  "Get the host-specific config."
  (let ((hostname (car (split-string (system-name) "\\."))))
    (plist-get host-configs hostname #'string-equal)))

(defun get-config (key)
  "Get the host-specific configuration option named KEY."
  (let ((conf (get-host-config)))
    (if conf
      (funcall key conf))))

(provide 'host-config)
;;; host-config.el ends here
