(defgroup robot-mode nil
  "Robot Framework major mode"
  :link '(url-link "https://github.com/wingyplus/robot-mode")
  :group 'languages)

(defconst robot-mode--header-keywords-re
  (regexp-opt '("Settings" "Test Cases" "Keywords" "Variables"))
  "Header keywords regexp")

(defconst robot-mode-header-three-star-re "\\*\\{3\\}")

(defconst robot-mode-whitespace-re "[ \t]+")

(defconst robot-mode-header-re
  (concat
    "^"
    robot-mode-header-three-star-re robot-mode-whitespace-re
    robot-mode--header-keywords-re
    robot-mode-whitespace-re robot-mode-header-three-star-re))

(defvar robot-mode-header
  `(,robot-mode-header-re . font-lock-keyword-face)
  "Header keywords")

(defconst robot-mode-settings-keywords-re
  (concat
    "^"
    (regexp-opt '("Library" "Resource" "Variables"
                   "Documentation" "Metadata" "Suite Setup"
                   "Suite Teardown" "Force Tags" "Default Tags"
                   "Test Setup" "Test Teardown" "Test Template"
                   "Test Timeout"))))

(defvar robot-mode-settings-keywords
  `(,robot-mode-settings-keywords-re . font-lock-keyword-face))

(defconst robot-mode-test-case-settings-keywords-re
  (regexp-opt '("Arguments" "Documentation" "Tags" "Setup"
		"Teardown" "Template" "Timeout" "Return"))
  "Test case settings keywords regexp")

(defconst robot-mode-test-case-settings-re
  (concat "\\[" robot-mode-test-case-settings-keywords-re "\\]"))

(defvar robot-mode-test-case-settings
  `(,robot-mode-test-case-settings-re . font-lock-keyword-face)
  "Test case settings keyword")

(defvar robot-mode-comment
  '("^[\s\ta-zA-Z0-9]*\\(#.*\\)$" . (1 font-lock-comment-face))
  (concat
    "Comment"
    "FIXME: it does not tokenizes when Test Case have embeded variable"))

(defvar robot-mode-variable
  '("[\\$&@]{.*?}" . font-lock-variable-name-face))

(defvar robot-mode-font-lock-keywords
  (list
    robot-mode-header
    robot-mode-settings-keywords
    robot-mode-test-case-settings
    robot-mode-comment
    robot-mode-variable)
  "All available keywords")

(define-derived-mode robot-mode fundamental-mode "Robot Framework"
  "A major mode for Robot Framework."
  (setq-local comment-start "# ")
  (setq-local font-lock-defaults
    '(robot-mode-font-lock-keywords)))

(add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode))

(provide 'robot-mode)
;; robot-mode ends here
