;;; editing.el --- Editing related customizations
;;;
;;; Commentary:
;;;
;;; 1. Implement spellcheck, code/text complation, highlighting of indent
;;;    guides.
;;; 2. Use syntax checkers and snippets templating to help with coding.
;;; 3. Improvements to keyboard shortcuts including live suggestions.
;;; 4. Functions that handles indentations during yank-pop operations.

;;; Code:

;; Code/text completion support
(use-package company
  :straight t
	:defer t
  :bind ("M-SPC" . company-complete)
  :hook (prog-mode . company-mode))  ; Enable only in prog-mode

;; Simplify keyboard shortcuts
(use-package hydra :straight t :defer t)

;; Jump to visible text
(use-package avy :straight t :defer t)

;; Autocomplete suggestions in minibuffer, load only after startup
(use-package which-key
  :straight t
  :defer t
  :config
  (which-key-mode))

;; Yank and Pop indentation helpers
(defvar yank-indent-modes '(prog-mode org-mode sgml-mode js2-mode)
  "Modes in which to indent regions that are yanked (or yank-popped)")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defun yank-indent-after (orig-fun &rest args)
  "Indent yanked text if current mode is one of 'yank-indent-modes."
  (apply orig-fun args)
  (when (and (not (car args))  ;; `car args` corresponds to `interactive` flag
             (boundp 'yank-indent-modes)
             (cl-some #'derived-mode-p yank-indent-modes))
    (let ((transient-mark-mode nil))
      (yank-advised-indent-function (region-beginning) (region-end)))))

(advice-add 'yank :around #'yank-indent-after)

(defadvice yank-pop (after yank-pop-indent activate)
  "Indent yanked text if current mode is one of 'yank-indent-modes."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(provide 'editing)
;;; editing.el ends here
