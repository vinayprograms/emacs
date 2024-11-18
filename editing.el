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

;; Spellchecking: Load only in text and programming modes
(use-package flyspell
  ;;:ensure t
  :defer t
  :bind ("C-$" . flyspell-correct-word-before-point)
  :hook ((text-mode prog-mode) . flyspell-mode)  ; Only load for relevant modes
  :init
  (unless (getenv "FLYSPELL_PATH_SET")
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
    (setq exec-path (append exec-path '("/usr/local/bin")))
    (setenv "FLYSPELL_PATH_SET" "1"))
  (setq ispell-program-name "ispell"
        ;; ispell-local-dictionary "english"
        ;; ispell-local-dictionary-alist
        ;; '(("english" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))
	))

;; Project management: Enable only in programming modes
(use-package projectile
  :ensure t
  :defer t
  :hook (prog-mode . projectile-mode))

;; Icon package
(use-package all-the-icons
  :ensure t)

;; Code/text completion support
(use-package company
  :ensure t
  :bind ("M-SPC" . company-complete)
  :hook (prog-mode . company-mode))  ; Enable only in prog-mode

;; Highlight indent guides: Activate in programming modes
(use-package highlight-indent-guides
  :ensure t
  :defer t
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character))

;; Set configuration for ibuffer mode without using hooks
(setq ibuffer-formats
      '((mark modified read-only " " (name 30 30 :left :elide))))

;; Syntax checker: Enable only in programming modes
(use-package flycheck
  :ensure t
  :defer t
  :hook (prog-mode . flycheck-mode))  ; Only load in prog-mode

;; Snippet templating and snippet collection
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)) ; Enable only in prog-mode
(use-package yasnippet-snippets :ensure t :defer t)

;; Simplify keyboard shortcuts
(use-package hydra :ensure t :defer t)

;; Jump to visible text
(use-package avy :ensure t :defer t)

;; Autocomplete suggestions in minibuffer, load only after startup
(use-package which-key
  :ensure t
;  :defer 2
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

(defadvice yank (after yank-indent activate)
  "Indent yanked text if current mode is one of 'yank-indent-modes."
  (if (and (not (ad-get-arg 0))
           (--any? (derived-mode-p it) yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "Indent yanked text if current mode is one of 'yank-indent-modes."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(provide 'editing)
;;; editing.el ends here
