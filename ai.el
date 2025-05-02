;;; ai.el --- Integration of AI/LLMs with Emacs

;;; Commentary:
;; Integrate AI/LLMs into everyday workflows in Emacs.

;;; Code:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package shell-maker
  :straight (:type git :host github :repo "xenodium/shell-maker" :files ("shell-maker*.el")))

; This snippet configures the chatgpt-shell package and sets the model version.
(use-package chatgpt-shell
  :straight (:type git :host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell*.el")))
(chatgpt-shell-ollama-load-models)
(setq chatgpt-shell-model-version (getenv "AI_MODEL"))

;;; ai.el ends here
