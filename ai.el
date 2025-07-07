;;; ai.el --- Integration of AI/LLMs with Emacs

;;; Commentary:
;; Integrate AI/LLMs into everyday workflows in Emacs.

;;; Code:

;; (use-package shell-maker
;;   :straight (:type git :host github :repo "xenodium/shell-maker" :files ("shell-maker*.el"))
;; 	:defer t)

;; (use-package chatgpt-shell
;;   :straight (:type git :host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell*.el"))
;; 	:defer t
;; 	:init
;; 	(setq chatgpt-shell-anthropic-key (getenv "ANTHROPIC_API_KEY"))
;; ;; (chatgpt-shell-ollama-load-models)
;; 	;;(setq chatgpt-shell-model-version (getenv "AI_MODEL")
;; 	)

(use-package copilot
	:ensure t
	:init
	(add-hook 'prog-mode-hook 'copilot-mode)
	(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
	(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

;;; ai.el ends here
