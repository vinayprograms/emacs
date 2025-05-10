;;; dap-support --- DAP (Debug Adapter Support) for programming
;;; Commentary:
;;  DAP mode packages and configuration for supported languages

;; Global DAP package
(use-package dap-mode
  :ensure t
	:defer t
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode 1)
  (global-set-key (kbd "<f6>") 'dap-debug)
  (global-set-key (kbd "<f10>") 'dap-next)
  (global-set-key (kbd "<f11>") 'dap-step-in)
  (global-set-key (kbd "<f12>") 'dap-step-out)
  (global-set-key (kbd "<f9>") 'dap-breakpoint-add)
  (global-set-key (kbd "<f8>") 'dap-breakpoint-delete)

  ;; Language specific configurations
	(require 'dap-dlv-go))
