;;; markdown.el --- Packages and customization for markdown -*- lexical-binding: t -*-

;;; Commentary:
;;; Packages and configuration for using Emacs as markdown IDE.

;;; Code:

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  ;; (add-hook 'markdown-mode-hook 'variable-pitch-mode)
  (setq markdown-command "markdown")
  (add-hook 'markdown-mode-hook 'variable-pitch-mode))

;;; markdown.el ends here
