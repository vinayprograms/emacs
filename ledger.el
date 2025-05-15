(use-package ledger-mode
  :defer t
  :mode ("\\.ledger\\'" . ledger-mode)
  :config
  ;; Run clean buffer when ledger-mode starts
  (add-hook 'ledger-mode-hook #'ledger-mode-clean-buffer)

  ;; Define wrapper to clean buffer only when major mode is ledger-mode
  (defun my/ledger-clean-in-org-src ()
    (when (derived-mode-p 'ledger-mode)
      (ledger-mode-clean-buffer)))

  ;; Add to org-src editing hooks conditionally
  (dolist (fn '(org-edit-src-save org-edit-src-abort org-edit-src-exit))
    (advice-add fn :before #'my/ledger-clean-in-org-src)))
