;;; go.el --- Packages to help with go programming

;;; Commentary:

;;; Code:

(use-package go :ensure t :defer t :hook (prog-mode . flycheck-mode))
(use-package go-add-tags :ensure t :defer t :hook (prog-mode . flycheck-mode))
(use-package go-autocomplete :ensure t :defer t :hook (prog-mode . flycheck-mode))
(use-package go-complete :ensure t :defer t :hook (prog-mode . flycheck-mode))
(use-package go-dlv :ensure t :defer t :hook (prog-mode . flycheck-mode))
(use-package go-errcheck :ensure t :defer t :hook (prog-mode . flycheck-mode))
(use-package go-imenu :ensure t :defer t :hook (prog-mode . flycheck-mode))
(use-package go-mode :ensure t :defer t :hook (prog-mode . flycheck-mode))
(use-package go-projectile :ensure t :defer t :hook (prog-mode . flycheck-mode))

;;; go.el ends here

