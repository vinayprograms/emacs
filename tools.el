;;; tools.el --- Utility tools and configurations

;;; Commentary:
;; This file contains custom configurations and utilities for file navigation,
;; internet searches, RSS feeds, Vagrant integration, and IP lookup.

;;; Code:

;; File/Folder navigation with Treemacs
(use-package treemacs
  :ensure t
  :defer t
  :config
  (setq image-types '(svg png gif tiff jpeg xpm xbm pbm))
  (treemacs-set-width 80)
  (setq frame-title-format '((:eval (projectile-project-name))))
  (global-prettify-symbols-mode t))

;; Google search for the word under the cursor
(use-package google-this
  :ensure t
  :defer t
  :config
  (google-this-mode)
  :bind ("C-c /" . google-this))

;; RSS Reader with elfeed
(use-package elfeed
  :ensure t
  :defer t)

;; Org integration for elfeed
(use-package elfeed-org
  :ensure t
  :after elfeed
  :config
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org")))

;; Vagrant integration with tramp
(use-package vagrant-tramp
  :ensure t
  :defer t)

;; Function to quickly retrieve the current IP address
(defun what-is-my-ip ()
  "Display the current IP address using an online API."
  (interactive)
  (message "IP: %s"
           (with-current-buffer (url-retrieve-synchronously "https://api.ipify.org")
             (buffer-substring (+ 1 url-http-end-of-headers) (point-max)))))

(provide 'tools)

;;; tools.el ends here
