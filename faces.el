
;;; package -- fonts and colors

;;; Commentary:
;;; Custom styles

;;; Code:

(use-package gruvbox-theme :ensure t
	:init
	(set-face-attribute 'default nil
											:family "JetBrains Mono"
											:height 120)
	(unless (display-graphic-p)
		(load-theme 'gruvbox-dark-medium t)
		;; Apply gruvbox-dark-soft colors for org code blocks
		(with-eval-after-load 'org
				(set-face-attribute 'org-block nil
														:inherit 'fixed-pitch
														:background "#32302f") ;; dark-soft bg1

				(set-face-attribute 'org-block-begin-line nil
														:foreground "#928374" ;; dark-soft gray
														:background "#3c3836") ;; dark-soft bg1

				(set-face-attribute 'org-block-end-line nil
														:foreground "#928374"
														:background "#3c3836")

				(set-face-attribute 'org-code nil
														:inherit '(shadow fixed-pitch)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;'(default ((t (:weight regular :family "Monaco"))))
 '(line-number ((t (:inherit fixed-pitch :inherit shadow))))
 '(line-number-current-line ((t (:inherit fixed-pitch :inherit shadow :background "gray80"))))
 '(org-document-info ((t (:height 1.2))))
 '(org-document-info-keyword ((t (:inherit shadow :height 1.2))))
 '(org-document-title ((t (:height 1.3 :weight bold))))
 '(org-drawer ((t (:inherit shadow :slant italic))))
 '(org-headline-done ((t (:strike-through t))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-property-value ((t (:inherit shadow :slant italic))))
 '(org-special-keyword ((t (:inherit org-drawer :weight bold))))
 '(region ((t (:inherit highlight :background "gray70"))))
 '(variable-pitch ((t (:weight light :family "Arial" :height 1.2)))))


;;; faces.el ends here
