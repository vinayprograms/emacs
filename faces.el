
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

;;; faces.el ends here
