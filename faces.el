
;;; package -- fonts and colors

;;; Commentary:
;;; Custom styles

;;; Code:

(set-face-attribute 'default nil
										:family "JetBrains Mono"
										:height 120)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;'(default ((t (:weight regular :family "Monaco"))))
 '(annotate-annotation ((t (:inherit fixed-pitch :background "coral" :foreground "black"))))
 '(annotate-annotation-secondary ((t (:inherit fixed-pitch :background "coral1" :foreground "black" :slant italic))))
 '(annotate-highlight ((t (:underline "coral"))))
 '(annotate-highlight-secondary ((t (:underline "coral1"))))
 '(cursor ((t (:inherit default :background "blue"))))
 '(fixed-pitch ((t (:weight regular :family "Monaco"))))
 '(line-number ((t (:inherit fixed-pitch :inherit shadow))))
 '(line-number-current-line ((t (:inherit fixed-pitch :inherit shadow :background "gray80"))))
 '(org-block ((t (:inherit fixed-pitch :background "#FFFFEA" :extend t))))
 '(org-block-begin-line ((t (:inherit shadow :background "#F0F0FF" :extend t))))
 '(org-block-end-line ((t (:inherit shadow :background "#F0F0FF" :extend t))))
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
