;;; package -- My Emacs customizations

;;; Commentary:
;;; This file loads specific Emacs configuration/customization files.

;;; Code:

;; Helper function. Accepts a list of files and loads them using a base path
(defun load-files (base-path files)
  "Load a list of FILES under BASE-PATH.  Print time taken."
  (let ((total-start-time (current-time))
        (total-time 0))
    (mapc (lambda (file)
              (let ((start-time (current-time)))
                (load-file (concat base-path file))
                (let ((elapsed (float-time (time-subtract (current-time) start-time))))
                  (setq total-time (+ total-time elapsed))
                  (message "---- %s took %.3f seconds to load ----" file elapsed))))
            files)
    (message "Total loading time: %.3f seconds" total-time)))

;; The base path from where all customization files should be loaded
(defvar settings-root-path "~/.emacs.d/customize/" "Location of all my Emacs customization files.")

;; This is required in macOS Ventura. Otherwise, there will be errors
;; in loading configuration.
(setq image-types '(svg png gif tiff jpeg xpm xbm pbm))

;; The files list
(defvar files-list '(
		   ;; Basic customizations
		   "secure-pkg-source.el" ;; https for package installation
		   "global-settings.el"
		   "global-keyboard-shortcuts.el"

		   ;; Customzations for all modes
		   "window-mgmt.el"
		   "completion.el"
		   "editing.el"
		   "git.el"
		   "evil.el"
		   "org.el"
		   "faces.el"
		   "ai.el"
			 "go.el"
			 "markdown.el"
			 "lsp.el"
			 "dap.el"
		   )
  "List of files containing my Emacs customizations.")

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
(setq straight-pull-recipe-repositories t)
(straight-use-package 'use-package)

;; Load all customizations
(load-files settings-root-path files-list)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 '(markdown-mode spinner s undo-tree go-projectile go-imenu go-errcheck go-dlv go-complete go-autocomplete go-add-tags go dotenv use-package))
 '(warning-suppress-types '((comp))))

;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
