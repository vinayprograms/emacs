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
		   "adm.el"
		   )
  "List of files containing my Emacs customizations.")

;; Load all customikzations
(load-files settings-root-path files-list)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(undo-tree go-projectile go-imenu go-errcheck go-dlv go-complete go-autocomplete go-add-tags go dotenv use-package))
 '(warning-suppress-types '((comp))))

;;; init.el ends here
