;;; shell.el --- Shell Specific customizations

;;; Commentary:
;;; Setup shell prompt.  This is similar to setting PS1.

;;; Code:

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize "┌" 'face `(:foreground "thistle4"))
         (propertize "⟦" 'face `(:foreground "thistle4" :weight bold))
         (propertize (user-login-name) 'face `(:foreground "deep sky blue"))
         (propertize " ⭑ " 'face `(:foreground "thistle4"))
         (propertize (system-name) 'face `(:foreground "light slate blue"))
         (propertize " ⭑ " 'face `(:foreground "thistle4"))
         (propertize (format-time-string "%H:%M" (current-time))
                     'face `(:foreground "forest green"))
         (propertize " ⭑ " 'face `(:foreground "thistle4"))
         (propertize (eshell/basename (eshell/pwd))
                     'face `(:foreground "indian red"))
         (if (magit-get-current-branch)
             (propertize (concat " ⎇ " (magit-get-current-branch))
                         'face `(:foreground "dark goldenrod"))
           )
         (propertize "⟧\n" 'face `(:foreground "thistle4" :weight bold))
         (propertize "└⟴" 'face `(:foreground "thistle4"))
         ;;(propertize (if (= (user-uid) 0) " #" " $") 'face `(:foreground "thistle4"))
         (propertize " " 'face `(:foreground "green"))
         )))

;; Without the line below, eshell buffer will become read only after
;; execution of the first command.
(setq eshell-highlight-prompt nil)

;;; shell.el ends here
