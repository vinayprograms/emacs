;; Remove menubar, toolbar and scrollbars from the window.ó
(if (display-graphic-p)
    (progn
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1))
  (menu-bar-mode -1))

(setq-default tab-width 2)

;; Show the file name and major mode in the title bar. This doesn't apply to emacs launched in Terminal.
(setq-default frame-title-format '("%b [%m]"))

;; ask for y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Deleting files moves it to trash instead of removing them outright.
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Don't create a new frame everytime you open a file
(setq ns-pop-up-frames nil)

;; Show column number in statusbar
(setq column-number-mode t)

;; Highlighting matching parentheses across all modes.
(show-paren-mode t)

;; Highlight the current line
(global-hl-line-mode 1)

;; Show relative line number in programming buffers
(setq display-line-numbers-type 'relative)

(defun my-enable-line-numbers ()
  "Enable relative line numbers only for programming modes."
  (when (derived-mode-p 'prog-mode)
    (display-line-numbers-mode 1)))

(add-hook 'after-change-major-mode-hook #'my-enable-line-numbers)

;; enable highlighting colors when a piece of text is selected (for copy/cut)
(transient-mark-mode 1)
;; Enable overwrite / delete highlighted region
(delete-selection-mode t)

;; Smooth scrolling settings
(setq scroll-step 1              ;; Scroll one line at a time
      scroll-conservatively 0    ;; Avoid recentering cursor unnecessarily
      scroll-margin 0            ;; No margin at the top or bottom
      scroll-up-aggressively 0.01 ;; Minimal scroll up
      scroll-down-aggressively 0.01 ;; Minimal scroll down
      next-screen-context-lines 1 ;; Keep one line visible when paging
      auto-window-vscroll nil)   ;; Disable automatic vertical scrolling

(setq org-hide-emphasis-markers t)

;; Don't show emacs welcome page
(setq inhibit-startup-message t)

;; Block bell rings from emacs by replacing it with a dummy function
(defun my-bell-function ())
(setq ring-bell-function 'my-bell-function)
(setq visible-bell t)

;; Word-wrapping
(global-visual-line-mode)

;; Install `use-package` if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Start cursor at the position where it was when file was previously opened
(use-package saveplace
	:straight t
  :init
  (save-place-mode 1)
  :config
  (setq-default save-place t)
  :custom
  (save-place-file (concat user-emacs-directory ".my-saved-places"))
  (save-place-forget-unreadable-files t))

(defun my-load-env-file (file)
  "Load environment variables from FILE, supporting shell-style variable expansion.
Strips quotes from values."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (line (split-string (buffer-string) "\n" t))
        (when (string-match "\\([^=]+\\)=\\(.+\\)" line)
          (let* ((key (match-string 1 line))
                 (value (match-string 2 line))
                 ;; Remove surrounding quotes, if present
                 (stripped-value (replace-regexp-in-string
                                  "^\"\\|\"$" "" (substitute-env-vars value))))
            (setenv key stripped-value)))))))
(my-load-env-file (expand-file-name "~/.emacs.d/.env"))


;; Kill special buffers that don't map to a specific file
;; (setq my-auto-kill-buffer-patterns
;;       '("^\\*scratch\\*$"
;;         "^\\*Messages\\*$"
;;         "^\\*Backtrace\\*$"
;;         "^\\*Help\\*$"
;;         "^\\*Warnings\\*$"
;;         "^\\*Compile-Log\\*$"
;; 				"\\*.*\\*"))

;; (defun my-buffer-should-auto-kill-p (buffer)
;;   "Return non-nil if BUFFER should be auto-killed."
;;   (let ((name (buffer-name buffer)))
;;     (and (cl-some (lambda (pattern)
;;                     (string-match-p pattern name))
;;                   my-auto-kill-buffer-patterns)
;;          (not (eq buffer (current-buffer))) ;; not the one you're using
;;          (not (get-buffer-window buffer 'visible))))) ;; not visible anywhere

;; (defun my-kill-unused-special-buffers ()
;;   "Kill unused special buffers matching user-defined patterns."
;;   (interactive)
;;   (dolist (buffer (buffer-list))
;;     (when (my-buffer-should-auto-kill-p buffer)
;;       (kill-buffer buffer))))

;; ;; Optional: Run it every minute
;; (run-at-time "1 min" 60 #'my-kill-unused-special-buffers)

(provide 'global-settings)
;;; global-settings.el ends here
