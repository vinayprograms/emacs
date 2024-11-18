;; No more accidental minimizing
(global-unset-key "\C-z")

;;-------------------------------------
;; Window management shortcuts
;;-------------------------------------
(global-set-key (kbd "<M-s-left>") 'windmove-left)
(global-set-key (kbd "<M-s-right>") 'windmove-right)
(global-set-key (kbd "<M-s-up>") 'windmove-up)
(global-set-key (kbd "<M-s-down>") 'windmove-down)
(global-set-key (kbd "<S-s-right>") 'next-buffer)
(global-set-key (kbd "<S-s-left>") 'previous-buffer)

;; Change frame width / height
(global-set-key (kbd "s-{") 'shrink-window-horizontally)
(global-set-key (kbd "s-}") 'enlarge-window-horizontally)
(global-set-key (kbd "s-+") 'enlarge-window)
(global-set-key (kbd "s-_") 'shrink-window)

;; Smoothen mouse wheel scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;;-------------------------------------
;; Editing shortcuts
;;-------------------------------------
;; Move between start/end of line
(global-set-key (kbd "<s-left>") 'move-beginning-of-line)
(global-set-key (kbd "<s-right>") 'move-end-of-line)

;; Map "copy" command to "M-w"
(global-set-key (kbd "M-w") 'kill-ring-save)
