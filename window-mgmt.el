;; package --- Customizations for window management.

;;; Commentary:
;; Custom minor mode and other changes to suit my window management needs.

;;; Code:

;; Record all window layout changes
;; "C-c <right>" and "C-c <left>" lets you move between these changes.
(when (fboundp 'winner-mode)
  (winner-mode 1))

(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))

;;; window-mgmt.el ends here

