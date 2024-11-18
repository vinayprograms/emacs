;; package --- Customizations for window management.

;;; Commentary:
;; Custom minor mode and other changes to suit my window management needs.

;;; Code:

;; Record all window layout changes
;; "C-c <right>" and "C-c <left>" lets you move between these changes.
(when (fboundp 'winner-mode)
  (winner-mode 1))

;;; window-mgmt.el ends here

