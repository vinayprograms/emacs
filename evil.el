;;; evil.el --- All Evil mode related customizations
;;; Commentary:
;;; Evil mode configures Emacs to use vim style keybindings.
;;; This helps mitigate carpel tunnel syndrome since you don't have to bend and
;;; twist your fingers in odd ways to use Emacs keybindings.

;;; Code:

;;(use-package undo-tree :ensure t)

(use-package evil
  :ensure t
  :config
  (evil-mode t)
  ;;(setq evil-esc-delay 0.001)

  ;; Change evil's redo mapping
  ;;(global-undo-tree-mode)
  ;;(evil-set-undo-system 'undo-tree)
  ;; fix some of the keyboard shortcuts to match vim
  (define-key evil-normal-state-map (kbd "ESC :") 'evil-ex)
  (define-key evil-normal-state-map (kbd "u") 'evil-undo)
  ;; Replace single character
  (define-key evil-normal-state-map (kbd "r") 'evil-replace)
  (define-key evil-visual-state-map (kbd "r") 'evil-replace)

  (define-key evil-normal-state-map (kbd "C-r") 'evil-redo)
  ;; Remap 'S' to save the current file
  (define-key evil-normal-state-map (kbd "S") 'save-buffer)
  ;; Remap 'D' to delete whole line
  (define-key evil-normal-state-map (kbd "D") 'evil-delete-whole-line)
  ;; Remove interpretation of 'j' in insert mode.
  (define-key evil-insert-state-map (kbd "j") '(lambda() (interactive) (insert "j")))

  ;; -- Mode line customizations --
  ;; Change color of mode line based on evil mode
  (defun my-update-mode-line-color ()
    "Update mode line color based on Evil state."
    (progn
      (cond
       ((evil-insert-state-p) (set-face-background 'mode-line "red" (selected-frame) ))
       ((evil-visual-state-p) (set-face-background 'mode-line "blue" (selected-frame) ))
       ((evil-replace-state-p) (set-face-background 'mode-line "green" (selected-frame) ))
       (t (set-face-background 'mode-line "gray20" (selected-frame))))
      ;; (cond
      ;;  ((evil-insert-state-p) (send-string-to-terminal "\e[5 q"))
      ;;  ((evil-visual-state-p) (send-string-to-terminal "\e[6 q"))
      ;;  ((evil-replace-state-p) (send-string-to-terminal "\e[4 q"))
      ;;  (t (send-string-to-terminal "\e[1 q")))
       ))  ;; Default color for normal mode
  
  ;; Add hooks to change the mode line color when entering each state
  (add-hook 'evil-insert-state-entry-hook #'my-update-mode-line-color)
  (add-hook 'evil-visual-state-entry-hook #'my-update-mode-line-color)
  (add-hook 'evil-normal-state-entry-hook #'my-update-mode-line-color)
  (add-hook 'evil-motion-state-entry-hook #'my-update-mode-line-color)
  (add-hook 'evil-replace-state-entry-hook #'my-update-mode-line-color)
  (add-hook 'evil-emacs-state-entry-hook #'my-update-mode-line-color)

  ;; Initialize mode line color when emacs starts
  (add-hook 'after-init-hook #'my-update-mode-line-color)
)

;;; evil.el ends here
