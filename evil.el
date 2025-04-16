;;; evil.el --- All Evil mode related customizations
;;; Commentary:
;;; Evil mode configures Emacs to use vim style keybindings.
;; This helps mitigate carpel tunnel syndrome since you don't have to bend and
;; twist your fingers in odd ways to use Emacs keybindings.

;;; Code:

(use-package evil
  :ensure t
  :init (setq evil-want-integration t
	      evil-want-keybinding nil)
  :config (evil-mode t)
  
  ;; Change evil's redo mapping
  (evil-set-undo-system 'undo-redo)

  (define-key evil-normal-state-map (kbd "R") 'evil-redo)
  ;; Remap 'D' to delete whole line
  ;;(define-key evil-normal-state-map (kbd "D") 'evil-delete-whole-line)

  ;; Remove interpretation of 'j' in insert mode.
  (define-key evil-insert-state-map (kbd "j") '(lambda() (interactive) (insert "j")))

  ;; ---------- Custom commands used with leader key ----------
  (defvar my/leader-map (make-sparse-keymap)
    "My personal leader map.")
  (define-key evil-normal-state-map (kbd ",") my/leader-map)
  (define-key evil-visual-state-map (kbd ",") my/leader-map)
  (define-key evil-motion-state-map (kbd ",") my/leader-map)

  (define-key my/leader-map (kbd "w") evil-window-map)
  ;; Replicate "Cmd + `" (macOS)
  (define-key evil-window-map (kbd "f") #'other-frame)

  ;; Bindings for 'help' specific functions
  (which-key-add-keymap-based-replacements my/leader-map "h" "help")
  (defvar my/leader-help-map (make-sparse-keymap)
    "My help keymap under , h.")
  (define-key my/leader-map (kbd "h") my/leader-help-map)
  (define-key my/leader-help-map (kbd "f") #'counsel-describe-function)
  (define-key my/leader-help-map (kbd "v") #'counsel-describe-variable)

  ;; Bindings for org-mode
  (defvar my/leader-org-map (make-sparse-keymap)
    "My help keymap under , o.")
  (define-key my/leader-map (kbd "o") my/leader-org-map)
  (defun my/org-cycle-at-point ()
    "Call `org-cycle` with point positioned at the beginning of the line."
    (interactive)
    (save-excursion
      (beginning-of-line)
      (org-cycle)))
  (define-key my/leader-org-map (kbd "a") #'my/org-cycle-at-point)
  
  ;; (define-key evil-normal-state-map (kbd "ESC SPC") evil-window-map)
  ;; (define-key evil-visual-state-map (kbd "ESC SPC") evil-window-map)
  ;; (define-key evil-normal-state-map (kbd "SPC .") #'execute-extended-command)
  ;; (define-key evil-normal-state-map (kbd "SPC x") ctl-x-map)
  ;; (define-key evil-normal-state-map (kbd "SPC c") mode-specific-map)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :init
  (evil-collection-init))

(use-package evil-org
  :after evil org
  :ensure t
  :hook ((org-mode . evil-org-mode))
  :init
  (unless (fboundp 'evil-redirect-digit-argument)
    (defalias 'evil-redirect-digit-argument 'ignore))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (defun my/evil-org-log-note-keys ()
    "Define Vim-style keys for log note and capture confirmation."
    (evil-local-set-key 'normal (kbd "SPC c c") #'org-ctrl-c-ctrl-c)
    (evil-local-set-key 'normal (kbd "SPC c k") #'org-ctrl-c-ctrl-k)
    (evil-local-set-key 'normal (kbd "SPC x") ctl-x-map)
    (evil-local-set-key 'motion (kbd "SPC x") ctl-x-map))

  (add-hook 'org-log-buffer-setup-hook #'my/evil-org-log-note-keys)
  (add-hook 'org-capture-mode-hook #'my/evil-org-log-note-keys))
;;; evil.el ends here
