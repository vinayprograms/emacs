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

  (define-key evil-normal-state-map (kbd ";") #'evil-ex)
  (define-key evil-visual-state-map (kbd ";") #'evil-ex)
  (define-key evil-motion-state-map (kbd ";") #'evil-ex)

  ;; ---------- Custom commands used with leader key ----------
  (defvar my/leader-map (make-sparse-keymap)
    "My personal leader map.")
  (define-key evil-normal-state-map (kbd ",") my/leader-map)
  (define-key evil-visual-state-map (kbd ",") my/leader-map)
  (define-key evil-motion-state-map (kbd ",") my/leader-map)

  (define-key my/leader-map (kbd "w") evil-window-map)
  ;; Replicate "Cmd + `" (macOS)
  (define-key evil-window-map (kbd "f") #'other-frame)
  (define-key evil-window-map (kbd "d") #'delete-window)
  (define-key evil-window-map (kbd "D") #'delete-frame)

  ;; ********** Bindings for 'help' specific functions *********
  (which-key-add-keymap-based-replacements my/leader-map "h" "help")
  (defvar my/leader-help-map (make-sparse-keymap)
    "My help keymap under , h")
  (define-key my/leader-map (kbd "h") my/leader-help-map)
  (define-key my/leader-help-map (kbd "f") #'counsel-describe-function)
  (define-key my/leader-help-map (kbd "v") #'counsel-describe-variable)

  ;; ********** Bindings for org-mode **********
  ;; (triggered only when org-mode is active)
  (defun my/org-insert-todo-heading-after ()
    "Insert a new TODO heading below the current line."
    (interactive)
    (end-of-line)
    (org-insert-todo-heading nil)  ; nil means insert after
    (evil-insert-state))

  (defun my/org-insert-todo-heading-before ()
    "Insert a new TODO heading above the current line."
    (interactive)
    (beginning-of-line)
    (org-insert-todo-heading t)    ; t means insert before
    (evil-insert-state))

  (defun my/org-insert-heading-after ()
    "Insert a new heading below the current line."
    (interactive)
    (end-of-line)
    (org-insert-heading nil)
    (evil-insert-state))

  (defun my/org-insert-heading-before ()
    "Insert a new heading above the current line."
    (interactive)
    (beginning-of-line)
    (org-insert-heading t)
    (evil-insert-state))

  (defun my/setup-org-evil-leader ()
    (let ((map (make-sparse-keymap)))
      (define-key my/leader-map (kbd "o") map)
      (define-key map (kbd "a") #'org-agenda)
      (define-key map (kbd "f") #'org-agenda-later)
      (define-key map (kbd "b") #'org-agenda-earlier)
      (define-key map (kbd "<tab>") #'my/org-cycle-at-point)
      (define-key map (kbd "c c") #'org-ctrl-c-ctrl-c)
      (define-key map (kbd "c k") #'org-kill-note-or-show-branches)
      (define-key map (kbd "t") #'org-todo)
      ;; 'o' style (after)
      (define-key map (kbd "o t") #'my/org-insert-todo-heading-after)
      (define-key map (kbd "o h") #'my/org-insert-heading-after)
      (define-key map (kbd "o i") #'org-insert-item)
      ;; 'O' style (before)
      (define-key map (kbd "O t") #'my/org-insert-todo-heading-before)
      (define-key map (kbd "O h") #'my/org-insert-heading-before)
      ;; Emacs-style (in-place)
      (define-key map (kbd "I t") #'org-insert-todo-heading)
      (define-key map (kbd "i t") #'org-insert-todo-heading-respect-content)
      (define-key map (kbd "I h") #'org-insert-heading)
      (define-key map (kbd "i h") #'org-insert-heading-respect-content)
 
      (define-key map (kbd "s") #'org-schedule)
      (define-key map (kbd "d") #'org-deadline)

      (define-key map (kbd "c i") #'org-clock-in)
      (define-key map (kbd "c o") #'org-clock-out)
      ))
  (add-hook 'org-mode-hook #'my/setup-org-evil-leader)

  ;; ********** Bindings for ivy minibuffer **********
  (with-eval-after-load 'ivy
    (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-next-line)
    (define-key ivy-minibuffer-map (kbd "C-k") #'ivy-previous-line)
    (define-key ivy-minibuffer-map (kbd "M-j") #'ivy-next-line)
    (define-key ivy-minibuffer-map (kbd "M-k") #'ivy-previous-line))
 
  ;; ********** Other bindings **********
  (define-key my/leader-map (kbd ".") #'execute-extended-command)
  ;; (define-key evil-normal-state-map (kbd "ESC SPC") evil-window-map)
  ;; (define-key evil-visual-state-map (kbd "ESC SPC") evil-window-map)
  ;; (define-key evil-normal-state-map (kbd "SPC .") #'execute-extended-command)
  (define-key my/leader-map (kbd "x") ctl-x-map)
  (define-key my/leader-map (kbd "c") mode-specific-map)
  (evil-ex-define-cmd "e" #'counsel-find-file)

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
