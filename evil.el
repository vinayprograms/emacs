;;; evil.el --- All Evil mode related customizations
;;; Commentary:
;;; Evil mode configures Emacs to use vim style keybindings.
;; This helps mitigate carpel tunnel syndrome since you don't have to bend and
;; twist your fingers in odd ways to use Emacs keybindings.

;;; Code:

(use-package evil
  :straight t
  :init (setq evil-want-integration t
	      evil-want-keybinding nil)
  :config (evil-mode t)

  ;; Change evil's redo mapping
  (evil-set-undo-system 'undo-redo)

	;;(evil-set-leader 'normal (kbd "SPC"))

  (define-key evil-normal-state-map (kbd "R") 'evil-redo)

  ;; Remove interpretation of 'j' in insert mode.
  (define-key evil-insert-state-map (kbd "j") #'(lambda() (interactive) (insert "j")))

  ;; Use '/' for local swiper search and 'g /' for global swiper search
  (define-key evil-normal-state-map (kbd "/") #'swiper)
  (define-key evil-normal-state-map (kbd "g /") #'swiper-all)

  ;; ---------- Custom commands used with leader key ----------
  (defvar my/leader-map (make-sparse-keymap)
    "My personal leader map.")
  (define-key evil-normal-state-map (kbd ",") my/leader-map)
  (define-key evil-visual-state-map (kbd ",") my/leader-map)
  (define-key evil-motion-state-map (kbd ",") my/leader-map)
	(with-eval-after-load 'org-agenda
		(define-key org-agenda-mode-map (kbd ",") my/leader-map))

  ;; ********** Bindings for 'window' specific functions *********
  (define-key my/leader-map (kbd "w") evil-window-map)
  ;; Replicate "Cmd + `" (macOS)
  (define-key evil-window-map (kbd "f") #'other-frame)
  (define-key evil-window-map (kbd "d") #'delete-window)
  (define-key evil-window-map (kbd "D") #'delete-frame)
  (define-key evil-window-map (kbd "q") #'kill-buffer-and-window)

  ;; ********** Bindings for 'buffer' specific functions *********
  (defvar my/buffer-map (make-sparse-keymap))
  (define-key my/leader-map (kbd "b") my/buffer-map)
  (define-key my/buffer-map (kbd "n") #'next-buffer)
  (define-key my/buffer-map (kbd "p") #'previous-buffer)
  (define-key my/buffer-map (kbd "d") #'kill-buffer)
  (define-key my/buffer-map (kbd "b") #'list-buffers)
  (define-key my/buffer-map (kbd "q") #'kill-buffer-and-window)

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

  ;; Custom evil bindings for org-mode
  (defun my/setup-org-evil-leader ()
    (let ((map (make-sparse-keymap)))
      (define-key my/leader-map (kbd "o") map)
      ;; Org-mode / Org-agenda specific shortcuts.
      (define-key map (kbd "a a") #'org-agenda)
      (define-key map (kbd "f") #'org-agenda-later)
      (define-key map (kbd "b") #'org-agenda-earlier)
      (define-key map (kbd "<tab>") #'my/org-cycle-at-point)
      (define-key map (kbd "a o") #'org-agenda-goto)
      (define-key map (kbd "c c") #'org-ctrl-c-ctrl-c)
      (define-key map (kbd "c k") #'org-kill-note-or-show-branches)
      (define-key map (kbd "t") #'org-todo)
      (define-key map (kbd "a t") #'org-agenda-todo)
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
      (define-key map (kbd "a s") #'org-agenda-schedule)
      (define-key map (kbd "d") #'org-deadline)
      (define-key map (kbd "a d") #'org-agenda-deadline)

      (define-key map (kbd "c i") #'org-clock-in)
      (define-key map (kbd "c o") #'org-clock-out)
      ))
  (add-hook 'org-mode-hook #'my/setup-org-evil-leader)

  ;; ********** Custom evil bindings for AI/LLM **********
  (defvar my/ai-map (make-sparse-keymap))
  (define-key my/ai-map (kbd "q") #'chatgpt-shell-quick-insert)
  (define-key my/ai-map (kbd "c") #'chatgpt-shell-prompt-compose)
  (define-key my/ai-map (kbd "s") #'chatgpt-shell)
  (define-key my/ai-map (kbd "d") #'chatgpt-shell-describe-code)
  (define-key my/ai-map (kbd "g") #'chatgpt-shell-write-git-commit)
  (define-key my/ai-map (kbd "<RET>") #'chatgpt-shell-prompt-compose-send-buffer)
  (define-key my/leader-map (kbd "a") my/ai-map)

  ;; ********** Bindings for ivy minibuffer **********
  (with-eval-after-load 'ivy
    (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-next-line)
    (define-key ivy-minibuffer-map (kbd "C-k") #'ivy-previous-line)
    (define-key ivy-minibuffer-map (kbd "M-j") #'ivy-next-line)
    (define-key ivy-minibuffer-map (kbd "M-k") #'ivy-previous-line))
 
  ;; ********** Other bindings **********
  (define-key my/leader-map (kbd ".") #'execute-extended-command)
  (define-key my/leader-map (kbd "x") ctl-x-map)
  (define-key my/leader-map (kbd "c") mode-specific-map)
  (evil-ex-define-cmd "e" #'counsel-find-file)
  (define-key evil-normal-state-map (kbd "z ;") #'org-cycle)
  )

(use-package evil-collection
  :after evil
  :straight t
	:defer t
  :init
  (evil-collection-init))

(use-package evil-org
  :after evil org
  :straight t
	:defer t
  :hook ((org-mode . evil-org-mode))
  :init
  (unless (fboundp 'evil-redirect-digit-argument)
    (defalias 'evil-redirect-digit-argument 'ignore))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
	(evil-define-key 'motion org-agenda-mode-map (kbd ",") my/leader-map))

;;; evil.el ends here
