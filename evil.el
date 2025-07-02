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

	;; (evil-set-leader 'normal (kbd "SPC"))

  (define-key evil-normal-state-map (kbd "R") 'evil-redo)

  ;; Remove interpretation of 'j' in insert mode.
  (define-key evil-insert-state-map (kbd "j") #'(lambda() (interactive) (insert "j")))
	;; Make '$' go to actual end of line instead of end of soft-wrapped line
	(define-key evil-normal-state-map (kbd "$") 'end-of-line)
	(define-key evil-visual-state-map (kbd "$") 'end-of-line)

  ;; ********** Special functions for use with evil maps **********
  ;; (triggered only when org-mode is active)
  (defun my/org-insert-todo-heading-after ()
    "Insert a new TODO heading below the current line."
    (interactive)
    (org-insert-todo-heading-respect-content nil)  ; nil means insert after
    (evil-insert-state))

  (defun my/org-insert-todo-heading-before ()
    "Insert a new TODO heading above the current line."
    (interactive)
		(beginning-of-visual-line)
		(org-insert-todo-heading t)    ; t means insert before
    (evil-insert-state))

  (defun my/org-insert-heading-after ()
    "Insert a new heading below the current line."
    (interactive)
    (org-insert-heading-respect-content nil)
    (evil-insert-state))

  (defun my/org-insert-heading-before ()
    "Insert a new heading above the current line."
    (interactive)
		(beginning-of-visual-line)
    (org-insert-heading t)
    (evil-insert-state))

  (defun my/org-insert-todo-subheading-after ()
    "Insert a new TODO heading below the current line."
    (interactive)
    (org-insert-todo-heading-respect-content nil)  ; nil means insert after
		(org-demote-subtree)
    (evil-insert-state))

  (defun my/org-insert-subheading-after ()
    "Insert a new heading below the current line."
    (interactive)
    (org-insert-heading-respect-content nil)  ; nil means insert after
		(org-demote-subtree)
    (evil-insert-state))

	(defun my/org-insert-bullet-after ()
    "Insert a new heading below the current line."
    (interactive)
		(org-end-of-line)
    (org-insert-item nil)
    (evil-insert-state))

	(defun my/org-insert-checkbox-after ()
    "Insert a new heading below the current line."
    (interactive)
		(org-end-of-line)
    (org-insert-item t)
    (evil-insert-state))

	(defun my/org-insert-subbullet-after ()
    "Insert a new heading below the current line."
    (interactive)
		(org-end-of-line)
    (org-insert-item nil)
		(org-indent-item)
    (evil-insert-state))

	(defun my/org-insert-subcheckbox-after ()
    "Insert a new heading below the current line."
    (interactive)
		(org-end-of-line)
    (org-insert-item t)
		(org-indent-item)
    (evil-insert-state))
	
	(defun my/surround-text ()
		"Surround region or word at point with a typed character, no prompt."
		(interactive)
		(let ((char (read-char))) ;; Wait for a single keypress without prompt
			(let* ((beg (if (use-region-p)
											(region-beginning)
										(car (bounds-of-thing-at-point 'word))))
						 (end (if (use-region-p)
											(region-end)
										(cdr (bounds-of-thing-at-point 'word)))))
				(save-excursion
					(goto-char end)
					(insert char)
					(goto-char beg)
					(insert char)))))
	;; ---------- Customizations (non-leader) ----------
  (define-key evil-normal-state-map (kbd "g h") #'evil-beginning-of-visual-line)
  (define-key evil-normal-state-map (kbd "g l") #'evil-end-of-visual-line)
  (define-key evil-normal-state-map (kbd "H") #'evil-first-non-blank)
  (define-key evil-normal-state-map (kbd "L") #'evil-end-of-line)
  (define-key evil-visual-state-map (kbd "g h") #'evil-beginning-of-visual-line)
  (define-key evil-visual-state-map (kbd "g l") #'evil-end-of-visual-line)
  (define-key evil-visual-state-map (kbd "H") #'evil-first-non-blank)
  (define-key evil-visual-state-map (kbd "L") #'evil-end-of-line)
  (define-key evil-motion-state-map (kbd "H") #'evil-first-non-blank)
  (define-key evil-motion-state-map (kbd "L") #'evil-end-of-line)
  ;; ---------- Custom commands used with leader key ----------
  (defvar my/leader-map (make-sparse-keymap)
    "My personal leader map.")
  (define-key evil-normal-state-map (kbd "SPC") my/leader-map)
  (define-key evil-visual-state-map (kbd "SPC") my/leader-map)
  (define-key evil-motion-state-map (kbd "SPC") my/leader-map)
	(with-eval-after-load 'org-agenda
		(define-key org-agenda-mode-map (kbd "SPC") my/leader-map))

  ;; Use '/' for local swiper search and '<leader> /' for global swiper search
  (define-key evil-normal-state-map (kbd "/") #'swiper)
  (define-key my/leader-map (kbd "/") #'swiper-all)

  ;; ********** 'o' style (after) *********
	(define-key my/leader-map (kbd "o t") #'my/org-insert-todo-heading-after)
	(define-key my/leader-map (kbd "o h") #'my/org-insert-heading-after)
	(define-key my/leader-map (kbd "o b") #'my/org-insert-bullet-after)
	(define-key my/leader-map (kbd "o c") #'my/org-insert-checkbox-after)
	(define-key my/leader-map (kbd "o i t") #'my/org-insert-todo-subheading-after)
	(define-key my/leader-map (kbd "o i h") #'my/org-insert-subheading-after)
	(define-key my/leader-map (kbd "o i b") #'my/org-insert-subbullet-after)
	(define-key my/leader-map (kbd "o i c") #'my/org-insert-subcheckbox-after)
  ;; ********** 'O' style (before) *********
	(define-key my/leader-map (kbd "O t") #'my/org-insert-todo-heading-before)
	(define-key my/leader-map (kbd "O h") #'my/org-insert-heading-before)

	;; ********** search *********
  (define-key my/leader-map (kbd "/") #'swiper-all)

  ;; ********** insert / add *********
  (define-key my/leader-map (kbd "i t b") #'org-insert-structure-template)
  (define-key my/leader-map (kbd "i t c") #'org-capture)
  (define-key my/leader-map (kbd "i c") #'org-clock-in)
  (define-key my/leader-map (kbd "i C") #'org-clock-out)
  (define-key my/leader-map (kbd "a d") #'org-deadline)
  (define-key my/leader-map (kbd "a s") #'org-schedule)
	(define-key evil-visual-state-map (kbd "a a") #'my/surround-text)
  (define-key my/leader-map (kbd "D") #'org-agenda-deadline)
  (define-key my/leader-map (kbd "S") #'org-agenda-schedule)

  ;; ********** show *********
  (define-key my/leader-map (kbd "v w") #'delete-other-windows) ;; Focus on current window
	(define-key my/leader-map (kbd "v o w") #'other-window)
  (define-key my/leader-map (kbd "v o f") #'other-frame)
  (define-key my/leader-map (kbd "v n b") #'next-buffer)
  (define-key my/leader-map (kbd "v p b") #'previous-buffer)
  (define-key my/leader-map (kbd "v b") #'list-buffers)
  (define-key my/leader-map (kbd "v a") #'org-agenda)
  (define-key my/leader-map (kbd "v n a") #'org-agenda-later)
  (define-key my/leader-map (kbd "v p a") #'org-agenda-earlier)
  (define-key my/leader-map (kbd "v g") #'my-org-agenda-switch-to)
  (define-key my/leader-map (kbd "v c") #'my/org-cycle-at-point)

  ;; ********** change / modify *********
  (define-key my/leader-map (kbd "c T") #'org-agenda-todo)
  (define-key my/leader-map (kbd "c t") #'org-todo)
  (define-key my/leader-map (kbd "c c") #'org-ctrl-c-ctrl-c)
  (define-key my/leader-map (kbd "c k") #'org-kill-note-or-show-branches)
  (define-key my/leader-map (kbd "c l") #'org-edit-special) ;; edit literate block
  (define-key my/leader-map (kbd "c s") #'org-schedule)
  (define-key my/leader-map (kbd "c d") #'org-deadline)

  ;; ********** delete *********
  (define-key my/leader-map (kbd "d w") #'delete-window)
  (define-key my/leader-map (kbd "d f") #'delete-frame)
  (define-key my/leader-map (kbd "d k") #'kill-buffer-and-window)
  (define-key my/leader-map (kbd "d l") #'org-edit-src-abort) ;; discard and exit literate block

  ;; ********** kill *********
  (define-key my/leader-map (kbd "k b") #'kill-buffer)
  (define-key my/leader-map (kbd "k k") #'kill-buffer-and-window)
  (define-key my/leader-map (kbd "k l") #'org-edit-src-exit) ;; kill literate block

  ;; ********** execute *********
  (define-key my/leader-map (kbd "x x") #'counsel-M-x)
  (define-key my/leader-map (kbd "x f") #'counsel-find-file)

  ;; ********** help *********
  (define-key my/leader-map (kbd "h f") #'describe-function)
  (define-key my/leader-map (kbd "h v") #'describe-variable)
  )

(use-package evil-collection
  :after evil
  :ensure t
	:defer t
  :init
  (evil-collection-init))

(use-package evil-org
  :after evil org
  :ensure t
	:defer t
  :hook ((org-mode . evil-org-mode))
  :init
  (unless (fboundp 'evil-redirect-digit-argument)
    (defalias 'evil-redirect-digit-argument 'ignore))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
	(evil-define-key 'motion org-agenda-mode-map (kbd "SPC") my/leader-map))

;;; evil.el ends here
