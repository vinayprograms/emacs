;; Ivy - Generic completion mechanism for emacs
(use-package ivy-hydra
  :straight t
  :defer t
  :after (ivy hydra)
  :bind*
  (("TAB" . 'ivy-partial)
   ("C-c C-r" . ivy-resume)
   ("C-x b" . ivy-switch-buffer))
  :init
  (ivy-mode t)
  :config
  (setq ivy-use-virtual-buffers t)
  (define-key read-expression-map (kbd "C-r")
    #'counsel-expression-history)
  (ivy-set-actions
   'counsel-find-file
   '(("d" (lambda (x) (delete0file (expand-file-name x)))
      "delete"
      )))
  (ivy-set-actions
   'ivy-switch-buffer
   '(("k"
      (lambda (x)
        (kill-buffer x)
        (ivy--reset-state ivy-last))
      "kill")
     ("j"
      ivy--switch-buffer-other-window-action
      "other window"))))

;; Help with minibuffer (keyboard shortcuts, commands, etc.)
(use-package counsel
  :straight t
  :defer t
  :bind*
  (("M-x" . counsel-M-x)
   ("s-r" . counsel-M-x) ;; Use Command+r as alternative to M-x
   ("C-x C-f" . counsel-find-file)
   ("C-c h f" . counsel-describe-function)
   ("C-c h v" . counsel-describe-variable)
   ("C-c i u" . counsel-unicode-character)
   ("M-i" . counsel-imenu)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-c l" . scounsel-locate)))

;; Show all matches in isearch
(use-package swiper
  :straight t
  :defer t
  :bind ("C-s" . swiper))

;; Add tooltip for commands used with C-x and C-c
(use-package which-key
  :straight t
  :defer t
  :config
  (which-key-mode))
