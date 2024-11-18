;;; org.el --- Optimized Org mode customizations

;;; Commentary:
;; Customizations for Org mode, focusing on task and note management
;; with enhanced usability and custom workflow.

;;; Code:

(use-package org
  :hook ((org-mode . org-bullets-mode)
         (org-mode . org-superstar-mode))
  :init
  ;; Load org-babel languages only when using src blocks
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python . t)
       (C . t)
       (shell . t)
       (emacs-lisp . t)
       (dot . t))))

    ;; Path for dot and LaTeX executables
    (setenv "PATH"
            (concat "/usr/local/Cellar/graphviz/2.42.2/bin/:/Library/TeX/texbin/latex:"
                    (getenv "PATH")))

    (defun org-toggle-strikethrough-for-completed-checkbox ()
      "Apply `org-strikethrough` face to completed checklist items in Org-mode."
      (font-lock-add-keywords
       nil
       '(("^[ \t]*\\([-+*]\\|[0-9]+[.)]\\) \\[X\\] \\(.*\\)$"
	  (2 'org-strikethrough t)))))

    ;; Add the strikethrough function to Org-mode hook
    (add-hook 'org-mode-hook #'org-toggle-strikethrough-for-completed-checkbox)
    
  ;; General Org settings
  :custom
  (org-startup-indented t)
  (org-hide-leading-stars t)
  (org-adapt-indentation nil)
  (org-use-speed-commands t)
  (org-log-done t)
  (org-log-reschedule 'time)
  (org-fontify-done-headline t)
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-ellipsis "…")  ;; Custom ellipsis
  (org-clock-into-drawer "CLOCKING")
  (org-indent-indentation-per-level 2)
  (org-todo-keywords '((sequence "TODO" "PROJ" "RECUR" "STARTED" "WAITING" "DEFERRED" "|" "DONE" "DELEGATED" "CANCELLED")))
  (org-todo-keyword-faces '(("TODO" . "red")
			    ("PROJ" . "violet") ("RECUR"."orange")
			    ("STARTED" . "violet")
                            ("WAITING" . "orange") ("DEFERRED" . "pink1")
                            ("DONE" . "green3") ("DELEGATED" . "cyan2")
                            ("CANCELLED" . "blue")))
  (org-log-into-drawer t)
  (org-edit-src-content-indentation 0)
  (org-log-reschedule 'time)
  (org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cc" . org-capture))
  )

;; Org Agenda settings, loaded after Org
(use-package org-agenda
  :after org
  :custom
  (org-agenda-start-with-clockreport-mode t)
  (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 8 :compact t :stepskip0 t :fileskip0 t))
  (org-agenda-custom-commands
  '(("A" "Agenda and all TODOs"
     ((agenda ""
	      ((org-agenda-span 'day)))
      (alltodo ""
	       ((org-agenda-overriding-header "=*= INBOX =*=")
		(org-agenda-files
		 (list (concat (getenv "ORG_AGENDA_DIR") "/inbox.org" ) ))))
      (todo "STARTED"
	    ((org-agenda-overriding-header "=*= STARTED =*=")))
      (todo "NEXT"
	    ((org-agenda-overriding-header "=*= NEXT ACTIONS =*=")))
      (todo "PROJ"
	    ((org-agenda-overriding-header "=*= PROJECTS =*=")))
      (todo "WAITING"
	    ((org-agenda-overriding-header "=*= WAITING FOR =*=")))
      (alltodo ""
	       ((org-agenda-overriding-header "=*= OPEN TASKS =*=")
		(org-agenda-sorting-strategy
		 '(todo-state-up)))))
     nil)
    ("w" "Tasks I am waiting for"
     ((todo "WAITING" nil))
     nil nil)))
  (org-capture-templates
   `(("g" "Capture a GOAL" entry
      (file ,(concat (getenv "ORG_AGENDA_DIR") "/inbox.org"))
      (file ,(concat (getenv "ORG_AGENDA_DIR") "/todo/templates/goal-capture.txt")))
     ("t" "Add a TASK to inbox" entry
      (file ,(concat (getenv "ORG_AGENDA_DIR") "/inbox.org"))
      (file ,(concat (getenv "ORG_AGENDA_DIR") "/todo/templates/task-capture.txt")))
     ("h" "Capture a THOUGHT. Something that needs further elaboration later." entry
      (file ,(concat (getenv "ORG_AGENDA_DIR") "/inbox.org"))
      (file ,(concat (getenv "ORG_AGENDA_DIR") "/todo/templates/thought-capture.txt")))
     ("r" "Capture a REMINDER" entry
      (file ,(concat (getenv "ORG_AGENDA_DIR") "/inbox.org"))
      (file ,(concat (getenv "ORG_AGENDA_DIR") "/todo/templates/reminder-capture.txt"))
      :time-prompt t)
     ("m" "Schedule a MEETING" entry
      (file ,(concat (getenv "ORG_AGENDA_DIR") "/inbox.org"))
      (file ,(concat (getenv "ORG_AGENDA_DIR") "/todo/templates/meeting-capture.txt")))
     ("p" "Capture a REPEATing task." entry
      (file ,(concat (getenv "ORG_AGENDA_DIR") "/inbox.org"))
      (file ,(concat (getenv "ORG_AGENDA_DIR") "/todo/templates/repeat-capture.txt")))
     ("j" "Capture a PROJECT." entry
      (file ,(concat (getenv "ORG_AGENDA_DIR") "/inbox.org"))
      (file ,(concat (getenv "ORG_AGENDA_DIR") "/todo/templates/project-capture.txt")))
     ))
  (org-refile-targets '((org-agenda-files :maxlevel . 9)))
  (org-agenda-archives-mode t)

  :config
  (defun my-org-agenda-switch-to ()
    "Open agenda item in a new frame if it doesn't exist; switch to it otherwise."
    (interactive)
    (let* ((marker (org-get-at-bol 'org-hd-marker))
           (buffer (and marker (marker-buffer marker)))
           (file (and buffer (buffer-file-name buffer)))
           (other-frame (car (delq (selected-frame) (frame-list)))))
      (when (and file marker)
        (select-frame-set-input-focus (or other-frame (make-frame)))
        (find-file file)
        (goto-char marker))))
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "<tab>") 'my-org-agenda-switch-to))

  (defun my-set-org-agenda-files ()
    "Load all org-mode files from work directory"
    (interactive)
    (let ((org-agenda-dir (getenv "ORG_AGENDA_DIR")))
      (if org-agenda-dir
          (setq org-agenda-files
		(directory-files-recursively org-agenda-dir "\\.org$")))))
  (advice-add 'org-agenda :before 'my-set-org-agenda-files)
)

;; Org recurring tasks
(use-package org-recur
  :hook ((org-mode . org-recur-mode)
         (org-agenda-mode . org-recur-agenda-mode))
  :config
  (setq org-recur-finish-done t
        org-recur-finish-archive t)
  (define-key org-mode-map (kbd "<tab>") 'org-cycle)
  (define-key org-recur-agenda-mode-map (kbd "d") 'org-recur-finish)
  (define-key org-recur-agenda-mode-map (kbd "C-c d") 'org-recur-finish))

;; Show org files as slides
(use-package org-tree-slide
  :bind (("<C-s-left>" . org-tree-slide-move-previous-tree)
         ("<C-s-right>" . org-tree-slide-move-next-tree)))

;; Note-taking extension
(use-package org-noter
  :custom (org-noter-auto-save-last-location t))

;; Bullets and superstars for Org
(use-package org-bullets
  )

(use-package org-superstar
  :config
  (setq org-superstar-item-bullet-alist '((?* . ?•) (?- . ?–) (?+ . ?•))
        org-superstar-todo-bullet-alist '(("[ ]" . 9744) ("[X]" . 9745))
        org-superstar-special-todo-items t))

(provide 'org)

;;; org.el ends here

