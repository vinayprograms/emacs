;;; adm.el --- Custom major mode for ADM files

;;; Commentary:
;;; ADM (Attack Defense Modeling) language is an extension of Gherkin.  adm-mode
;;; highlights speciifc keywords from that language.

;;; Code:

;; Define the custom major mode for ADM
(define-derived-mode adm-mode fundamental-mode "ADM"
  "Major mode for editing Attack Defense Modeling (ADM) files."
  (setq font-lock-defaults
        '(("\\<\\(Model\\|Assumption\\|Policy\\|Attack\\|Defense\\|Given\\|When\\|Then\\|And\\|But\\)\\>"
           . font-lock-keyword-face))))

;; Automatically recognize .adm files for ADM mode
(add-to-list 'auto-mode-alist '("\\.adm\\'" . adm-mode))

;; Add ADM to Org Babel only if Org is loaded, delaying `org-babel` config until Org is in use
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t) (shell . t)))  ;; Adjust languages as needed
  (add-to-list 'org-src-lang-modes '("adm" . adm-mode)))

(provide 'adm)
;;; adm.el ends here

