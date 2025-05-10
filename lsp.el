;;; lsp-support --- LSP support for programming
;;; Commentary:
;;  LSP mode packages for all languages I work with.

;; Global LSP package
(use-package lsp-mode :ensure t :defer t)
;;   :config
;;   (setq gc-cons-threshold (* 100 1024 1024)
;; 	read-process-output-max (* 1024 1024)
;; ;;	treemacs-space-between-root-nodes nil
;; 	company-idle-delay 0.0
;; 	company-minimum-prefix-length 1
;; 	lsp-idle-delay 0.5 ;; clangd is fast
;; 	;; be more ide-ish
;; 	lsp-headerline-breadcrumb-enable t
;; 	lsp-log-io nil
;; 	lsp-print-performance t)

  ;(with-eval-after-load 'lsp-mode
  ;  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  ;  (yas-global-mode)))

;; Pop-up documentation and other UI features for LSP.
(use-package lsp-ui
  :ensure t
	:defer t
  :config
  (setq lsp-ui-doc-max-width 60))

;; Show symbol tree in treemacs under the current file.
;;(use-package lsp-treemacs :ensure t)

;;--------------------------------------
;; Language specific LSP packages
;;--------------------------------------
;; LISP / elisp
;; NOTE: We don't need a LSP for lisp since emacs has all required features built-in.

;;--------------------------------------
;; C/C++
;; NOTE: Make sure you install clangd using
;; macOS - brew install llvm
;; linux - sudo apt-get install clangd-9; sudo update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-9 100
(add-hook 'c-mode-hook 'lsp)
(add-hook 'cpp-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

;;--------------------------------------
;; Shell (Bash)
;; First run "npm i -g bash-language-server"
(add-hook 'sh-mode-hook 'lsp)

;;--------------------------------------
;; CMake
;; First run "pip install cmake-language-server"
;; (use-package cmake-mode
;;   :ensure t
;;   :config
;;   (add-hook 'cmake-mode-hook 'lsp))

;;--------------------------------------
;; Python
;; Make sure python-language-server is installed using latest version of pip.
;; (use-package lsp-python-ms
;;   :ensure t
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-python-ms)
;;                          (lsp))))

;;--------------------------------------
;; Java
;; Make sure you install java from https://www.java.com and
;; maven using "brew install maven". If you are on macOS Big Sur,
;; Homebrew may complain about CLT. In that case, run "sudo rm -rf /Library/Developer/CommandLineTools" and "sudo xcode-select --install". This will reinstall XCode Command Line Tools. You should be able to install maven now.
;; During the first run of Java LSP, emacs will ask you to install EDT Language Server. Wait for a minute for it to download and install before continuing.
;; (use-package lsp-java 
;;   :ensure t
;;   :config
;;   (setq lsp-java-jdt-download-url  "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz")
;;   (add-hook 'java-mode-hook 'lsp))

;;--------------------------------------
;; C#
;; Just run [M-x] lsp-install-server [RET] csharp [RET].
;;(use-package csharp-mode :ensure t :defer t)

;;--------------------------------------
;; Javascript/Typescript
;; Run "brew install npm". NPM is required for installing other JS/TS packages
;; Install LSP server using "npm i -g typescript-language-server; npm i -g typescript"
;; (use-package js2-mode
;;   :ensure t
;;   :config
;;   (add-hook 'js2-mode-hook 'lsp)
;;   (add-hook 'typescript-mode-hook 'lsp))

;;--------------------------------------
;; Angular
;; First run "npm install -g @angular/language-service@next typescript @angular/language-server"
;; (use-package angular-mode
;;   :ensure t
;;   :config
;;   (add-hook 'angular-mode-hook 'lsp)
;;   (add-hook 'angular-html-mode-hook 'lsp))

;;--------------------------------------
;; HTML
;; First run "npm install -g vscode-html-languageserver-bin"
(add-hook 'html-mode-hook 'lsp)

;;--------------------------------------
;; CSS
;; First run "npm install -g vscode-css-languageserver-bin"
(add-hook 'css-mode-hook 'lsp)

;;--------------------------------------
;; JSON
;; First run "npm i -g vscode-json-languageserver"
;; (use-package json-mode
;;   :ensure t
;;   :config
;;   (add-hook 'json-mode-hook 'lsp))

;;--------------------------------------
;; PHP
;; First run "npm i intelephense -g"
;; SUGGESTION: Use "php -S 127.0.0.1:<port>" in the folder containing
;; your PHP/HTML files to run a web-server
;; (use-package php-mode
;;   :ensure t
;;   :config
;;   (add-hook 'php-mode-hook 'lsp))

;;--------------------------------------
;; SQL / T-SQL
;; First install golang using "brew install golang"
;; Install sqls server -"go get github.com/lighttiger2505/sqls"
(setq lsp-sqls-server "~/go/bin/sqls")
(add-hook 'sql-mode-hook 'lsp)

;;--------------------------------------
;; Go
;; Install golang using "brew install golang"
;; Install gopls server - "go get golang.org/x/tools/gopls"
(use-package go-mode
  :ensure t
	:defer t
  :config
  (setq lsp-gopls-server-path "~/go/bin/gopls")
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

;;--------------------------------------
;; Swift
;; (use-package lsp-sourcekit
;;   :ensure t
;;   :config
;;   (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")
;;   (setq lsp-sourcekit-executable "/usr/bin/sourcekit-lsp"))

;; (use-package swift-mode
;;   :ensure t
;;   :config
;;   (add-hook 'swift-mode-hook 'lsp))

;;--------------------------------------
;; YAML / Ansible / Kubernetes
;; Make sure ansible & kubernetes are installed first.
;; First install yaml LSP server - "npm install -g yaml-language-server"
(add-hook 'yaml-mode-hook #'lsp)

;;--------------------------------------
;; XML
;; First run [M-x] lsp-install-server [RET] xmlls [RET]
;; If lsp shows a "corrupt jar" error, run this command - "curl https://repo.eclipse.org/content/repositories/lemminx-releases/org/eclipse/lemminx/org.eclipse.lemminx/0.13.1/org.eclipse.lemminx-0.13.1-uber.jar --output ~/.emacs.d/.cache/lsp/xmlls/"
(add-hook 'nxml-mode-hook 'lsp)

;;--------------------------------------
;; Packer

;;--------------------------------------
;; Terraform
;; First install terraform language server by following
;; the guide at https://github.com/juliosueiras/terraform-lsp
;; NOTE: Clone the repo into ~/go/src/ as part of installation
;; NOTE: Use make copy DST="~/go/bin/" at the end. 
;; (use-package terraform-mode
;;   :ensure t
;;   :config
;;   (setq lsp-terraform-server "~/go/bin/terraform-lsp")
;;   (add-to-list 'lsp-language-id-configuration '(terraform-mode . "terraform"))
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection '("~/go/bin/terraform-lsp" "-enable-log-file"))
;;                     :major-modes '(terraform-mode)
;;                     :server-id 'terraform-lsp))
;;   (add-hook 'terraform-mode-hook #'lsp))

;;--------------------------------------
;; HCL (Hashicorp Custom Language)

;;--------------------------------------
;; Ruby
;; First run "sudo gem install solargraph"
(use-package ruby-mode
  :ensure t
	:defer t
  :config
  (add-hook 'ruby-mode-hook 'lsp))

;;--------------------------------------
;; Docker
;; First run "npm install -g dockerfile-language-server-nodejs"
;; (use-package dockerfile-mode
;;   :ensure t
;;   :config
;;   (add-hook 'dockerfile-mode-hook #'lsp))

;;--------------------------------------
;; Dot (Graphviz, etc.) / PlantUML
;; (use-package graphviz-dot-mode
;;   :ensure t
;;   :config
;;   (setq graphviz-dot-indent-width 2)
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection '("/usr/local/bin/dot-language-server" "--stdio"))
;;                     :major-modes '(graphviz-dot-mode)
;;                     :server-id 'dot-lsp)))
