;; Ensure 'package' is present
(require 'package)

;; Enable strict TLS certificate verification
(setq tls-checktrust t)
(setq gnutls-verify-error t)

;; Set the CA certificates file location directly without Python
;; Typically, this will be in standard locations based on the OS
(setq gnutls-trustfiles
      (cond
       ((file-exists-p "/etc/ssl/certs/ca-certificates.crt")
	; Debian/Ubuntu/Gentoo/etc.
        '("/etc/ssl/certs/ca-certificates.crt"))
       ((file-exists-p "/etc/ssl/ca-bundle.pem")
	; OpenSUSE/SLES
        '("/etc/ssl/ca-bundle.pem"))
       ((file-exists-p "/etc/pki/tls/certs/ca-bundle.crt")
	; Fedora/RHEL
        '("/etc/pki/tls/certs/ca-bundle.crt"))
       ((file-exists-p "/usr/local/etc/openssl/cert.pem")
	; macOS (MacPorts)
        '("/usr/local/etc/openssl/cert.pem"))
       ; macOS with Homebrew (Apple Silicon)
       ((file-exists-p "/opt/homebrew/etc/openssl@3/cert.pem")
        '("/opt/homebrew/etc/openssl@3/cert.pem"))
       ; Default: no CA file
       ;;       (t nil)
       ))
;; Force gnutls to use TLS
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Setup package URLs and archives, omitting those not required
(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
				("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
	))

;; Enable `package-quickstart` if Emacs 27 or later for faster loading
(when (boundp 'package-quickstart)
  (setq package-quickstart t))

;; Only call `package-initialize` for Emacs 26 and earlier
;;(unless (boundp 'package-quickstart)
;;  (package-initialize))

;; Refresh package contents only if needed
(unless (file-exists-p "~/.emacs.d/elpa/archives")
		(package-refresh-contents)
		(package-initialize))
