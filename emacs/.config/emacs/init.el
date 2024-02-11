;; Hello
(setq user-full-name "Rich Smith"
      user-mail-address (concat "rls" "@" "hwyl.org"))

;; Emacs-handled config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
;; Load machine-localised settings if present
(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load local-file)))

;; Load initialisation files
(setq init-dir (file-name-concat user-emacs-directory "init"))
(defun load-init-file (file-name)
  (org-babel-load-file (expand-file-name file-name init-dir)))

(load-init-file "core.org")
(load-init-file "display.org")
(load-init-file "navigation.org")
(load-init-file "code.org")
(load-init-file "misc.org")
(load-init-file "vcs.org")
(load-init-file "org.org")
(when (eq system-type 'darwin)
  (load-init-file "mac.org"))
